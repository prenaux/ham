/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * execunix.c - execute a shell script on UNIX/WinNT/OS2/AmigaOS
 *
 * If $(JAMSHELL) is defined, uses that to formulate execvp()/spawnvp().
 * The default is:
 *
 *  /bin/sh -c %    [ on UNIX/AmigaOS ]
 *  cmd.exe /c %    [ on OS2/WinNT ]
 *
 * Each word must be an individual element in a jam variable value.
 *
 * In $(JAMSHELL), % expands to the command string and ! expands to
 * the slot number (starting at 1) for multiprocess (-j) invocations.
 * If $(JAMSHELL) doesn't include a %, it is tacked on as the last
 * argument.
 *
 * Don't just set JAMSHELL to /bin/sh or cmd.exe - it won't work!
 *
 * External routines:
 *  execcmd() - launch an async command execution
 *  execwait() - wait and drive at most one execution completion
 *
 * Internal routines:
 *  onintr() - bump intr to note command interruption
 *
 * 04/08/94 (seiwald) - Coherent/386 support added.
 * 05/04/94 (seiwald) - async multiprocess interface
 * 01/22/95 (seiwald) - $(JAMSHELL) support
 * 06/02/97 (gsar)    - full async multiprocess support for Win32
 * 01/20/00 (seiwald) - Upgraded from K&R to ANSI C
 * 11/04/02 (seiwald) - const-ing for string literals
 * 12/27/02 (seiwald) - grist .bat file with pid for system uniqueness
 */

#include "jam.h"
#include "lists.h"
#include "execcmd.h"
#include <errno.h>

/* # define _TRACE_EXECCMD */

#ifdef USE_EXECUNIX

  #ifdef OS_OS2
    #define USE_EXECNT
    #include <process.h>
  #endif

  #if defined OS_MACOSX || defined OS_LINUX
    #include <spawn.h>
    #include <sys/wait.h>
    #include <unistd.h>
    #define _exit exit
extern char** environ;
  #endif

  #ifdef OS_NT

    #define USE_EXECNT
    #include <process.h>
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h> /* do the ugly deed */
    #define USE_MYWAIT
    #define wait my_wait
static intptr_t my_wait(int* status);
    #define MY_SPAWNVP _spawnvp

  #endif

static int intr = 0;
static int cmdsrunning = 0;
static void (*istat)(int);

static struct {
  intptr_t pid; /* on win32, a real process handle */
  void (*func)(void* closure, int status, const char* output);
  void* closure;
  int outputFileUsed;
  char* outputFile;
  #ifdef USE_EXECNT
  char* tempFile;
  #endif
} cmdtab[MAXJOBS] = {{0}};

void exec_init() {
  char* tempdir;
  int i;

  #ifdef USE_EXECNT
  tempdir = "C:\\";
  #else
  tempdir = "/tmp";
  #endif

  if (getenv("TMPDIR"))
    tempdir = getenv("TMPDIR");
  else if (getenv("TEMP"))
    tempdir = getenv("TEMP");
  else if (getenv("TMP"))
    tempdir = getenv("TMP");

  for (i = 0; i < globs.jobs; ++i) {
  #ifdef USE_EXECNT
    cmdtab[i].tempFile = malloc(strlen(tempdir) + 128);
    sprintf(
      cmdtab[i].tempFile,
      "%s/jam_p%d_tmp%d_r%d.sh",
      tempdir,
      getpid(),
      i,
      rand());
  #endif
    cmdtab[i].outputFileUsed = 0;
    cmdtab[i].outputFile = malloc(strlen(tempdir) + 128);
    sprintf(
      cmdtab[i].outputFile,
      "%s/jam_p%d_out%d_r%d.out",
      tempdir,
      getpid(),
      i,
      rand());
  }
}

void exec_done() {
  int i;
  for (i = 0; i < globs.jobs; ++i) {
  #ifdef USE_EXECNT
    unlink(cmdtab[i].tempFile);
  #endif
    if (cmdtab[i].outputFileUsed)
      unlink(cmdtab[i].outputFile);
  }
}

/*
 * onintr() - bump intr to note command interruption
 */

void onintr(int disp) {
  intr++;
  printf("...interrupted\n");
}

/*
 * execcmd() - launch an async command execution
 */
static int _firstExec = 0;

  #define _MAX_CMD_LINE_ARGS 128

//! Converts a command line to argc/argv
//! \param aszCmdLine is the command line to convert
//! \param apArgvBuffer is the buffer where the command line will be copied, apArgv will point to location
//!        in that buffer. It should be at least the same length as aaszCmdLine.
//! \param apArgv will contain the arguments.
int StrCommandLineToArgcArgv(
  const char* aszCmdLine,
  char* apArgvBuffer,
  const char** apArgv,
  int anArgv0) {
  int argc = 0;

  // Set to no argv elements, in case we have to bail out
  apArgv[anArgv0 + 0] = 0;

  // Copy the system version of the command line into our copy
  strcpy(apArgvBuffer, aszCmdLine);

  if ('"' == *apArgvBuffer)  // If command line starts with a quote ("),
  {                          // it's a quoted filename.  Skip to next quote.
    apArgvBuffer++;

    apArgv[anArgv0 + 0] = apArgvBuffer;  // argv[0] == executable name

    while (*apArgvBuffer && (*apArgvBuffer != '"'))
      apArgvBuffer++;

    if (*apArgvBuffer)      // Did we see a non-NULL ending?
      *apArgvBuffer++ = 0;  // Null terminate and advance to next char
    else
      return 0;  // Oops!  We didn't see the end quote
  }
  else  // A regular (non-quoted) filename
  {
    apArgv[anArgv0 + 0] = apArgvBuffer;  // argv[0] == executable name

    while (*apArgvBuffer && (' ' != *apArgvBuffer) && ('\t' != *apArgvBuffer))
      apArgvBuffer++;

    if (*apArgvBuffer)
      *apArgvBuffer++ = 0;  // Null terminate and advance to next char
  }

  // Done processing argv[0] (i.e., the executable name).  Now do th
  // actual arguments

  argc = 1;

  while (1) {
    // Skip over any whitespace
    while (*apArgvBuffer && (' ' == *apArgvBuffer) || ('\t' == *apArgvBuffer))
      apArgvBuffer++;

    if (0 == *apArgvBuffer)  // End of command line???
      return argc;

    if ('"' == *apArgvBuffer)  // Argument starting with a quote???
    {
      apArgvBuffer++;  // Advance past quote character

      apArgv[anArgv0 + (argc++)] = apArgvBuffer;
      apArgv[anArgv0 + argc] = 0;

      // Scan to end quote, or NULL terminator
      while (*apArgvBuffer && (*apArgvBuffer != '"'))
        apArgvBuffer++;

      if (0 == *apArgvBuffer)
        return argc;

      if (*apArgvBuffer)
        *apArgvBuffer++ = 0;  // Null terminate and advance to next char
    }
    else  // Non-quoted argument
    {
      apArgv[anArgv0 + (argc++)] = apArgvBuffer;
      apArgv[anArgv0 + argc] = 0;

      // Skip till whitespace or NULL terminator
      while (*apArgvBuffer && (' ' != *apArgvBuffer) && ('\t' != *apArgvBuffer))
        apArgvBuffer++;

      if (0 == *apArgvBuffer)
        return argc;

      if (*apArgvBuffer)
        *apArgvBuffer++ = 0;  // Null terminate and advance to next char
    }

    if (argc >= (_MAX_CMD_LINE_ARGS))
      return argc;
  }
}

void execcmd(
  const char* string,
  void (*func)(void* closure, int status, const char* output),
  void* closure,
  const char* aShellPath,
  int serialOutput) {
  int pid;
  int slot;
  char* buffer = NULL;
  const char* argv[MAXARGC + 1]; /* +1 for NULL */
  int stringLen = 0;

  /* # ifdef USE_EXECNT */
  /*  char *p; */
  /* # endif */

  #ifdef USE_EXECNT
  if (globs.jobs > 1) {
    Sleep(
      10);  // try to avoid have two process starting exactly at the same moment
    // which causes trouble for some programs
  }
  #endif

  if (!_firstExec) {
    srand(clock());
    _firstExec = 1;
  }

  /* Trim leading whitspaces */
  while (isspace(*string))
    ++string;

  stringLen = strlen(string);

  #ifdef _TRACE_EXECCMD
  /* printf("EXECCMD(%d)\n",strlen(string)); */
  printf("EXECCMD(%lu): %s\n", strlen(string), string);
  #endif

  /* Find a slot in the running commands table for this one. */
  for (slot = 0; slot < MAXJOBS; ++slot)
    if (!cmdtab[slot].pid)
      break;

  if (slot == MAXJOBS) {
    printf("no slots for child!\n");
    exit(EXITBAD);
  }

  /* Forumulate argv */
  if (aShellPath) {
    int i = 0;
    if (*aShellPath) {
      argv[i++] = aShellPath;
    }
    buffer = malloc(stringLen + 1);
    StrCommandLineToArgcArgv(string, buffer, argv, i);
  }
  else {
    // Use Bash, the default shell
    int i = 0;
  #ifdef USE_EXECNT
    extern char g_bash_path[_MAX_PATH];
    if (
      strstr(g_bash_path, "cygwin") == NULL &&
      strstr(g_bash_path, "CYGWIN") == NULL) {
      /* write to a temporary bash script */
      {
        int maxTry = 10;
        while (maxTry--) {
          FILE* f;
          /* Write command to script file. */
          f = fopen(cmdtab[slot].tempFile, "w");
          if (!f) {
            Sleep(100);
            continue;
          }
          fputs(string, f);
          fclose(f);
    #ifdef _TRACE_EXECCMD
          printf("TEMP(%s) : %s\n", string, cmdtab[slot].tempFile);
    #endif
          string = cmdtab[slot].tempFile;
          break;
        }
        if (maxTry == 0) {
          printf("can't write command script !\n");
          exit(EXITBAD);
        }
      }
      argv[i++] = g_bash_path;
      argv[i++] = string;
    }
    else {
      argv[i++] = g_bash_path;
      argv[i++] = "-c \"";
      argv[i++] = string;
      argv[i++] = "\"";
    #ifdef _TRACE_EXECCMD
      printf("CYGBASH: %s %s %s\n", argv[0], argv[1], argv[2]);
    #endif
    }
  #else
    {
      argv[i++] = "/bin/bash";
      argv[i++] = "-c";
      argv[i++] = string;
    }
  #endif
    argv[i++] = 0;
  }

  /* Catch interrupts whenever commands are running. */
  if (!cmdsrunning++)
    istat = signal(SIGINT, onintr);

    /* Start the command */

  #ifdef USE_EXECNT
      /****** WinNT *********/

    #ifdef _TRACE_EXECCMD
  printf("EXECNT-SPAWN (%s) (PATH:%s)\n", argv[0], getenv("PATH"));
    #endif

  if (serialOutput) {
    int out, err, fd, bad_spawn = 0, spawn_err = -1;

    out = _dup(1);
    err = _dup(2);
    cmdtab[slot].outputFileUsed = 1;
    fd = open(cmdtab[slot].outputFile, O_WRONLY | O_TRUNC | O_CREAT, 0644);
    _dup2(fd, 1);
    _dup2(fd, 2);
    close(fd);

    if ((pid = MY_SPAWNVP(P_NOWAIT, argv[0], argv)) == -1) {
      bad_spawn = 1;
      spawn_err = errno;
    }

    _dup2(out, 1);
    _dup2(err, 2);
    close(out);
    close(err);

    if (bad_spawn) {
      errno = spawn_err;
      printf("Ham: Error invoking spawn() for %s\n", argv[0]);
      perror("spawn");
      exit(EXITBAD);
    }
  }
  else {
    if ((pid = MY_SPAWNVP(P_NOWAIT, argv[0], argv)) == -1) {
      int i = 0;
      char tmp[8192] = {'\0'};
      /*    printf("EXEC-SPAWN-ERRL %d (%s)\n",pid); */
      strcat(tmp, "spawn-unix(");
      while (argv[i] && i < 10) {
        strcat(tmp, argv[i]);
        strcat(tmp, ",");
        ++i;
      }
      if (argv[i]) {
        strcat(tmp, "...");
      }
      strcat(tmp, ")");
      strcat(tmp, "[");
      strcat(tmp, getenv("PATH"));
      strcat(tmp, "]");
      perror(tmp);
      /*      perror( "spawn" ); */
      exit(EXITBAD);
    }
  }

  #else

    #ifdef _TRACE_EXECCMD
  printf("EXEC-POSIX-SPAWN (serialOutput: %d): %s\n", serialOutput, argv[0]);
    #endif

  /****** POSIX *********/
  posix_spawn_file_actions_t child_fd_actions;
  if (serialOutput) {
    int ret;

    #ifdef _TRACE_EXECCMD
    printf("serialOutput: %s\n", cmdtab[slot].outputFile);
    fflush(stdout);
    #endif

    cmdtab[slot].outputFileUsed = 1;
    if (ret = posix_spawn_file_actions_init(&child_fd_actions)) {
      perror("posix_spawn_file_actions_init");
      exit(EXITBAD);
    }
    if (
      ret = posix_spawn_file_actions_addopen(
        &child_fd_actions,
        1,
        cmdtab[slot].outputFile,
        O_WRONLY | O_CREAT | O_TRUNC,
        0644)) {
      perror("posix_spawn_file_actions_addopen");
      exit(EXITBAD);
    }
    if (ret = posix_spawn_file_actions_adddup2(&child_fd_actions, 1, 2)) {
      perror("posix_spawn_file_actions_adddup2");
      exit(EXITBAD);
    }
  }

  // Note: posix_spawn must be used with zigcc. vfork + exec doesn't work
  // correctly.
  int rspawn = posix_spawn(
    &pid,
    argv[0],
    serialOutput ? &child_fd_actions : NULL,
    NULL,
    (char**)argv,
    environ);
  if (rspawn == 0) {
    #ifdef _TRACE_EXECCMD
    printf("EXEC-POSIX-SPAWN pid: %d\n", pid);
    #endif
  }
  else {
    printf("spawn error: %s\n", strerror(rspawn));
    perror("posix_spawn");
    exit(EXITBAD);
  }
  #endif

  /* Save the operation for execwait() to find. */
  cmdtab[slot].pid = pid;
  cmdtab[slot].func = func;
  cmdtab[slot].closure = closure;

  /* Wait until we're under the limit of concurrent commands. */
  /* Don't trust globs.jobs alone. */

  while (cmdsrunning >= MAXJOBS || cmdsrunning >= globs.jobs)
    if (!execwait())
      break;

  if (buffer)
    free(buffer);
}

/*
 * execwait() - wait and drive at most one execution completion
 */

int execwait() {
  int i;
  int status, w;
  int rstat;

  /* Handle naive make1() which doesn't know if cmds are running. */

  if (!cmdsrunning)
    return 0;

  /* Pick up process pid and status */

  while ((w = wait(&status)) == -1 && errno == EINTR)
    ;

  if (w == -1) {
    printf("child process(es) lost!\n");
    perror("wait");
    exit(EXITBAD);
  }

  /* Find the process in the cmdtab. */

  for (i = 0; i < MAXJOBS; i++)
    if (w == cmdtab[i].pid)
      break;

  if (i == MAXJOBS) {
    printf("waif child found!\n");
    exit(EXITBAD);
  }

  /* Drive the completion */
  if (!--cmdsrunning)
    signal(SIGINT, istat);

  if (intr) {
    rstat = EXEC_CMD_INTR;
  }
  else if (w == -1 || status != 0) {
    rstat = EXEC_CMD_FAIL;
  }
  else {
    rstat = EXEC_CMD_OK;
  }

  cmdtab[i].pid = 0;

  (*cmdtab[i].func)(
    cmdtab[i].closure,
    rstat,
    cmdtab[i].outputFileUsed ? cmdtab[i].outputFile : 0);

  return 1;
}

  #ifdef USE_MYWAIT

static intptr_t my_wait(int* status) {
  int i, num_active = 0;
  DWORD exitcode, waitcode;
  static HANDLE* active_handles = 0;

  if (!active_handles)
    active_handles = (HANDLE*)malloc(globs.jobs * sizeof(HANDLE));

  /* first see if any non-waited-for processes are dead,
   * and return if so.
   */
  for (i = 0; i < globs.jobs; i++) {
    if (cmdtab[i].pid) {
      if (GetExitCodeProcess((HANDLE)cmdtab[i].pid, &exitcode)) {
        if (exitcode == STILL_ACTIVE)
          active_handles[num_active++] = (HANDLE)cmdtab[i].pid;
        else {
          CloseHandle((HANDLE)cmdtab[i].pid);
          *status = (int)((exitcode & 0xff) << 8);
          return cmdtab[i].pid;
        }
      }
      else
        goto FAILED;
    }
  }

  /* if a child exists, wait for it to die */
  if (!num_active) {
    errno = ECHILD;
    return -1;
  }
  waitcode =
    WaitForMultipleObjects(num_active, active_handles, FALSE, INFINITE);
  if (waitcode != WAIT_FAILED) {
    if (
      waitcode >= WAIT_ABANDONED_0 && waitcode < WAIT_ABANDONED_0 + num_active)
      i = waitcode - WAIT_ABANDONED_0;
    else
      i = waitcode - WAIT_OBJECT_0;
    if (GetExitCodeProcess(active_handles[i], &exitcode)) {
      CloseHandle(active_handles[i]);
      *status = (int)((exitcode & 0xff) << 8);
      return (intptr_t)active_handles[i];
    }
  }

FAILED:
  errno = GetLastError();
  return -1;
}

  #endif /* USE_MYWAIT */

#endif /* USE_EXECUNIX */

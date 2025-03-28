/*
 * /+\
 * +\   Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 * \+/
 *
 * This file is part of jam.
 *
 * License is hereby granted to use this software and distribute it
 * freely, as long as this copyright notice is retained and modifications
 * are clearly marked.
 *
 * ALL WARRANTIES ARE HEREBY DISCLAIMED.
 */

/*
 * jam.c - make redux
 *
 * See Jam.html for usage information.
 *
 * These comments document the code.
 *
 * The top half of the code is structured such:
 *
 *                       jam
 *                      / | \
 *                 +---+  |  \
 *                /       |   \
 *         jamgram     option  \
 *        /  |   \              \
 *       /   |    \              \
 *      /    |     \             |
 *  scan     |     compile      make
 *   |       |    /  | \       / |  \
 *   |       |   /   |  \     /  |   \
 *   |       |  /    |   \   /   |    \
 * jambase parse     |   rules  search make1
 *                   |           |      |   \
 *                   |           |      |    \
 *                   |           |      |     \
 *               builtins    timestamp command execute
 *                               |
 *                               |
 *                               |
 *                             filesys
 *
 *
 * The support routines are called by all of the above, but themselves
 * are layered thus:
 *
 *                     variable|expand
 *                      /  |   |   |
 *                     /   |   |   |
 *                    /    |   |   |
 *                 lists   |   |   pathsys
 *                    \    |   |
 *                     \   |   |
 *                      \  |   |
 *                     newstr  |
 *                        \    |
 *                         \   |
 *                          \  |
 *                          hash
 *
 * Roughly, the modules are:
 *
 *      builtins.c - jam's built-in rules
 *      command.c - maintain lists of commands
 *      compile.c - compile parsed jam statements
 *      execunix.c - execute a shell script on UNIX
 *      execvms.c - execute a shell script, ala VMS
 *      expand.c - expand a buffer, given variable values
 *      file*.c - scan directories and archives on *
 *      hash.c - simple in-memory hashing routines
 *      headers.c - handle #includes in source files
 *      jambase.c - compilable copy of Jambase
 *      jamgram.y - jam grammar
 *      lists.c - maintain lists of strings
 *      make.c - bring a target up to date, once rules are in place
 *      make1.c - execute command to bring targets up to date
 *      newstr.c - string manipulation routines
 *      option.c - command line option processing
 *      parse.c - make and destroy parse trees as driven by the parser
 *      path*.c - manipulate file names on *
 *      hash.c - simple in-memory hashing routines
 *      regexp.c - Henry Spencer's regexp
 *      rules.c - access to RULEs, TARGETs, and ACTIONs
 *      scan.c - the jam yacc scanner
 *      search.c - find a target along $(SEARCH) or $(LOCATE)
 *      timestamp.c - get the timestamp of a file or archive member
 *      variable.c - handle jam multi-element variables
 *
 * 05/04/94 (seiwald) - async multiprocess (-j) support
 * 02/08/95 (seiwald) - -n implies -d2.
 * 02/22/95 (seiwald) - -v for version info.
 * 09/11/00 (seiwald) - PATCHLEVEL folded into VERSION.
 * 01/10/01 (seiwald) - pathsys.h split from filesys.h
 * 01/21/02 (seiwald) - new -q to quit quickly on build failure
 * 03/16/02 (seiwald) - support for -g (reorder builds by source time)
 * 09/19/02 (seiwald) - new -d displays
 * 10/22/02 (seiwald) - list_new() now does its own newstr()/copystr()
 * 11/04/02 (seiwald) - const-ing for string literals
 */

#include "jam.h"
#include "option.h"
#include "patchlevel.h"

/* These get various function declarations. */
#include "lists.h"
#include "parse.h"
#include "variable.h"
#include "compile.h"
#include "builtins.h"
#include "rules.h"
#include "newstr.h"
#include "scan.h"
#include "timestamp.h"
#include "make.h"

/* Macintosh is "special" */
#ifdef OS_MAC
  #include <QuickDraw.h>
#endif

/* And UNIX for this */
#ifdef unix
  #include <sys/utsname.h>
#endif

/* And NT for that */
#ifdef OS_NT
  #include <windows.h>
#endif

/* And Linux for those */
#ifdef OS_LINUX
  #include <errno.h>
  #include <unistd.h>
  #include <sys/wait.h>
#endif

/* And OSX for these */
#ifdef OS_MACOSX
  #include <errno.h>
  #include <unistd.h>
  #include <sys/param.h>
  #include <sys/sysctl.h>
  #include <sys/wait.h>
#endif

struct globs globs = {
  .noexec = 0,
  .jobs = 1,
  .quitquick = 1,
  .newestfirst = 0,
  .debug = {},
  .cmdout = 0,
  .numpass = 1,
};

/* Symbols to be defined as true for use in Jambase */

static const char* othersyms[] = { OSMAJOR, OSMINOR, OSPLAT, HAMVERSYM, 0 };

/* Known for sure:
 *      mac needs arg_enviro
 *      OS2 needs extern environ
 */

#ifdef OS_MAC
  #define use_environ arg_environ
  #ifdef MPW
QDGlobals qd;
  #endif
#endif

#ifndef use_environ
  #define use_environ environ
  #if !defined(__WATCOM__) && !defined(OS_OS2) && !defined(OS_NT)
extern char** environ;
  #endif
#endif

char g_bash_path[2048] = "bash";

static int run_ham_pass(int argc, char** argv, int numpassleft)
{
  char passleftbuf[64] = { 0 };
  snprintf(passleftbuf, sizeof(passleftbuf), "%d", numpassleft);

  char** new_argv = malloc((argc + 3) * sizeof(char*));
  int i;
  new_argv[0] = argv[0];
  new_argv[1] = "-p";
  new_argv[2] = passleftbuf;
  for (i = 1; i < argc; i++) {
    new_argv[i + 2] = argv[i];
  }
  new_argv[argc + 2] = NULL;

#ifdef OS_NT
  STARTUPINFO si = { sizeof(si) };
  PROCESS_INFORMATION pi;

  // Build command line string
  char cmdline[4096] = "";
  for (i = 0; new_argv[i]; i++) {
    if (i > 0)
      strcat(cmdline, " ");
    strcat_s(cmdline, sizeof(cmdline), new_argv[i]);
  }

  if (!CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi))
  {
    printf("error: CreateProcess failed: %lu\n", GetLastError());
    return EXITBAD;
  }

  // Wait for process to complete
  WaitForSingleObject(pi.hProcess, INFINITE);

  DWORD exit_code;
  GetExitCodeProcess(pi.hProcess, &exit_code);

  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  return exit_code;
#else
  pid_t child = fork();
  if (child == -1) {
    printf("error: fork failed: %s.\n", strerror(errno));
    return EXITBAD;
  }

  if (child == 0) {
    // Child process - exec the new pass
    execvp(argv[0], new_argv);
    printf("error: failed to launch pass (numpassleft: %d).\n", numpassleft);
    exit(EXITBAD);
  }

  // Parent process - wait for child to complete
  int child_status;
  waitpid(child, &child_status, 0);
  return WEXITSTATUS(child_status);
#endif
}

typedef struct {
  unsigned int numProcessors;
  unsigned int memoryKB;
  unsigned int diskTotalGB;
  unsigned int diskFreeGB;
} sHostInfo;

void detect_host_info(sHostInfo* info)
{
  // Initialize with defaults
  info->numProcessors = 1;
  info->memoryKB = 0;
  info->diskTotalGB = 0;
  info->diskFreeGB = 0;

  // Get processor count
#ifdef OS_NT
  {
    SYSTEM_INFO sysInfo;
    ZeroMemory(&sysInfo, sizeof(SYSTEM_INFO));
    GetSystemInfo(&sysInfo);
    info->numProcessors = sysInfo.dwNumberOfProcessors;
  }
#endif
#ifdef OS_LINUX
  {
    info->numProcessors = sysconf(_SC_NPROCESSORS_ONLN);
  }
#endif
#ifdef OS_MACOSX
  {
    int mib[4];
    size_t len = sizeof(info->numProcessors);
    mib[0] = CTL_HW;
    mib[1] = HW_AVAILCPU;
    sysctl(mib, 2, &info->numProcessors, &len, NULL, 0);
    if (info->numProcessors < 1) {
      mib[1] = HW_NCPU;
      sysctl(mib, 2, &info->numProcessors, &len, NULL, 0);
      if (info->numProcessors < 1) {
        info->numProcessors = 1;
      }
    }
  }
#endif

// Get memory size
#ifdef OS_NT
  {
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    if (GlobalMemoryStatusEx(&memInfo)) {
      info->memoryKB = (unsigned int)(memInfo.ullTotalPhys / (1024 * 1024));
    }
  }
#endif
#ifdef OS_LINUX
  {
    struct sysinfo sysInfo;
    if (sysinfo(&sysInfo) == 0) {
      info->memoryKB =
        (unsigned int)((sysInfo.totalram * sysInfo.mem_unit) / (1024 * 1024));
    }
  }
#endif
#ifdef OS_MACOSX
  {
    int mib[2] = { CTL_HW, HW_MEMSIZE };
    uint64_t memsize;
    size_t len = sizeof(memsize);
    if (sysctl(mib, 2, &memsize, &len, NULL, 0) == 0) {
      info->memoryKB = (unsigned int)(memsize / (1024 * 1024));
    }
  }
#endif

// Get disk space
#ifdef OS_NT
  {
    char currentDir[MAX_PATH];
    GetCurrentDirectory(MAX_PATH, currentDir);
    char drive[4] = { currentDir[0], ':', '\\', '\0' };

    ULARGE_INTEGER freeBytesAvailable, totalBytes, totalFreeBytes;
    if (GetDiskFreeSpaceEx(drive, &freeBytesAvailable, &totalBytes,
                           &totalFreeBytes))
    {
      info->diskFreeGB =
        (unsigned int)(freeBytesAvailable.QuadPart / (1024 * 1024 * 1024));
      info->diskTotalGB =
        (unsigned int)(totalBytes.QuadPart / (1024 * 1024 * 1024));
    }
  }
#endif
#if defined(OS_LINUX) || defined(OS_MACOSX)
  {
    struct statvfs stat;
    if (statvfs(".", &stat) == 0) {
      info->diskFreeGB = (unsigned int)((uint64_t)stat.f_bsize * stat.f_bavail /
                                        (1024 * 1024 * 1024));
      info->diskTotalGB = (unsigned int)((uint64_t)stat.f_bsize *
                                         stat.f_blocks / (1024 * 1024 * 1024));
    }
  }
#endif
}

int main(int argc, char** argv, char** arg_environ)
{
  int n, num_targets;
  const char* s;
  struct option optv[N_OPTS];
  char* targets[N_TARGETS];
  const char* all = "all";
  int anyhow = 0;
  int status = 0;

  sHostInfo hostInfo;
  detect_host_info(&hostInfo);
  unsigned int numProcessors = hostInfo.numProcessors;

  num_targets =
    getoptions(argc - 1, argv + 1, "d:j:f:gs:t:ano:qvp:", optv, targets);
  if (num_targets < 0) {
    printf("\nusage: ham [ options ] targets...\n\n");

    printf("-a      Build all targets, even if they are current.\n");
    printf("-dx     Display (a)quiet actions (c)causes (d)dependencies\n");
    printf(
      "        (m)make tree (x)commands (g)generated (0-9) debug levels.\n");
    printf("-fx     Read x instead of Hambase.\n");
    printf("-g      Build from newest sources first.\n");
    printf("-jx     Run up to x shell commands concurrently.\n");
    printf("-n      Don't actually execute the updating actions.\n");
    printf("-ox     Write the updating actions to file x.\n");
    printf("-px     Passes that can run after targets have been generated.\n");
    printf("-q      Try to continue building even if a target fails.\n");
    printf("-sx=y   Set variable x=y, overriding environment.\n");
    printf("-tx     Rebuild x, even if it is up-to-date.\n");
    printf("-v      Print the version of ham and exit.\n");

    exit(EXITBAD);
  }

  /* Check that bash is available */
  {
    char* hamShell = getenv("HAMSHELL");
    if (hamShell && *hamShell) {
      strcpy(g_bash_path, hamShell);
    }
#ifdef OS_NT
    else {
      char* devEnvDir = getenv("AGLDEVENV");
      if (devEnvDir) {
        // look for bash_ham
        strcpy(g_bash_path, devEnvDir);
        strcat(g_bash_path, "\\bin\\ham_bash.exe");
        {
          int fp = _open(g_bash_path, _O_RDONLY);
          if (fp == -1) {
            g_bash_path[0] = 0;
          }
          else {
            _close(fp);
          }
        }

        // look for bash
        if (!g_bash_path[0]) {
          strcpy(g_bash_path, devEnvDir);
          strcat(g_bash_path, "\\msys\\bin\\bash.exe");
          {
            int fp = _open(g_bash_path, _O_RDONLY);
            if (fp == -1) {
              g_bash_path[0] = 0;
            }
            else {
              _close(fp);
            }
          }
        }
      }
    }
    // convert '/' to '\\'
    {
      char* p = g_bash_path;
      while (*p) {
        if (*p == '/')
          *p = '\\';
        ++p;
      }
    }
#endif
    if (g_bash_path[0] == 0) {
      printf("-- Can't find the bash executable --\n");
      printf(
        "Make sure its in the path or set the environment variable HAMSHELL to point to it.\n");
      exit(EXITBAD);
    }
  }

  /* Version info. */

  if ((s = getoptval(optv, 'v', 0))) {
    printf("Ham v%s, %s, CPU=%d, MEM=%dKB, DISKFREE=%dGB, DISKSZ=%dGB.\n",
           VERSION, OSMINOR, numProcessors, hostInfo.memoryKB,
           hostInfo.diskFreeGB, hostInfo.diskTotalGB);
    return EXITOK;
  }

  /* Pick up interesting options */

  if ((s = getoptval(optv, 'n', 0)))
    globs.noexec++;

  if ((s = getoptval(optv, 'q', 0)))
    globs.quitquick = 0;
  else
    globs.quitquick = 1;

  if ((s = getoptval(optv, 'a', 0)))
    anyhow++;

  if ((s = getoptval(optv, 'j', 0))) {
    globs.jobs = atoi(s);
    if (globs.jobs < 1)
      globs.jobs = numProcessors;
  }
  else {
    globs.jobs = numProcessors;
  }

  if ((s = getoptval(optv, 'p', 0))) {
    globs.numpass = atoi(s);
  }

  // make sure the number of jobs are in reasonable limits...
  if (globs.jobs < 1 || globs.jobs > (numProcessors * 8))
    globs.jobs = numProcessors;

  if ((s = getoptval(optv, 'g', 0)))
    globs.newestfirst = 1;

  /* Turn on/off debugging */
  DEBUG_MAKE = 1; // Show actions when running
  for (n = 0; (s = getoptval(optv, 'd', n)) != 0; n++) {
    int i = atoi(s);

    /* n turns on levels 1-n */
    /* +n turns on level n */
    /* c turns on named display c */

    if (i < 0 || i >= DEBUG_MAX) {
      printf("Invalid debug level '%s'.\n", s);
    }
    else if (*s == '+') {
      globs.debug[i] = 1;
    }
    else if (i)
      while (i) {
        globs.debug[i--] = 1;
      }
    else {
      while (*s) {
        switch (*s++) {
        case 'a': DEBUG_MAKEQ = 1; break;
        case 'c': DEBUG_CAUSES = 1; break;
        case 'd': DEBUG_DEPENDS = 1; break;
        case 'm': DEBUG_MAKEPROG = 1; break;
        case 'x': DEBUG_EXEC = 1; break;
        case 'g': DEBUG_GENERATED = 1; break;
        case '0': break;
        default: {
          printf("Invalid debug flag '%c'.\n", s[-1]);
          break;
        }
        }
      }
    }
  }

  /* Set HAMCMDARGS */
  {
    LIST* l = L0;
    for (n = 0; n < num_targets; n++) {
      l = list_new(l, targets[n], 0);
    }
    var_set("HAMCMDARGS", l, VAR_SET);
  }

  {
    char buf[64] = { 0 };

    /* Set HAMCMDNUMPASS */
    snprintf(buf, sizeof(buf), "%d", globs.numpass);
    var_set("HAMCMDNUMPASS", list_new(L0, buf, 0), VAR_SET);

    /* Set HAMCMDJOBS */
    snprintf(buf, sizeof(buf), "%d", globs.jobs);
    var_set("HAMCMDJOBS", list_new(L0, buf, 0), VAR_SET);
  }

  /*
   * Ham defined variables OS, OSPLAT
   */
  var_defines(othersyms);

  /* load up environment variables */
  var_defines((const char**)use_environ);

  /* Load up variables set on command line. */
  for (n = 0; (s = getoptval(optv, 's', n)) != 0; n++) {
    const char* symv[2];
    symv[0] = s;
    symv[1] = 0;
    var_defines(symv);
  }

  /* Initialize built-in rules */
  load_builtins();

  /* Parse ruleset */
  for (n = 0; (s = getoptval(optv, 'f', n)) != 0; n++)
    parse_file(s);

  if (!n)
    parse_file("+");

  if (yyanyerrors()) {
    printf("ham: Parsing errors.\n");
    exit(EXITBAD);
  }

  /* Manually touch -t targets */
  for (n = 0; (s = getoptval(optv, 't', n)) != 0; n++)
    touchtarget(s);

  /* If an output file is specified, set globs.cmdout to that */
  if ((s = getoptval(optv, 'o', 0)) != 0) {
    if (!(globs.cmdout = fopen(s, "w"))) {
      printf("Failed to write to '%s'\n", s);
      exit(EXITBAD);
    }
    globs.noexec++;
  }

  /* Now make target */
  int generated = 0;
  if (!num_targets) {
    status |= make(1, &all, anyhow, &generated);
  }
  else {
    status |= make(num_targets, (const char**)targets, anyhow, &generated);
  }

  /* Widely scattered cleanup */
  var_done();
  donerules();
  donestamps();
  donestr();

  /* close cmdout */
  if (globs.cmdout)
    fclose(globs.cmdout);

  {
    fputs("...targets", stdout);
    if (!num_targets) {
      fputs(" all", stdout);
    }
    else {
      int i;
      for (i = 0; i < num_targets; ++i) {
        fputs(" ", stdout);
        fputs(targets[i], stdout);
      }
    }
    fputs("...\n", stdout);
    fflush(stdout);
  }

  // Run the next pass
  if ((status == 0) && (generated > 0)) {
    if (globs.numpass > 0) {
      int passleft = globs.numpass - 1;
      printf(
        "...running next pass after %d target(s) were generated, %d pass left afterwards...\n",
        generated, passleft);
      fflush(stdout);
      return run_ham_pass(argc, argv, passleft);
    }
    else {
      printf("warning: exhausted all passes but %d target(s) were generated.\n",
             generated);
      fflush(stdout);
    }
  }

  return status ? EXITBAD : EXITOK;
}

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
 *	/bin/sh -c %		[ on UNIX/AmigaOS ]
 *	cmd.exe /c %		[ on OS2/WinNT ]
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
 *	execcmd() - launch an async command execution
 * 	execwait() - wait and drive at most one execution completion
 *
 * Internal routines:
 *	onintr() - bump intr to note command interruption
 *
 * 04/08/94 (seiwald) - Coherent/386 support added.
 * 05/04/94 (seiwald) - async multiprocess interface
 * 01/22/95 (seiwald) - $(JAMSHELL) support
 * 06/02/97 (gsar)    - full async multiprocess support for Win32
 * 01/20/00 (seiwald) - Upgraded from K&R to ANSI C
 * 11/04/02 (seiwald) - const-ing for string literals
 * 12/27/02 (seiwald) - grist .bat file with pid for system uniqueness
 */

# include "jam.h"
# include "lists.h"
# include "execcmd.h"
# include <errno.h>

/* # define _TRACE_EXECCMD */

# ifdef USE_EXECUNIX

# ifdef OS_OS2
# define USE_EXECNT
# include <process.h>
# endif

# ifdef OS_NT

# define USE_EXECNT
# include <process.h>
# define WIN32_LEAN_AND_MEAN
# include <windows.h>		/* do the ugly deed */
# define USE_MYWAIT
# if !defined( __BORLANDC__ )
# define wait my_wait
static int my_wait( int *status );
# endif

intptr_t __cdecl _my_spawnvp (
    int modeflag,
    const char *filename,
    const char * const *argv
    );
#define MY_SPAWNVP _my_spawnvp
/* #define MY_SPAWNVP spawnvp */

# endif

static int intr = 0;
static int cmdsrunning = 0;
static void (*istat)( int );

static struct
{
	int	pid; /* on win32, a real process handle */
	void	(*func)( void *closure, int status, const char* output );
	void*   closure;
    int     outputFileUsed;
    char*   outputFile;
#ifdef USE_EXECNT
	char*	tempFile;
#endif
} cmdtab[ MAXJOBS ] = {{0}};

void exec_init()
{
	char 	*tempdir;
	int		i;

# ifdef USE_EXECNT
	tempdir = "C:\\";
# else
	tempdir = "/tmp";
# endif

	if( getenv( "TMPDIR" ) )
		tempdir = getenv( "TMPDIR" );
	else if( getenv( "TEMP" ) )
		tempdir = getenv( "TEMP" );
	else if( getenv( "TMP" ) )
		tempdir = getenv( "TMP" );

	for (i = 0; i < globs.jobs; ++i)
	{
#ifdef USE_EXECNT
		cmdtab[i].tempFile = malloc(strlen(tempdir) + 128);
        sprintf(cmdtab[i].tempFile, "%s/jam_p%d_tmp%d_r%d.sh",
                tempdir, getpid(), i, rand());
#endif
        cmdtab[i].outputFileUsed = 0;
		cmdtab[i].outputFile = malloc(strlen(tempdir) + 128);
        sprintf(cmdtab[i].outputFile, "%s/jam_p%d_out%d_r%d.out",
                tempdir, getpid(), i, rand());
	}
}

void exec_done()
{
    /* int i; */
    /* for (i = 0; i < globs.jobs; ++i) { */
    /* #ifdef USE_EXECNT */
    /* unlink(cmdtab[i].tempFile); */
    /* #endif */
    /* if (cmdtab[i].outputFileUsed) */
    /* unlink(cmdtab[i].outputFile); */
    /* } */
}

/*
 * onintr() - bump intr to note command interruption
 */

void
onintr( int disp )
{
	intr++;
	printf( "...interrupted\n" );
}

static int issplit(char c) {
	if (isspace(c) || c == '\n' || c == '\r')
		return 1;
	return 0;
}

static char* get_fullpathname(char* prog) {
#ifdef USE_EXECNT
    static char buffer[_MAX_PATH];
    char* bufferFile;
    if (GetFullPathName(prog,_MAX_PATH,buffer,&bufferFile)) {
        return buffer;
    }
    return prog;
#else
    return prog;
#endif
}


/*
 * execcmd() - launch an async command execution
 */
static int _firstExec = 0;

#define _MAX_CMD_LINE_ARGS  128

//! Converts a command line to argc/argv
//! \param aszCmdLine is the command line to convert
//! \param apArgvBuffer is the buffer where the command line will be copied, apArgv will point to location
//!        in that buffer. It should be at least the same length as aaszCmdLine.
//! \param apArgv will contain the arguments.
int StrCommandLineToArgcArgv(const char* aszCmdLine, char* apArgvBuffer, char** apArgv, int anArgv0)
{
    int argc = 0;

    // Set to no argv elements, in case we have to bail out
    apArgv[anArgv0 + 0] = 0;

    // Copy the system version of the command line into our copy
    strcpy( apArgvBuffer, aszCmdLine );

    if ( '"' == *apArgvBuffer )   // If command line starts with a quote ("),
    {                           // it's a quoted filename.  Skip to next quote.
        apArgvBuffer++;

        apArgv[anArgv0 + 0] = apArgvBuffer;  // argv[0] == executable name

        while ( *apArgvBuffer && (*apArgvBuffer != '"') )
            apArgvBuffer++;

        if ( *apArgvBuffer )      // Did we see a non-NULL ending?
            *apArgvBuffer++ = 0;  // Null terminate and advance to next char
        else
            return 0;           // Oops!  We didn't see the end quote
    }
    else    // A regular (non-quoted) filename
    {
        apArgv[anArgv0 + 0] = apArgvBuffer;  // argv[0] == executable name

        while ( *apArgvBuffer && (' ' != *apArgvBuffer) && ('\t' != *apArgvBuffer) )
            apArgvBuffer++;

        if ( *apArgvBuffer )
            *apArgvBuffer++ = 0;  // Null terminate and advance to next char
    }

    // Done processing argv[0] (i.e., the executable name).  Now do th
    // actual arguments

    argc = 1;

    while ( 1 )
    {
        // Skip over any whitespace
        while ( *apArgvBuffer && (' ' == *apArgvBuffer) || ('\t' == *apArgvBuffer) )
            apArgvBuffer++;

        if ( 0 == *apArgvBuffer ) // End of command line???
            return argc;

        if ( '"' == *apArgvBuffer )   // Argument starting with a quote???
        {
            apArgvBuffer++;   // Advance past quote character

            apArgv[ anArgv0 + (argc++) ] = apArgvBuffer;
            apArgv[ anArgv0 + argc ] = 0;

            // Scan to end quote, or NULL terminator
            while ( *apArgvBuffer && (*apArgvBuffer != '"') )
                apArgvBuffer++;

            if ( 0 == *apArgvBuffer )
                return argc;

            if ( *apArgvBuffer )
                *apArgvBuffer++ = 0;  // Null terminate and advance to next char
        }
        else                        // Non-quoted argument
        {
            apArgv[ anArgv0 + (argc++) ] = apArgvBuffer;
            apArgv[ anArgv0 + argc ] = 0;

            // Skip till whitespace or NULL terminator
            while ( *apArgvBuffer && (' '!=*apArgvBuffer) && ('\t'!=*apArgvBuffer) )
                apArgvBuffer++;

            if ( 0 == *apArgvBuffer )
                return argc;

            if ( *apArgvBuffer )
                *apArgvBuffer++ = 0;  // Null terminate and advance to next char
        }

        if ( argc >= (_MAX_CMD_LINE_ARGS) )
            return argc;
    }
}

void
execcmd(
	const char *string,
	void (*func)( void *closure, int status, const char* output ),
	void *closure,
	const char* aShellPath,
    int serialOutput)
{
	int pid;
	int slot;
    char* buffer = NULL;
	const char *argv[ MAXARGC + 1 ];	/* +1 for NULL */
	int stringLen = 0;

    /* # ifdef USE_EXECNT */
    /* 	char *p; */
    /* # endif */

#ifdef USE_EXECNT
    if (globs.jobs > 1) {
        Sleep(10); // try to avoid have two process starting exactly at the same moment
                   // which causes trouble for some programs
    }
#endif

    if (!_firstExec) {
        srand(clock());
        _firstExec = 1;
    }

	/* Trim leading whitspaces */
	while( isspace( *string ) )
		++string;

	stringLen = strlen(string);

#ifdef _TRACE_EXECCMD
 	printf("UNXCMD(%d)\n",strlen(string));
#endif

	/* Find a slot in the running commands table for this one. */

	for (slot = 0; slot < MAXJOBS; ++slot)
	    if (!cmdtab[ slot ].pid)
            break;

	if( slot == MAXJOBS )
	{
	    printf( "no slots for child!\n" );
	    exit( EXITBAD );
	}

	/* Forumulate argv */
    if (aShellPath) {
        int i = 0;
        if (*aShellPath) {
            argv[i++] = aShellPath;
        }
        buffer = malloc(stringLen+1);
        StrCommandLineToArgcArgv(string,buffer,argv,i);
    }
    else {
        // Use Bash, the default shell
		int i = 0;
#ifdef USE_EXECNT
        extern char g_bash_path[_MAX_PATH];
        if (strstr(g_bash_path,"cygwin") == NULL && strstr(g_bash_path,"CYGWIN") == NULL)
        {
            /* write to a temporary bash script */
            {
                int maxTry = 10;
                while (maxTry--) {
                    FILE *f;
                    /* Write command to script file. */
                    f = fopen( cmdtab[ slot ].tempFile, "w" );
                    if (!f) {
                        Sleep(100);
                        continue;
                    }
                    fputs( string, f );
                    fclose( f );
#ifdef _TRACE_EXECCMD
                    printf("TEMP(%s) : %s\n", string, cmdtab[ slot ].tempFile);
#endif
                    string = cmdtab[slot].tempFile;
                    break;
                }
                if (maxTry == 0) {
                    printf( "can't write command script !\n" );
                    exit( EXITBAD );
                }
            }
            argv[i++] = g_bash_path;
            argv[i++] = string;
        }
        else
        {
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
	    istat = signal( SIGINT, onintr );

	/* Start the command */

# ifdef USE_EXECNT
    /****** WinNT *********/

#ifdef _TRACE_EXECCMD
	printf("EXEC-SPAWN (%s) (PATH:%s)\n",argv[0],getenv("PATH"));
#endif
	if ( serialOutput )
	{
		int	out, err, fd, bad_spawn = 0, spawn_err = -1;

		out = _dup (1);
		err = _dup (2);
		cmdtab[ slot ].outputFileUsed = 1;
		fd = open( cmdtab[ slot ].outputFile,
                   O_WRONLY | O_TRUNC | O_CREAT, 0644 );
		_dup2 (fd, 1);
		_dup2 (fd, 2);
		close (fd);

		if( ( pid = MY_SPAWNVP( P_NOWAIT, argv[0], argv ) ) == -1 )
		{
			bad_spawn = 1;
			spawn_err = errno;
		}

		_dup2 (out, 1);
		_dup2 (err, 2);
		close (out);
		close (err);

		if( bad_spawn )
		{
			errno = spawn_err;
			printf( "Ham: Error invoking spawn() for %s\n", argv[0] );
			perror( "spawn" );
			exit( EXITBAD );
		}
	}
    else {
        if( ( pid = MY_SPAWNVP( P_NOWAIT, argv[0], argv ) ) == -1 )
        {
            int i = 0;
            char tmp[8192] = {'\0'};
            /* 		printf("EXEC-SPAWN-ERRL %d (%s)\n",pid); */
            strcat(tmp,"spawn-unix(");
            while (argv[i] && i < 10) {
                strcat(tmp,argv[i]);
                strcat(tmp,",");
                ++i;
            }
            if (argv[i]) {
                strcat(tmp,"...");
            }
            strcat(tmp,")");
            strcat(tmp,"[");
            strcat(tmp,getenv("PATH"));
            strcat(tmp,"]");
            perror(tmp);
            /* 	    perror( "spawn" ); */
            exit( EXITBAD );
        }
    }
# else
    /****** POSIX *********/

# ifdef NO_VFORK
	if ((pid = fork()) == 0)
	{
        /* 		printf("EXEC-FORK\n"); */
		int fd;

		close( 1 );
		close( 2 );
		cmdtab[ slot ].outputFileUsed = 1;
		fd = open( cmdtab[ slot ].outputFile,
                   O_WRONLY | O_TRUNC | O_CREAT, 0644 );
		dup( fd );
		dup( fd );

	    execvp( argv[0], argv );
	    _exit(127);
	}
# else
	if ((pid = vfork()) == 0)
   	{
        /* 		printf("EXEC-VFORK\n"); */
		if ( serialOutput )
		{
			int fd;

			close( 1 );
			close( 2 );
			cmdtab[ slot ].outputFileUsed = 1;
			fd = open( cmdtab[ slot ].outputFile,
                       O_WRONLY | O_TRUNC | O_CREAT, 0644 );
			dup( fd );
			dup( fd );
		}

		execvp( argv[0], argv );
		_exit(127);
	}
# endif

	if( pid == -1 )
	{
	    perror( "vfork" );
	    exit( EXITBAD );
	}

# endif
	/* Save the operation for execwait() to find. */
	cmdtab[ slot ].pid = pid;
	cmdtab[ slot ].func = func;
	cmdtab[ slot ].closure = closure;

	/* Wait until we're under the limit of concurrent commands. */
	/* Don't trust globs.jobs alone. */

	while( cmdsrunning >= MAXJOBS || cmdsrunning >= globs.jobs )
	    if( !execwait() )
            break;

    if (buffer)
        free(buffer);
}

/*
 * execwait() - wait and drive at most one execution completion
 */

int
execwait()
{
	int i;
	int status, w;
	int rstat;

	/* Handle naive make1() which doesn't know if cmds are running. */

	if( !cmdsrunning )
	    return 0;

	/* Pick up process pid and status */

	while( ( w = wait( &status ) ) == -1 && errno == EINTR )
		;

	if( w == -1 )
	{
	    printf( "child process(es) lost!\n" );
	    perror("wait");
	    exit( EXITBAD );
	}

	/* Find the process in the cmdtab. */

	for( i = 0; i < MAXJOBS; i++ )
	    if( w == cmdtab[ i ].pid )
            break;

	if( i == MAXJOBS )
	{
	    printf( "waif child found!\n" );
	    exit( EXITBAD );
	}

	/* Drive the completion */
	if( !--cmdsrunning )
	    signal( SIGINT, istat );

	if (intr) {
	    rstat = EXEC_CMD_INTR;
    }
	else if( w == -1 || status != 0 ) {
	    rstat = EXEC_CMD_FAIL;
    }
	else {
	    rstat = EXEC_CMD_OK;
    }

	cmdtab[ i ].pid = 0;

	(*cmdtab[ i ].func)( cmdtab[ i ].closure, rstat, cmdtab[ i ].outputFileUsed ? cmdtab[ i ].outputFile : 0 );

	return 1;
}

# ifdef USE_MYWAIT

static int
my_wait( int *status )
{
	int i, num_active = 0;
	DWORD exitcode, waitcode;
	static HANDLE *active_handles = 0;

	if (!active_handles)
	    active_handles = (HANDLE *)malloc(globs.jobs * sizeof(HANDLE) );

	/* first see if any non-waited-for processes are dead,
	 * and return if so.
	 */
	for ( i = 0; i < globs.jobs; i++ ) {
	    if ( cmdtab[i].pid ) {
            if ( GetExitCodeProcess((HANDLE)cmdtab[i].pid, &exitcode) ) {
                if ( exitcode == STILL_ACTIVE )
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
	if ( !num_active ) {
	    errno = ECHILD;
	    return -1;
	}
	waitcode = WaitForMultipleObjects( num_active,
                                       active_handles,
                                       FALSE,
                                       INFINITE );
	if ( waitcode != WAIT_FAILED ) {
	    if ( waitcode >= WAIT_ABANDONED_0
             && waitcode < WAIT_ABANDONED_0 + num_active )
            i = waitcode - WAIT_ABANDONED_0;
	    else
            i = waitcode - WAIT_OBJECT_0;
	    if ( GetExitCodeProcess(active_handles[i], &exitcode) ) {
            CloseHandle(active_handles[i]);
            *status = (int)((exitcode & 0xff) << 8);
            return (int)active_handles[i];
	    }
	}

  FAILED:
	errno = GetLastError();
	return -1;

}

# endif /* USE_MYWAIT */

# endif /* USE_EXECUNIX */

#ifdef OS_NT

#include <stdlib.h>
#include <process.h>
#include <tchar.h>
#include <errno.h>
#include <string.h>
#include <malloc.h>
#include <mbstring.h>

#define SLASH _T("\\")
#define SLASHCHAR _T('\\')
#define XSLASHCHAR _T('/')
#define DELIMITER _T(";")

/* __osfile flag values for DOS file handles */
#define FOPEN           0x01    /* file handle open */
#define FEOFLAG         0x02    /* end of file has been encountered */
#define FCRLF           0x04    /* CR-LF across read buffer (in text mode) */
#define FPIPE           0x08    /* file handle refers to a pipe */
#define FNOINHERIT      0x10    /* file handle opened _O_NOINHERIT */
#define FAPPEND         0x20    /* file handle opened O_APPEND */
#define FDEV            0x40    /* file handle refers to device */
#define FTEXT           0x80    /* file handle is in text mode */
#define ISSLASH(c)      ( ((c) == SLASHCHAR) || ((c) == XSLASHCHAR) )

#define _ERRCHECK(exp) exp
#define _ERRCHECK_EINVAL(exp) exp

#define _VALIDATE_RETURN( expr, errorcode, retexpr )    \
    {                                                   \
        int _Expr_val=!!(expr);                         \
        if ( !( _Expr_val ) )                           \
        {                                               \
            errno = errorcode;                          \
            return ( retexpr );                         \
        }                                               \
    }

#define _VALIDATE_RETURN_ERRCODE( expr, errorcode ) \
    {                                               \
        int _Expr_val=!!(expr);                     \
        if ( !( _Expr_val ) )                       \
        {                                           \
            errno = errorcode;                      \
            return ( errorcode );                   \
        }                                           \
    }

static
__forceinline
errno_t __cdecl _my_dupenv_s_helper (
    char **pBuffer,
    size_t *pBufferSizeInTChars,
    const char *varname
    )
{
    const char *str;
    size_t size;

    /* validation section */
    _VALIDATE_RETURN_ERRCODE(pBuffer != NULL, EINVAL);
    *pBuffer = NULL;
    if (pBufferSizeInTChars != NULL)
    {
        *pBufferSizeInTChars = 0;
    }
    _VALIDATE_RETURN_ERRCODE(varname != NULL, EINVAL);

    str = getenv(varname);
    if (str == NULL)
    {
        return 0;
    }

    size = _tcslen(str) + 1;
    *pBuffer = (char*)calloc(size, sizeof(char));
    if (*pBuffer == NULL)
    {
        errno = ENOMEM;
        return errno;
    }

    _ERRCHECK(_tcscpy_s(*pBuffer, size, str));
    if (pBufferSizeInTChars != NULL)
    {
        *pBufferSizeInTChars = size;
    }
    return 0;
}

errno_t __cdecl _my_dupenv_s (
    char **pBuffer,
    size_t *pBufferSizeInTChars,
    const char *varname
    )
{
    errno_t retval;
    __try {
        retval = _my_dupenv_s_helper(pBuffer, pBufferSizeInTChars, varname);
    }
    __finally {
    }
    return retval;
}

/***
 *int _dospawn(mode, name, cmdblk, envblk) - spawn a child process
 *
 *Purpose:
 *       Spawns a child process
 *
 *Entry:
 *       int mode     - _P_WAIT, _P_NOWAIT, _P_NOWAITO, _P_OVERLAY, or _P_DETACH
 *       char *name   - name of program to execute
 *       char *cmdblk - parameter block
 *       char *envblk - environment block
 *
 *Exit:
 *       _P_OVERLAY: -1 = error, otherwise doesn't return
 *       _P_WAIT:    termination code << 8 + result code
 *       _P_DETACH: -1 = error, 0 = success
 *       others:    PID of process
 *
 *Exceptions:
 *
 *******************************************************************************/

intptr_t __cdecl my_dospawn (
    int mode,
    const char *name,
    char *cmdblk,
    char *envblk
    )
{
    char syncexec, asyncresult, background;
    LPTSTR CommandLine;
    STARTUPINFO StartupInfo;
    PROCESS_INFORMATION ProcessInformation;
    BOOL CreateProcessStatus;
    ULONG dosretval;                /* OS return value */
    DWORD exitcode;
    intptr_t retval;
    DWORD fdwCreate = 0;            /* flags for CreateProcess */
    int i;

    /* translate input mode value to individual flags */
    syncexec = asyncresult = background = 0;
    switch (mode) {
    case _P_WAIT:    syncexec=1;    break;  /* synchronous execution */
    case 2: /* _P_OVERLAY */
    case _P_NOWAITO: break;                 /* asynchronous execution */
    case _P_NOWAIT:  asyncresult=1; break;  /* asynch + remember result */
    case _P_DETACH:  background=1;  break;  /* detached in null scrn grp */
    default:
        _doserrno = 0;              /* not a Dos error */
        _VALIDATE_RETURN(("invalid mode", 0), EINVAL, -1);
    }

    /*
     * Loop over null separate arguments, and replace null separators
     * with spaces to turn it back into a single null terminated
     * command line.
     */
    CommandLine = cmdblk;
    while (*cmdblk) {
        while (*cmdblk) {
            cmdblk++;
        }

        /*
         * If not last argument, turn null separator into a space.
         */
        if (cmdblk[1] != _T('\0')) {
            *cmdblk++ = _T(' ');
        }
    }

    memset(&StartupInfo,0,sizeof(StartupInfo));
    StartupInfo.cb = sizeof(StartupInfo);

    StartupInfo.dwFlags |= STARTF_USESTDHANDLES;
    StartupInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput =  GetStdHandle(STD_OUTPUT_HANDLE);
    StartupInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    StartupInfo.dwFlags |= STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow = SW_HIDE;

    /*
     * if the child process is detached, it cannot access the console, so
     * we must remove the information passed for the first three handles.
     */
    if ( background ) {
        fdwCreate |= DETACHED_PROCESS;
    }
    else {
        fdwCreate |= CREATE_NO_WINDOW;
    }

    /*
     * errno is set to something non-zero if there are some
     * errors in the spawning
     */
    _doserrno = 0;

#ifdef WPRFLAG
    /* indicate to CreateProcess that environment block is wide */
    fdwCreate |= CREATE_UNICODE_ENVIRONMENT;
#endif  /* WPRFLAG */

    CreateProcessStatus = CreateProcess( (LPTSTR)name,
                                         CommandLine,
                                         NULL,
                                         NULL,
                                         TRUE,
                                         fdwCreate,
                                         envblk,
                                         NULL,
                                         &StartupInfo,
                                         &ProcessInformation
        );

    dosretval = GetLastError();
    free( StartupInfo.lpReserved2 );

    if (!CreateProcessStatus) {
        _dosmaperr(dosretval);
        return -1;
    }

    if (mode == 2 /* _P_OVERLAY */) {
        /* destroy ourselves */
        _exit(0);
    }
    else if (mode == _P_WAIT) {
        WaitForSingleObject(ProcessInformation.hProcess, (DWORD)(-1L));

        /* return termination code and exit code -- note we return
           the full exit code */
        GetExitCodeProcess(ProcessInformation.hProcess, &exitcode);

        retval = (intptr_t)(int)exitcode;

        CloseHandle(ProcessInformation.hProcess);
    }
    else if (mode == _P_DETACH) {
        /* like totally detached asynchronous spawn, dude,
           close process handle, return 0 for success */
        CloseHandle(ProcessInformation.hProcess);
        retval = (intptr_t)0;
    }
    else {
        /* asynchronous spawn -- return PID */
        retval = (intptr_t)ProcessInformation.hProcess;
    }

    CloseHandle(ProcessInformation.hThread);
    return retval;
}

/***
 *static int my_comexecmd(mode, name, argv, envp) - do the exec
 *       or spawn after name fixup
 *
 *Purpose:
 *       Spawns a child process with given parameters and environment.  Either
 *       overlays current process or loads in free memory while parent process
 *       waits.  If the named file is a .cmd file, modifies the calling sequence
 *       and prepends the /c and filename arguments into the command string
 *
 *       Exec doesn't take a mode; instead, the parent process goes away as
 *       the child process is brought in.
 *
 *Entry:
 *       int mode - mode to spawn (WAIT, NOWAIT, or OVERLAY)
 *                   only WAIT and OVERLAY currently supported
 *
 *           ****  mode is only used in the spawnve() version  ****
 *
 *       char *name - pathname of file to spawn.  Includes the extension
 *       char **argv - vector of parameter strings
 *       char **envp - vector of environment variables
 *
 *Exit:
 *       returns exit code of child process
 *       if fails, returns -1
 *
 *Exceptions:
 *       Returns a value of (-1) to indicate an error in exec'ing the child
 *       process.  errno may be set to:
 *
 *       E2BIG   = failed in argument/environment processing (_cenvarg)
 *                 argument list or environment too big;
 *       EACCESS = locking or sharing violation on file;
 *       EMFILE  = too many files open;
 *       ENOENT  = failed to find program name - no such file or directory;
 *       ENOEXEC = failed in exec - bad executable format;
 *       ENOMEM  = failed in memory allocation (during malloc, or in
 *                 setting up memory for executing child process).
 *
 *******************************************************************************/

static intptr_t __cdecl my_comexecmd (
    int mode,
    const char *name,
    const char * const *argv,
    const char * const *envp
    )
{
    char *argblk;
    char *envblk;
    intptr_t rc;

    /* validation section */
    _VALIDATE_RETURN(name != NULL, EINVAL, -1);
    _VALIDATE_RETURN(argv != NULL, EINVAL, -1);

    if (_cenvarg(argv, envp, &argblk, &envblk, name) == -1)
        return -1;

    rc = my_dospawn(mode, name, argblk, envblk);

    /* free memory */
    free(argblk);
    free(envblk);

    return rc;
}

/***
 *int _spawnve(mode, name, argv, envp) - low level _spawnXX library function
 *int _execve(name, argv, envp) - low level _execXX library function
 *
 *Purpose:
 *       spawns or execs a child process; takes a single pointer to an argument
 *       list as well as a pointer to the environment; unlike _spawnvpe,
 *       _spawnve does not search the PATH= list in processing the name
 *       parameter; mode specifies the parent's execution mode.
 *
 *Entry:
 *       int mode    - parent process's execution mode:
 *                     must be one of _P_OVERLAY, _P_WAIT, _P_NOWAIT;
 *                     not used for _execve
 *       char *name  - path name of program to spawn;
 *       char **argv - pointer to array of pointers to child's arguments;
 *       char **envp - pointer to array of pointers to child's environment
 *                     settings.
 *
 *Exit:
 *       Returns : (int) a status value whose meaning is as follows:
 *               0        = normal termination of child process;
 *               positive = exit code of child upon error termination
 *                          (abort or exit(nonzero));
 *               -1       = child process was not spawned;
 *                          errno indicates what kind of error:
 *                          (E2BIG, EINVAL, ENOENT, ENOEXEC, ENOMEM).
 *
 *Exceptions:
 *       Returns a value of (-1) to indicate an error in spawning the child
 *       process.  errno may be set to:
 *
 *       E2BIG   = failed in argument/environment processing (_cenvarg) -
 *                 argument list or environment too big;
 *       EINVAL  = invalid mode argument;
 *       ENOENT  = failed to find program name - no such file or directory;
 *       ENOEXEC = failed in spawn - bad executable format;
 *       ENOMEM  = failed in memory allocation (during malloc, or in
 *                 setting up memory for spawning child process).
 *
 *******************************************************************************/

/* Extension array - ordered in search order from right to left.

   ext_strings  = array of extensions
*/

static char *ext_strings[] = { _T(".cmd"), _T(".bat"), _T(".exe"), _T(".com") };
enum {CMD, BAT, EXE, COM, EXTFIRST=CMD, EXTLAST=COM};

intptr_t __cdecl

_my_spawnve (
    int mode,
    const char *name,
    const char * const *argv,
    const char * const *envp
    )
{
    char *ext;   /* where the extension goes if we have to add one */
    char *p;
    char *q;
    char *pathname = (char *)name;
    intptr_t rc;
    int i;
    errno_t save_errno = 0;

    /* validation section */
    _VALIDATE_RETURN(name != NULL, EINVAL, -1);
    _VALIDATE_RETURN(*name != _T('\0'), EINVAL, -1);
    _VALIDATE_RETURN(argv != NULL, EINVAL, -1);
    _VALIDATE_RETURN(*argv != NULL, EINVAL, -1);
    _VALIDATE_RETURN(**argv != _T('\0'), EINVAL, -1);

    p = _tcsrchr(pathname, SLASHCHAR);
    q = _tcsrchr(pathname, XSLASHCHAR);

    /* ensure that pathname is an absolute or relative pathname. also,
     * position p to point at the filename portion of pathname (i.e., just
     * after the last occurence of a colon, slash or backslash character */

    if (!q) {
        if (!p)
            if (!(p = _tcschr(pathname, _T(':')))) {

                /* pathname is a filename only, force it to be
                 * a relative pathname. note that an extra byte
                 * is malloc-ed just in case pathname is NULL,
                 * to keep the heap from being trashed by
                 * strcpy */
                size_t pathname_size = _tcslen(pathname) + 3;
                if (!(pathname = calloc(pathname_size, sizeof(char))))
                    return(-1);

                _ERRCHECK(_tcscpy_s(pathname, pathname_size, _T(".\\")));
                _ERRCHECK(_tcscat_s(pathname, pathname_size, name));

                /* set p to point to the start of the filename
                 * (i.e., past the ".\\" prefix) */
                p = pathname + 2;
            }
        /* else pathname has drive specifier prefix and p is
         * is pointing to the ':' */
    }
    else if (!p || q > p)   /* p == NULL or q > p */
        p = q;


    rc = -1;        /* init to error value */

    if (ext = _tcsrchr(p, _T('.')))  {

        /* extension given; only do filename */

        if (_taccess_s(pathname, 0) == 0) {
            rc = my_comexecmd(mode, pathname, argv, envp);
        }

    }
    else    {

        /* no extension; try .cmd/.bat, then .com and .exe */

        size_t size = _tcslen(pathname) + 5;
        if (!(p = calloc(size, sizeof(char))))
            return(-1);

        _ERRCHECK(_tcscpy_s(p, size, pathname));
        ext = p + _tcslen(pathname);

        save_errno = errno;
        for (i = EXTLAST; i >= EXTFIRST; --i) {
            _ERRCHECK(_tcscpy_s(ext, size - (ext - p), ext_strings[i]));

            if (_taccess_s(p, 0) == 0) {
                errno = save_errno;
                rc = my_comexecmd(mode, p, argv, envp);
                break;
            }
        }
        free(p);
    }

    if (pathname != name)
        free(pathname);

    return rc;
}

/***
 *_spawnvpe(modeflag, filename, argv, envptr) - spawn a child process
 *
 *Purpose:
 *       Spawns a child process with the given arguments and environ,
 *       searches along PATH for given file until found.
 *       Formats the parameters and calls _spawnve to do the actual work. The
 *       NULL environment pointer indicates that the new process will inherit
 *       the parents process's environment.  NOTE - at least one argument must
 *       be present.  This argument is always, by convention, the name of the
 *       file being spawned.
 *
 *Entry:
 *       int modeflag - defines mode of spawn (WAIT, NOWAIT, or OVERLAY)
 *                       only WAIT and OVERLAY supported
 *       char *filename - name of file to execute
 *       char **argv - vector of parameters
 *       char **envptr - vector of environment variables
 *
 *Exit:
 *       returns exit code of spawned process
 *       if fails, returns -1
 *
 *Exceptions:
 *
 *******************************************************************************/

intptr_t __cdecl _my_spawnvpe (
    int modeflag,
    const char *filename,
    const char * const *argv,
    const char * const *envptr
    )
{
    intptr_t i;
    char *envbuf = NULL;
    char *env;
    char *buf = NULL;
    char *pfin;
    errno_t save_errno;

    /* validation section */
    _VALIDATE_RETURN(filename != NULL, EINVAL, -1);
    _VALIDATE_RETURN(*filename != _T('\0'), EINVAL, -1);
    _VALIDATE_RETURN(argv != NULL, EINVAL, -1);
    _VALIDATE_RETURN(*argv != NULL, EINVAL, -1);
    _VALIDATE_RETURN(**argv != _T('\0'), EINVAL, -1);

    save_errno = errno;
    errno = 0;

    if (
        (i = _my_spawnve(modeflag, filename, argv, envptr)) != -1
        /* everything worked just fine; return i */

        || (errno != ENOENT)
        /* couldn't spawn the process, return failure */

        || (_tcschr(filename, XSLASHCHAR) != NULL)
        /* filename contains a '/', return failure */

        || (_ERRCHECK_EINVAL(_my_dupenv_s(&envbuf, NULL, _T("PATH"))) != 0)
        || (envbuf == NULL)
        /* no PATH environment string name, return failure */

        || ( (buf = calloc(_MAX_PATH, sizeof(char))) == NULL )
        /* cannot allocate buffer to build alternate pathnames, return
         * failure */
        ) {
        goto done;
    }

    /* could not find the file as specified, search PATH. try each
     * component of the PATH until we get either no error return, or the
     * error is not ENOENT and the component is not a UNC name, or we run
     * out of components to try.
     */

    env = envbuf;
    while ( (env = _getpath(env, buf, _MAX_PATH - 1)) && (*buf) ) {
        pfin = buf + _tcslen(buf) - 1;

        /* if necessary, append a '/'
         */
        if (*pfin != SLASHCHAR && *pfin != XSLASHCHAR)
            _ERRCHECK(_tcscat_s(buf, _MAX_PATH, SLASH));

        /* check that the final path will be of legal size. if so,
         * build it. otherwise, return to the caller (return value
         * and errno rename set from initial call to _spawnve()).
         */
        if ( (_tcslen(buf) + _tcslen(filename)) < _MAX_PATH )
            _ERRCHECK(_tcscat_s(buf, _MAX_PATH, filename));
        else
            break;

        /* try spawning it. if successful, or if errno comes back with a
         * value other than ENOENT and the pathname is not a UNC name,
         * return to the caller.
         */
        errno = 0;
        if ( (i = _my_spawnve(modeflag, buf, argv, envptr)) != -1
             || (((errno != ENOENT) && (_doserrno != ERROR_NOT_READY))
                 && (!ISSLASH(*buf) || !ISSLASH(*(buf+1)))) )
            break;

    }

  done:
    if (errno == 0) {
        errno = save_errno;
    }
    if (buf != NULL)
        free(buf);
    if (envbuf != NULL)
        free(envbuf);
    return(i);
}

intptr_t __cdecl _my_spawnvp (
    int modeflag,
    const char *filename,
    const char * const *argv
    )
{
    return _my_spawnvpe(modeflag, filename, argv, NULL);
}

#endif

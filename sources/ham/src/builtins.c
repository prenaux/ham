/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * builtins.c - builtin jam rules
 *
 * External routines:
 *
 *  load_builtin() - define builtin rules
 *
 * Internal routines:
 *
 *  builtin_depends() - DEPENDS/INCLUDES rule
 *  builtin_echo() - ECHO rule
 *  builtin_exit() - EXIT rule
 *  builtin_flags() - NOCARE, NOTFILE, TEMPORARY rule
 *  builtin_glob() - GLOB rule
 *  builtin_match() - MATCH rule
 *  builtin_hdrmacro() - HDRMACRO rule
 *
 * 01/10/01 (seiwald) - split from compile.c
 * 01/08/01 (seiwald) - new 'Glob' (file expansion) builtin
 * 03/02/02 (seiwald) - new 'Match' (regexp match) builtin
 * 04/03/02 (seiwald) - Glob matches only filename, not directory
 * 10/22/02 (seiwald) - list_new() now does its own newstr()/copystr()
 * 10/22/02 (seiwald) - working return/break/continue statements
 * 11/04/02 (seiwald) - const-ing for string literals
 * 12/03/02 (seiwald) - fix odd includes support by grafting them onto depends
 * 01/14/03 (seiwald) - fix includes fix with new internal includes TARGET
 */

# include "jam.h"

# include "lists.h"
# include "parse.h"
# include "builtins.h"
# include "rules.h"
# include "filesys.h"
# include "newstr.h"
# include "regexp.h"
# include "pathsys.h"
# include "hdrmacro.h"
# include "buffer.h"
# include "execcmd.h"
# include "sha256.h"

#if defined OS_NT
#include <sys/stat.h>
#endif

#if defined OS_MACOSX || defined OS_LINUX
#include <unistd.h>
#endif
#include <time.h>

/*
 * compile_builtin() - define builtin rules
 */

# define P0 (PARSE *)0
# define C0 (char *)0

LIST *builtin_depends( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_echo( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_fecho( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_fexists( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_fisdir( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_exit( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_flags( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_glob( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_globstring( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_hdrmacro( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_bash( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_execcmd( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_match( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_subst( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_subst_literalize( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_math( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_split( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_absolutepath( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_strafter( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_strafteri( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_sort( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_sorti( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_sha256( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_sha256_128( PARSE *parse, LOL *args, int *jmp );
LIST *builtin_sha256_64( PARSE *parse, LOL *args, int *jmp );

int glob( const char *s, const char *c );

void
load_builtins()
{
  bindrule( "Always" )->procedure =
      bindrule( "ALWAYS" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_TOUCHED );

  bindrule( "Depends" )->procedure =
      bindrule( "DEPENDS" )->procedure =
      parse_make( builtin_depends, P0, P0, P0, C0, C0, 0 );

  bindrule( "echo" )->procedure =
      bindrule( "Echo" )->procedure =
      bindrule( "ECHO" )->procedure =
      parse_make( builtin_echo, P0, P0, P0, C0, C0, 0 );

  bindrule( "fecho" )->procedure =
      bindrule( "FEcho" )->procedure =
      bindrule( "FECHO" )->procedure =
      parse_make( builtin_fecho, P0, P0, P0, C0, C0, T_FLAG_TOUCHED );

  bindrule( "fexists" )->procedure =
      bindrule( "FExists" )->procedure =
      bindrule( "FEXISTS" )->procedure =
      parse_make( builtin_fexists, P0, P0, P0, C0, C0, 0 );

  bindrule( "fisdir" )->procedure =
      bindrule( "FIsDir" )->procedure =
      bindrule( "FISDIR" )->procedure =
      parse_make( builtin_fisdir, P0, P0, P0, C0, C0, 0 );

  bindrule( "exit" )->procedure =
      bindrule( "Exit" )->procedure =
      bindrule( "EXIT" )->procedure =
      parse_make( builtin_exit, P0, P0, P0, C0, C0, 0 );

  bindrule( "Glob" )->procedure =
      bindrule( "GLOB" )->procedure =
      parse_make( builtin_glob, P0, P0, P0, C0, C0, 0 );

  bindrule( "GlobString" )->procedure =
      bindrule( "GLOBSTRING" )->procedure =
      parse_make( builtin_globstring, P0, P0, P0, C0, C0, 0 );

  bindrule( "Includes" )->procedure =
      bindrule( "INCLUDES" )->procedure =
      parse_make( builtin_depends, P0, P0, P0, C0, C0, 1 );

  bindrule( "Leaves" )->procedure =
      bindrule( "LEAVES" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_LEAVES );

  bindrule( "NoCare" )->procedure =
      bindrule( "NOCARE" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_NOCARE );

  bindrule( "NOTIME" )->procedure =
      bindrule( "NotFile" )->procedure =
      bindrule( "NOTFILE" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_NOTFILE );

  bindrule( "NoUpdate" )->procedure =
      bindrule( "NOUPDATE" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_NOUPDATE );

  bindrule( "Temporary" )->procedure =
      bindrule( "TEMPORARY" )->procedure =
      parse_make( builtin_flags, P0, P0, P0, C0, C0, T_FLAG_TEMP );

  bindrule( "HdrMacro" )->procedure =
      bindrule( "HDRMACRO" )->procedure =
      parse_make( builtin_hdrmacro, P0, P0, P0, C0, C0, 0 );

  bindrule( "Bash" )->procedure =
      bindrule( "BASH" )->procedure =
      parse_make( builtin_bash, P0, P0, P0, C0, C0, 0 );

  bindrule( "ExecCmd" )->procedure =
      bindrule( "EXECCMD" )->procedure =
      parse_make( builtin_execcmd, P0, P0, P0, C0, C0, 0 );

  bindrule( "Subst" )->procedure =
      bindrule( "SUBST" )->procedure =
      parse_make( builtin_subst, P0, P0, P0, C0, C0, 0 );
  bindrule( "SubstLiteralize" )->procedure =
      bindrule( "SUBSTLITERALIZE" )->procedure =
      parse_make( builtin_subst_literalize, P0, P0, P0, C0, C0, 0 );

  bindrule( "Math" )->procedure =
      bindrule( "MATH" )->procedure =
      parse_make( builtin_math, P0, P0, P0, C0, C0, 0 );

  bindrule( "Match" )->procedure =
      bindrule( "MATCH" )->procedure =
      parse_make( builtin_match, P0, P0, P0, C0, C0, 0 );

  bindrule( "Split" )->procedure =
      bindrule( "SPLIT" )->procedure =
      parse_make( builtin_split, P0, P0, P0, C0, C0, 0 );

  bindrule( "GetAbsolutePath" )->procedure =
      bindrule( "GETABSOLUTEPATH" )->procedure =
      parse_make( builtin_absolutepath, P0, P0, P0, C0, C0, 0 );

  bindrule( "StrAfter" )->procedure =
      bindrule( "STRAFTER" )->procedure =
      parse_make( builtin_strafter, P0, P0, P0, C0, C0, 0 );
  bindrule( "StrAfterI" )->procedure =
      bindrule( "STRAFTERI" )->procedure =
      parse_make( builtin_strafteri, P0, P0, P0, C0, C0, 0 );

  bindrule( "Sort" )->procedure =
      bindrule( "SORT" )->procedure =
      parse_make( builtin_sort, P0, P0, P0, C0, C0, 0 );
  bindrule( "SortI" )->procedure =
      bindrule( "SORTI" )->procedure =
      parse_make( builtin_sorti, P0, P0, P0, C0, C0, 0 );

  bindrule( "Sha256" )->procedure =
      bindrule( "SHA256" )->procedure =
      parse_make( builtin_sha256, P0, P0, P0, C0, C0, 0 );

  bindrule( "Sha256_128" )->procedure =
      bindrule( "SHA256_128" )->procedure =
      parse_make( builtin_sha256_128, P0, P0, P0, C0, C0, 0 );

  bindrule( "Sha256_64" )->procedure =
      bindrule( "SHA256_64" )->procedure =
      parse_make( builtin_sha256_64, P0, P0, P0, C0, C0, 0 );
}

/*
 * builtin_depends() - DEPENDS/INCLUDES rule
 *
 * The DEPENDS builtin rule appends each of the listed sources on the
 * dependency list of each of the listed targets.  It binds both the
 * targets and sources as TARGETs.
 */

LIST *
builtin_depends(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *targets = lol_get( args, 0 );
  LIST *sources = lol_get( args, 1 );
  int which = parse->num;
  LIST *l;

  for( l = targets; l; l = list_next( l ) )
  {
    TARGET *t = bindtarget( l->string );

    /* If doing INCLUDES, switch to the TARGET's include */
    /* TARGET, creating it if needed.  The internal include */
    /* TARGET shares the name of its parent. */

    if( parse->num )
    {
      if( !t->includes )
        t->includes = copytarget( t );
      t = t->includes;
    }

    t->depends = targetlist( t->depends, sources );
  }

  return L0;
}

/*
 * builtin_echo() - ECHO rule
 *
 * The ECHO builtin rule echoes the targets to the user.  No other
 * actions are taken.
 */

LIST *
builtin_echo(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  list_print( lol_get( args, 0 ) );
  printf( "\n" );
  return L0;
}

/*
 * builtin_fecho() - FECHO file : TEXT : [create|append(default)|nl]
 *
 * The FECHO builtin rule echoes the sources to the target files. No other
 * actions are taken.
 *
 */

LIST *
builtin_fecho(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *targets = lol_get( args, 0 );
  LIST *sources = lol_get( args, 1 );
  LIST *flags = lol_get( args, 2 );
  LIST *l;
  FILE *fp = NULL;

  /* Process flags */
  char create = 0;
  char newline = 0;
  for( l = flags; l; l = list_next(l)) {
    if (strcmp("create",l->string) == 0)
      create = 1;
    else if (strcmp("nl",l->string) == 0)
      newline = 1;
  }

  for( l = targets; l; l = list_next( l ) )
  {
    TARGET *t = bindtarget( l->string );
    t->flags |= parse->num;

    fp = fopen(t->boundname,create?"wb":"ab");
    if (fp) {
      list_fprint(fp, sources);
      if (newline) fprintf(fp,"\n");
      fclose(fp);
    }
    else {
      printf("warning: fecho: can't open file '%s' !\n",t->boundname);
    }
  }

  return L0;
}

/*
 * builtin_fexists() - FEXIST file
 *
 * Return 1 if all files exists, else NULL
 *
 */

LIST *
builtin_fexists(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *targets = lol_get( args, 0 );
  LIST *l;
  time_t time;
  for( l = targets; l; l = list_next( l ) )
  {
    int r;
    TARGET *t = bindtarget( l->string );
    t->flags |= parse->num;
    r = file_time(t->boundname,&time);
    if (r < 0)
      return L0;
  }
  return list_new(L0,"1",0);
}

/*
 * builtin_fisdir() - fisdir file
 *
 * Return 1 if all exists and are directories, else NULL
 *
 */

LIST *
builtin_fisdir(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *targets = lol_get( args, 0 );
  LIST *l;
  time_t time;
  for( l = targets; l; l = list_next( l ) )
  {
    struct stat statbuf;
    TARGET *t = bindtarget(l->string);
    t->flags |= parse->num;
    if (stat(t->boundname, &statbuf ) < 0)
      return L0;
    if (!(statbuf.st_mode&S_IFDIR)) {
      return L0;
    }
  }
  return list_new(L0,"1",0);
}

/*
 * builtin_exit() - EXIT rule
 *
 * The EXIT builtin rule echoes the targets to the user and exits
 * the program with a failure status.
 */

LIST *
builtin_exit(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  list_print( lol_get( args, 0 ) );
  printf( "\n" );
  exit( EXITBAD ); /* yeech */
  return L0;
}

/*
 * builtin_flags() - NOCARE, NOTFILE, TEMPORARY rule
 *
 * Builtin_flags() marks the target with the appropriate flag, for use
 * by make0().  It binds each target ARGET.
 */

LIST *
builtin_flags(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *l = lol_get( args, 0 );

  for( ; l; l = list_next( l ) )
    bindtarget( l->string )->flags |= parse->num;

  return L0;
}

/*
 * builtin_globbing() - GLOB rule
 */

struct globbing {
  LIST  *patterns;
  LIST  *results;
} ;

static void
builtin_glob_back(
    void  *closure,
    const char *file,
    int status,
    time_t  time )
{
  struct globbing *globbing = (struct globbing *)closure;
  LIST    *l;
  PATHNAME  f;
  char    buf[ MAXJPATH ];

  /* Null out directory for matching. */
  /* We wish we had file_dirscan() pass up a PATHNAME. */

  path_parse( file, &f );
  f.f_dir.len = 0;
  path_build( &f, buf, 0 );

  for( l = globbing->patterns; l; l = l->next )
    if( !glob( l->string, buf ) )
    {
      globbing->results = list_new( globbing->results, file, 0 );
      break;
    }
}

LIST *
builtin_glob(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *l = lol_get( args, 0 );
  LIST *r = lol_get( args, 1 );

  struct globbing globbing;

  globbing.results = L0;
  globbing.patterns = r;

  for( ; l; l = list_next( l ) )
    file_dirscan( l->string, builtin_glob_back, &globbing );

  return globbing.results;
}

/*
 * builtin_globstring() - check whether the specified strings match
 *
 * Return 1 if all name match the glob, else NULL
 *
 */

LIST *
builtin_globstring(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *targets = lol_get( args, 0 );
  LIST *l, *r;
  time_t time;
  for (l = targets; l; l = list_next( l ))
  {
    struct stat statbuf;
    TARGET *t = bindtarget(l->string);
    int hasOneMatch = 0;
    for (r = lol_get( args, 1 ); r; r = r->next) {
      TARGET* rt = bindtarget(r->string);
      int gr = glob(rt->boundname,t->boundname);
      if (gr == 0) { // is a match
        hasOneMatch = 1;
        break;
      }
    }
    if (!hasOneMatch) // no match
      return L0;
  }

  return list_new(L0,"1",0);
}

LIST *
builtin_hdrmacro(
    PARSE    *parse,
    LOL      *args,
    int      *jmp )
{
  LIST*  l = lol_get( args, 0 );

  for ( ; l; l = list_next(l) )
  {
    TARGET*  t = bindtarget( l->string );

    /* scan file for header filename macro definitions */
    if ( DEBUG_HEADER )
      printf( "scanning '%s' for header file macro definitions\n",
              l->string );

    macro_headers( t );
  }

  return L0;
}

static void bash_done( void *closure, int status, const char* outputname )
{
  char* buffer;
  long size;
  LIST **list = (LIST**)closure;
  FILE *file = fopen(outputname, "rb");
  if (!file)
    return;
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return;
  }
  size = ftell(file);

  fseek(file, 0, SEEK_SET);

  buffer = malloc(size + 1);
  if (fread(buffer, 1, size, file) != size) {
    free(buffer);
    fclose(file);
    return;
  }
  buffer[size] = 0;

  *list = list_new(*list, buffer, 0);

  free(buffer);
  fclose(file);
}

LIST *
builtin_bash(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *l = lol_get( args, 0 );
  LIST *output = L0;
  exec_init();
  for( ; l; l = list_next( l ) ) {
    execcmd( l->string, bash_done, &output, NULL, 1 );
    execwait();
  }
  exec_done();
  return output;
}

LIST *
builtin_execcmd(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *l = lol_get( args, 0 );
  LIST *output = L0;
  exec_init();
  for( ; l; l = list_next( l ) ) {
    execcmd( l->string, bash_done, &output, NULL, 1 );
    execwait();
  }
  exec_done();
  return output;
}

extern int str_gsub (BUFFER *buff, const char *src, const char *p, const char *repl, int max_s);

LIST *
builtin_subst(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *liststring;
  LIST *result = 0;
  LIST *pattern = lol_get( args, 1 );
  LIST *repl = lol_get( args, 2 );
  LIST *nstr = lol_get( args, 3 );
  int n = nstr ? atoi( nstr->string ) : -1;

  /* For each string */

  for( liststring = lol_get( args, 0 ); liststring; liststring = liststring->next )
  {
    BUFFER buff;
    buffer_init( &buff );
    str_gsub (&buff, liststring->string, pattern->string, repl ? repl->string : "", n);
    result = list_new( result, buffer_ptr( &buff ), 0 );
    buffer_free( &buff );
  }

  return result;
}



LIST *builtin_subst_literalize( PARSE *parse, LOL *args, int  *jmp )
{
  LIST *pattern;
  LIST *result = L0;

  for( pattern = lol_get( args, 0 ); pattern; pattern = pattern->next )
  {
    const char* patternString;
    BUFFER patternBuff;
    buffer_init( &patternBuff );

    for ( patternString = pattern->string; *patternString; ++patternString )
    {
      if ( *patternString == '('  ||  *patternString == ')'  ||  *patternString == '.'  ||
           *patternString == '%'  ||  *patternString == '+'  ||  *patternString == '-'  ||
           *patternString == '*'  ||  *patternString == '?'  ||  *patternString == '['  ||
           *patternString == ']'  ||  *patternString == '^'  ||  *patternString == '$' )
      {
        buffer_addchar( &patternBuff, '%' );
      }
      buffer_addchar( &patternBuff, *patternString );
    }
    buffer_addchar( &patternBuff, 0 );
    result = list_new( result, buffer_ptr( &patternBuff ), 0 );
  }

  return result;
}

/*
 * builtin_match() - MATCH rule, regexp matching
 */

LIST *
builtin_match(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *l, *r;
  LIST *result = 0;

  /* For each pattern */

  for( l = lol_get( args, 0 ); l; l = l->next )
  {
    regexp *re = regcomp( l->string );

    /* For each string to match against */

    for( r = lol_get( args, 1 ); r; r = r->next )
      if( regexec( re, r->string ) )
      {
        int i, top;

        /* Find highest parameter */

        for( top = NSUBEXP; top-- > 1; )
          if( re->startp[top] )
            break;

        /* And add all parameters up to highest onto list. */
        /* Must have parameters to have results! */

        for( i = 1; i <= top; i++ )
        {
          BUFFER buff;
          size_t l;
          buffer_init( &buff );
          l = re->endp[i] - re->startp[i];
          buffer_addstring( &buff, re->startp[i], l );
          buffer_addchar( &buff, 0 );
          result = list_new( result, buffer_ptr( &buff ), 0 );
          buffer_free( &buff );
        }
      }

    free( (char *)re );
  }

  return result;
}

LIST *
builtin_math(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  char buffer[100];
  int num1;
  int num2;
  int result;

  LIST *expression = lol_get( args, 0 );
  if ( !expression  ||  !expression->next  ||  !expression->next->next )
    return NULL;

  num1 = atoi( expression->string );
  num2 = atoi( expression->next->next->string );
  result = 0;

  switch ( expression->next->string[0] )
  {
    case '+':   result = num1 + num2;    break;
    case '-':   result = num1 - num2;    break;
    case '*':   result = num1 * num2;    break;
    case '/':   result = num1 / num2;    break;
    case '%':   result = num1 % num2;    break;
    default:
      printf( "jam: rule Math: Unknown operator [%s].\n", expression->next->string );
      exit( EXITBAD );
  }

  sprintf(buffer, "%d", result);

  return list_new(L0, buffer, 0);
}

LIST *
builtin_split(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST* input  = lol_get( args, 0 );
  LIST* tokens = lol_get( args, 1 );
  LIST* result = L0;
  char  token[256];
  BUFFER  buff;

  buffer_init( &buff );

  /* build token array */
  memset( token, 0, sizeof( token ) );
  for ( ; tokens; tokens = tokens->next ) {
    const char* s = tokens->string;
    if (strcmp(s,"%t") == 0) token['\t'] = 1;
    else if (strcmp(s,"%n") == 0) token['\n'] = 1;
    else if (strcmp(s,"%r") == 0) token['\r'] = 1;
    else if (strcmp(s,"%nl") == 0) {
      token['\r'] = 1;
      token['\n'] = 1;
    }
    else if (strcmp(s,"%s") == 0) {
      token[' '] = 1;
      token['\t'] = 1;
    }
    else if (strcmp(s,"%w") == 0) {
      token[' '] = 1;
      token['\t'] = 1;
      token['\r'] = 1;
      token['\n'] = 1;
    }
    else if (*s == '%' && *(s+1) >= '0' && *(s+1) <= '9') {
      token[(unsigned char)(atoi(s+1))] = 1;
    }
    else {
      for ( ; *s; s++ )
        token[(unsigned char)*s] = 1;
    }
  }

  /* now parse the input and split it */
  for ( ; input; input = input->next ) {
    const char* ptr = input->string;
    const char* lastPtr = input->string;

    while ( *ptr ) {
      if ( token[(unsigned char) *ptr] ) {
        size_t count = ptr - lastPtr;
        if ( count > 0 ) {
          buffer_reset( &buff );
          buffer_addstring( &buff, lastPtr, count );
          buffer_addchar( &buff, 0 );
          result = list_new( result, buffer_ptr( &buff ), 0 );
        }
        lastPtr = ptr + 1;
      }
      ++ptr;
    }
    if ( ptr > lastPtr )
      result = list_new( result, lastPtr, 0 );
  }

  buffer_free( &buff );
  return  result;
}

#ifdef OS_NT
#include <windows.h>
#pragma comment(lib,"kernel32.lib")
static int _GetAbsolutePath(const char* input, BUFFER* buff) {
  char buffer[_MAX_PATH] = {0};
  char* bufferFile;
  // reset the output buffer so that its empty if _GetAbsolutPath fails
  buffer_reset(buff);
  if (GetFullPathNameA(input,_MAX_PATH,buffer,&bufferFile)) {
    buffer_addstring(buff,buffer,strlen(buffer));
    buffer_addchar(buff,0);
    return 1;
  }
  return 0;
}
#elif defined OS_MACOSX || defined OS_LINUX
static int _GetAbsolutePath(const char* input, BUFFER* buff) {
  char* resolvedPath = realpath(input,NULL);
  // reset the output buffer so that its empty if _GetAbsolutPath fails
  buffer_reset(buff);
  if (resolvedPath) {
    buffer_addstring(buff,resolvedPath,strlen(resolvedPath));
    buffer_addchar(buff,0);
    free(resolvedPath);
    return 1;
  }
  else {
    // if 'realpath' failed do what GetFullPathNameA on Windows does: CWD + PATH
    char cwd[4096];
    getcwd(cwd,4094); // -2 for the 0 and potential added end slash
    {
      const int cwdlen = strlen(cwd);
      if (cwd[cwdlen-1] != '/') {
        cwd[cwdlen] = '/';
        cwd[cwdlen+1] = 0;
      }
    }
    buffer_addstring(buff,cwd,strlen(cwd));
    buffer_addstring(buff,input,strlen(input));
    buffer_addchar(buff,0);
    return 1;
  }
  return 0;
}
#else
#error "_GetAbsolutePath not implemented on this platform."
#endif

LIST *
builtin_absolutepath(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST* input  = lol_get( args, 0 );
  LIST* result = L0;
  char  token[256];
  BUFFER  buff;
  buffer_init( &buff );

  for ( ; input; input = input->next ) {
    const char* ptr = input->string;
    if (_GetAbsolutePath(input->string,&buff)) {
      result = list_new( result, buffer_ptr( &buff ), 0 );
    }
  }

  buffer_free( &buff );
  return  result;
}

/*
 * builtin_strafter()
 */

LIST *
builtin_strafter(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *liststring;
  LIST *result = 0;
  LIST *pattern = lol_get( args, 1 );
  int patternLen = strlen(pattern->string), stringLen;

  /* For each string */

  for( liststring = lol_get( args, 0 ); liststring; liststring = liststring->next )
  {
    BUFFER buff;
    buffer_init( &buff );

    stringLen = strlen(liststring->string);
    if (stringLen >= patternLen &&
        strncmp(liststring->string,pattern->string,patternLen) == 0)
    {
      buffer_addstring(&buff,liststring->string+patternLen,stringLen-patternLen);
    }
    else {
      buffer_addstring(&buff,liststring->string,stringLen);
    }
    buffer_addchar( &buff, 0 );

    result = list_new( result, buffer_ptr( &buff ), 0 );
    buffer_free( &buff );
  }

  return result;
}

/*
 * builtin_strafteri()
 */

LIST *
builtin_strafteri(
    PARSE *parse,
    LOL *args,
    int *jmp )
{
  LIST *liststring;
  LIST *result = 0;
  LIST *pattern = lol_get( args, 1 );
  int patternLen = strlen(pattern->string), stringLen;

  /* For each string */

  for( liststring = lol_get( args, 0 ); liststring; liststring = liststring->next )
  {
    BUFFER buff;
    buffer_init( &buff );

    stringLen = strlen(liststring->string);
    if (stringLen >= patternLen &&
#ifdef OS_NT
        strnicmp(liststring->string,pattern->string,patternLen) == 0
#else
        strncasecmp(liststring->string,pattern->string,patternLen) == 0
#endif
        )
    {
      buffer_addstring(&buff,liststring->string+patternLen,stringLen-patternLen);
    }
    else {
      buffer_addstring(&buff,liststring->string,stringLen);
    }
    buffer_addchar( &buff, 0 );

    result = list_new( result, buffer_ptr( &buff ), 0 );
    buffer_free( &buff );
  }

  return result;
}

// compare list items
static int builtin_sort_casesensitive(const void* a, const void* b) {
    const LIST* aa = *(const LIST**)a;
    const LIST* bb = *(const LIST**)b;
    return strcmp(aa->string, bb->string);
}
static int builtin_sort_caseinsensitive(const void* a, const void* b) {
    const LIST* aa = *(const LIST**)a;
    const LIST* bb = *(const LIST**)b;
#ifdef OS_NT
    return stricmp(aa->string, bb->string);
#else
    return strcasecmp(aa->string, bb->string);
#endif
}

static LIST*
builtin_sort_ex(
    PARSE *parse,
    LOL *args,
    int *jmp,
    int (*item_compare)(const void *, const void *))
{
  LIST *l = lol_get(args, 0);
  LIST *output = L0;

  // gather all the elements in an array
  int i;
  int llen = list_length(l);
  LIST** listItems = (LIST**)malloc(sizeof(LIST*)*llen);
  for (i = 0; l; l = list_next(l), ++i) {
    listItems[i] = l;
  }

  // sort the strings
  qsort(listItems,llen,sizeof(LIST*),item_compare);

  // output the ordered items
  for (i = 0; i < llen; ++i) {
    output = list_new(output, listItems[i]->string, 0);
  }
  return output;
}

LIST *builtin_sort( PARSE *parse, LOL *args, int *jmp ) {
  return builtin_sort_ex(parse, args, jmp, builtin_sort_casesensitive);
}

LIST *builtin_sorti( PARSE *parse, LOL *args, int *jmp ) {
  return builtin_sort_ex(parse, args, jmp, builtin_sort_caseinsensitive);
}

LIST *
builtin_sha256_ex(
    PARSE *parse,
    LOL *args,
    int *jmp,
    int keyLen)
{
  LIST* input  = lol_get( args, 0 );
  LIST* result = L0;
  char  token[256];
  char  hexDigest[65] = {0};

  for ( ; input; input = input->next ) {
    const int len = strlen(input->string);
    sha256_easy_hash_hex(input->string, len, hexDigest);
    hexDigest[keyLen] = 0;
    result = list_new( result, hexDigest, 0 );
  }

  return  result;
}

LIST *builtin_sha256( PARSE *parse, LOL *args, int *jmp ) {
  return builtin_sha256_ex(parse, args, jmp, 64);
}
LIST *builtin_sha256_128( PARSE *parse, LOL *args, int *jmp ) {
  return builtin_sha256_ex(parse, args, jmp, 32);
}
LIST *builtin_sha256_64( PARSE *parse, LOL *args, int *jmp ) {
  return builtin_sha256_ex(parse, args, jmp, 16);
}

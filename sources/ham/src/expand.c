/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * expand.c - expand a buffer, given variable values
 *
 * External routines:
 *
 *	var_expand() - variable-expand input string into list of strings
 *
 * Internal routines:
 *
 *	var_edit_parse() - parse : modifiers into PATHNAME structure
 *	var_edit_file() - copy input target name to output, modifying filename
 *	var_edit_shift() - do upshift/downshift mods
 *
 * 01/25/94 (seiwald) - $(X)$(UNDEF) was expanding like plain $(X)
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 01/20/00 (seiwald) - Upgraded from K&R to ANSI C
 * 01/11/01 (seiwald) - added support for :E=emptyvalue, :J=joinval
 * 01/13/01 (seiwald) - :UDJE work on non-filename strings
 * 02/19/01 (seiwald) - make $($(var):J=x) join multiple values of var
 * 01/25/02 (seiwald) - fixed broken $(v[1-]), by ian godin
 * 10/22/02 (seiwald) - list_new() now does its own newstr()/copystr()
 * 11/04/02 (seiwald) - const-ing for string literals
 * 12/30/02 (armstrong) - fix out-of-bounds access in var_expand()
 * 29/04/08 (pierre)  - added :C that converts \ and / to the platform's path sep
 */

#include "jam.h"
#include "lists.h"
#include "variable.h"
#include "expand.h"
#include "pathsys.h"
#include "newstr.h"
#include "buffer.h"
#include "regexp.h"
#include "hash.h"

typedef struct {
  PATHNAME f;     /* :GDBSMR -- pieces */
  char parent;    /* :P -- go to parent directory */
  char filemods;  /* one of the above applied */
  char downshift; /* :L -- downshift result */
  char upshift;   /* :U -- upshift result */
  PATHPART empty; /* :E -- default for empties */
  PATHPART join;  /* :J -- join list with char */
  char fslash;    /* :/ -- convert all \ to / */
  char bslash;    /* :\ -- convert all / to \ */

} VAR_EDITS;

static void var_edit_parse(const char* mods, VAR_EDITS* edits);
static void var_edit_file(const char* in, BUFFER* buff, VAR_EDITS* edits);
static void var_edit_shift(char* out, VAR_EDITS* edits);
static void var_edit_slash(char* out, VAR_EDITS* edits);

struct hash* regexhash;

typedef struct {
  const char* name;
  regexp* re;
} regexdata;

#define MAGIC_COLON '\001'
#define MAGIC_LEFT '\002'
#define MAGIC_RIGHT '\003'

/*
 * var_expand() - variable-expand input string into list of strings
 *
 * Would just copy input to output, performing variable expansion,
 * except that since variables can contain multiple values the result
 * of variable expansion may contain multiple values (a list).  Properly
 * performs "product" operations that occur in "$(var1)xxx$(var2)" or
 * even "$($(var2))".
 *
 * Returns a newly created list.
 */

LIST* var_expand(LIST* l, const char* in, const char* end, LOL* lol,
                 int cancopyin)
{
  BUFFER buff;
  const char* inp = in;
  int depth;
  size_t save_buffer_pos, ov_save_buffer_pos;

  if (DEBUG_VAREXP)
    printf("expand '%.*s'\n", (int)(end - in), in);

  /* This gets alot of cases: $(<) and $(>) */

  if (end - in == 4 && in[0] == '$' && in[1] == '(' && in[3] == ')') {
    switch (in[2]) {
    case '1':
    case '<': return list_copy(l, lol_get(lol, 0));

    case '2':
    case '>': return list_copy(l, lol_get(lol, 1));
    }
  }

  buffer_init(&buff);

  /* Just try simple copy of in to out. */

  while (in < end) {
    char ch = *in++;
    buffer_addchar(&buff, ch);
    if (ch == '$' && *in == '(')
      goto expand;
  }

  /* No variables expanded - just add copy of input string to list. */

  /* Cancopyin is an optimization: if the input was already a list */
  /* item, we can use the copystr() to put it on the new list. */
  /* Otherwise, we use the slower newstr(). */

  buffer_putchar(&buff, 0);

  if (cancopyin) {
    LIST* new_list = list_new(l, inp, 1);
    buffer_free(&buff);
    return new_list;
  }
  else {
    LIST* new_list = list_new(l, buffer_ptr(&buff), 0);
    buffer_free(&buff);
    return new_list;
  }

expand:
  /*
	 * Input so far (ignore blanks):
	 *
	 *	stuff-in-outbuf $(variable) remainder
	 *			 ^	             ^
	 *			 in		     end
	 * Output so far:
	 *
	 *	stuff-in-outbuf $
	 *	^	         ^
	 *	out_buf          out
	 *
	 *
	 * We just copied the $ of $(...), so back up one on the output.
	 * We now find the matching close paren, copying the variable and
	 * modifiers between the $( and ) temporarily into out_buf, so that
	 * we can replace :'s with MAGIC_COLON.  This is necessary to avoid
	 * being confused by modifier values that are variables containing
	 * :'s.  Ugly.
	 */

  depth = 1;
  buffer_deltapos(&buff, -1);
  save_buffer_pos = buffer_pos(&buff);
  in++;

  while (in < end && depth) {
    char ch = *in++;
    buffer_addchar(&buff, ch);
    switch (ch) {
    case '(': depth++; break;
    case ')': depth--; break;
    case ':':
      buffer_deltapos(&buff, -1);
      buffer_addchar(&buff, MAGIC_COLON);
      break;
    case '[':
      buffer_deltapos(&buff, -1);
      buffer_addchar(&buff, MAGIC_LEFT);
      break;
    case ']':
      buffer_deltapos(&buff, -1);
      buffer_addchar(&buff, MAGIC_RIGHT);
      break;
    }
  }

  /* Copied ) - back up. */

  buffer_deltapos(&buff, -1);
  ov_save_buffer_pos = buffer_pos(&buff);
  buffer_setpos(&buff, save_buffer_pos);

  /*
	 * Input so far (ignore blanks):
	 *
	 *	stuff-in-outbuf $(variable) remainder
	 *			            ^        ^
	 *			            in       end
	 * Output so far:
	 *
	 *	stuff-in-outbuf variable
	 *	^	        ^       ^
	 *	out_buf         out	ov
	 *
	 * Later we will overwrite 'variable' in out_buf, but we'll be
	 * done with it by then.  'variable' may be a multi-element list,
	 * so may each value for '$(variable element)', and so may 'remainder'.
	 * Thus we produce a product of three lists.
	 */

  {
    LIST* variables = 0;
    LIST* remainder = 0;
    LIST* vars;

    /* Recursively expand variable name & rest of input */

    if (save_buffer_pos < ov_save_buffer_pos)
      variables = var_expand(L0, buffer_posptr(&buff),
                             buffer_ptr(&buff) + ov_save_buffer_pos, lol, 0);
    if (in < end)
      remainder = var_expand(L0, in, end, lol, 0);

    /* Now produce the result chain */

    /* For each variable name */

    for (vars = variables; vars; vars = list_next(vars)) {
      LIST *value, *evalue = 0;
      char* colon;
      char* bracket;
      BUFFER varnamebuff;
      int sub1 = 0, sub2 = -1;
      VAR_EDITS edits;
      memset(&edits, 0, sizeof(VAR_EDITS));

      /* Look for a : modifier in the variable name */
      /* Must copy into varname so we can modify it */

      buffer_init(&varnamebuff);
      buffer_addstring(&varnamebuff, vars->string, strlen(vars->string));
      buffer_addchar(&varnamebuff, 0);

      if (colon = strchr(buffer_ptr(&varnamebuff), MAGIC_COLON)) {
        *colon = '\0';
        var_edit_parse(colon + 1, &edits);
      }

      /* Look for [x-y] and [x-] subscripting */
      /* sub1 is x (0 default) */
      /* sub2 is length (-1 means forever) */

      if (bracket = strchr(buffer_ptr(&varnamebuff), MAGIC_LEFT)) {
        char* dash;

        if (dash = strchr(bracket + 1, '-'))
          *dash = '\0';

        sub1 = atoi(bracket + 1) - 1;

        if (!dash)
          sub2 = 1;
        else if (!dash[1] || dash[1] == MAGIC_RIGHT)
          sub2 = -1;
        else
          sub2 = atoi(dash + 1) - sub1;

        *bracket = '\0';
      }

      /* Get variable value, specially handling $(<), $(>), $(n) */

      {
        const char* varname = buffer_ptr(&varnamebuff);
        if (varname[0] == '<' && !varname[1])
          value = lol_get(lol, 0);
        else if (varname[0] == '>' && !varname[1])
          value = lol_get(lol, 1);
        else if (varname[0] >= '1' && varname[0] <= '9' && !varname[1])
          value = lol_get(lol, varname[0] - '1');
        else
          value = var_get(varname);
      }

      /* The fast path: $(x) - just copy the variable value. */
      /* This is only an optimization */

      if (buffer_isempty(&buff) && !bracket && !colon && in == end) {
        l = list_copy(l, value);
        buffer_free(&buff);
        continue;
      }

      /* Handle start subscript */

      while (sub1 > 0 && value)
        --sub1, value = list_next(value);

      /* Empty w/ :E=default? */

      if (!value && colon && edits.empty.ptr)
        evalue = value = list_new(L0, edits.empty.ptr, 0);

      /* For each variable value */

      for (; value; value = list_next(value)) {
        LIST* rem;
        size_t save_buffer_pos;
        size_t end_buffer_pos;
        const char* valuestring;

        /* Handle end subscript (length actually) */

        if (sub2 >= 0 && --sub2 < 0)
          break;

        /* Apply : mods, if present */

        save_buffer_pos = buffer_pos(&buff);

        valuestring = value->string;

        if (colon && edits.filemods) {
          var_edit_file(valuestring, &buff, &edits);
        }
        else {
          buffer_addstring(&buff, valuestring, strlen(valuestring) + 1);
        }
        buffer_setpos(&buff, save_buffer_pos);

        if (colon && (edits.upshift || edits.downshift))
          var_edit_shift(buffer_posptr(&buff), &edits);

        if (colon && (edits.fslash || edits.bslash))
          var_edit_slash(buffer_posptr(&buff), &edits);

        /* Handle :J=joinval */
        /* If we have more values for this var, just */
        /* keep appending them (with the join value) */
        /* rather than creating separate LIST elements. */

        if (colon && edits.join.ptr && (list_next(value) || list_next(vars))) {
          buffer_setpos(&buff,
                        buffer_pos(&buff) + strlen(buffer_posptr(&buff)));
          buffer_addstring(&buff, edits.join.ptr, strlen(edits.join.ptr) + 1);
          buffer_deltapos(&buff, -1);
          continue;
        }

        /* If no remainder, append result to output chain. */

        if (in == end) {
          l = list_new(l, buffer_ptr(&buff), 0);
          continue;
        }

        /* For each remainder, append the complete string */
        /* to the output chain. */
        /* Remember the end of the variable expansion so */
        /* we can just tack on each instance of 'remainder' */

        save_buffer_pos = buffer_pos(&buff);
        end_buffer_pos = strlen(buffer_ptr(&buff));
        buffer_setpos(&buff, end_buffer_pos);

        for (rem = remainder; rem; rem = list_next(rem)) {
          buffer_addstring(&buff, rem->string, strlen(rem->string) + 1);
          buffer_setpos(&buff, end_buffer_pos);
          l = list_new(l, buffer_ptr(&buff), 0);
        }

        buffer_setpos(&buff, save_buffer_pos);
      }

      /* Toss used empty */

      if (evalue)
        list_free(evalue);
    }

    /* variables & remainder were gifts from var_expand */
    /* and must be freed */

    if (variables)
      list_free(variables);
    if (remainder)
      list_free(remainder);

    if (DEBUG_VAREXP) {
      printf("expanded to ");
      list_print(l);
      printf("\n");
    }

    buffer_free(&buff);
    return l;
  }
}

/*
 * var_edit_parse() - parse : modifiers into PATHNAME structure
 *
 * The : modifiers in a $(varname:modifier) currently support replacing
 * or omitting elements of a filename, and so they are parsed into a
 * PATHNAME structure (which contains pointers into the original string).
 *
 * Modifiers of the form "X=value" replace the component X with
 * the given value.  Modifiers without the "=value" cause everything
 * but the component X to be omitted.  X is one of:
 *
 *	G <grist>
 *	D directory name
 *	B base name
 *	S .suffix
 *	M (member)
 *	R root directory - prepended to whole path
 *
 * This routine sets:
 *
 *	f->f_xxx.ptr = 0
 *	f->f_xxx.len = 0
 *		-> leave the original component xxx
 *
 *	f->f_xxx.ptr = string
 *	f->f_xxx.len = strlen( string )
 *		-> replace component xxx with string
 *
 *	f->f_xxx.ptr = ""
 *	f->f_xxx.len = 0
 *		-> omit component xxx
 *
 * var_edit_file() below and path_build() obligingly follow this convention.
 */

static void var_edit_parse(const char* mods, VAR_EDITS* edits)
{
  int havezeroed = 0;
  memset((char*)edits, 0, sizeof(*edits));

  while (*mods) {
    char* p;
    PATHPART* fp;

    switch (*mods++) {
    case 'L': edits->downshift = 1; continue;
    case 'U': edits->upshift = 1; continue;
    case 'P': edits->parent = edits->filemods = 1; continue;
    case 'E': fp = &edits->empty; goto strval;
    case 'J': fp = &edits->join; goto strval;
    case 'G': fp = &edits->f.f_grist; goto fileval;
    case 'R': fp = &edits->f.f_root; goto fileval;
    case 'D': fp = &edits->f.f_dir; goto fileval;
    case 'B': fp = &edits->f.f_base; goto fileval;
    case 'S': fp = &edits->f.f_suffix; goto fileval;
    case 'M': fp = &edits->f.f_member; goto fileval;
    case '/': edits->fslash = 1; continue;
    case '\\': edits->bslash = 1; continue;
    case MAGIC_COLON: continue;
    default: return; /* should complain, but so what... */
    }

fileval:

    /* Handle :CHARS, where each char (without a following =) */
    /* selects a particular file path element.  On the first such */
    /* char, we deselect all others (by setting ptr = "", len = 0) */
    /* and for each char we select that element (by setting ptr = 0) */

    edits->filemods = 1;

    if (*mods != '=') {
      int i;

      if (!havezeroed++)
        for (i = 0; i < 6; i++) {
          edits->f.part[i].len = 0;
          edits->f.part[i].ptr = "";
        }

      fp->ptr = 0;
      continue;
    }

strval:

    /* Handle :X=value, or :X */

    if (*mods != '=') {
      fp->ptr = "";
      fp->len = 0;
    }
    else if (p = strchr(mods, MAGIC_COLON)) {
      *p = 0;
      fp->ptr = ++mods;
      fp->len = (int)(p - mods);
      mods = p + 1;
    }
    else {
      fp->ptr = ++mods;
      fp->len = (int)(strlen(mods));
      mods += fp->len;
    }
  }
}

/*
 * var_edit_file() - copy input target name to output, modifying filename
 */

static void var_edit_file(const char* in, BUFFER* buff, VAR_EDITS* edits)
{
  PATHNAME pathname;
  char buf[MAXJPATH];

  /* Parse apart original filename, putting parts into "pathname" */

  path_parse(in, &pathname);

  /* Replace any pathname with edits->f */

  if (edits->f.f_grist.ptr)
    pathname.f_grist = edits->f.f_grist;

  if (edits->f.f_root.ptr)
    pathname.f_root = edits->f.f_root;

  if (edits->f.f_dir.ptr)
    pathname.f_dir = edits->f.f_dir;

  if (edits->f.f_base.ptr)
    pathname.f_base = edits->f.f_base;

  if (edits->f.f_suffix.ptr)
    pathname.f_suffix = edits->f.f_suffix;

  if (edits->f.f_member.ptr)
    pathname.f_member = edits->f.f_member;

  /* If requested, modify pathname to point to parent */

  if (edits->parent)
    path_parent(&pathname);

  /* Put filename back together */

  path_build(&pathname, buf, 0);
  buffer_addstring(buff, buf, strlen(buf) + 1);
}

/*
 * var_edit_shift() - do upshift/downshift mods
 */

static void var_edit_shift(char* out, VAR_EDITS* edits)
{
  /* Handle upshifting, downshifting now */

  if (edits->upshift) {
    for (; *out; ++out)
      *out = (char)toupper(*out);
  }
  else if (edits->downshift) {
    for (; *out; ++out)
      *out = (char)tolower(*out);
  }
}

/*
 * var_edit_slash() - do forward/backward slash mod
 */

static void var_edit_slash(char* out, VAR_EDITS* edits)
{
  /* Handle forward, backward slash modifications now */

  if (edits->fslash) {
    for (; *out; ++out)
      if (*out == '\\')
        *out = '/';
  }
  else if (edits->bslash) {
    for (; *out; ++out)
      if (*out == '/')
        *out = '\\';
  }
}

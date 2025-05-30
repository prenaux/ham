/*
 * Copyright 1993, 2000 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * headers.c - handle #includes in source files
 *
 * Using regular expressions provided as the variable $(HDRSCAN),
 * headers() searches a file for #include files and phonies up a
 * rule invocation:
 *
 *	$(HDRRULE) <target> : <include files> ;
 *
 * External routines:
 *    headers() - scan a target for include files and call HDRRULE
 *
 * Internal routines:
 *    headers1() - using regexp, scan a file and build include LIST
 *
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 09/10/00 (seiwald) - replaced call to compile_rule with evaluate_rule,
 *		so that headers() doesn't have to mock up a parse structure
 *		just to invoke a rule.
 * 03/02/02 (seiwald) - rules can be invoked via variable names
 * 10/22/02 (seiwald) - list_new() now does its own newstr()/copystr()
 * 11/04/02 (seiwald) - const-ing for string literals
 * 12/09/02 (seiwald) - push regexp creation down to headers1().
 */

#include "jam.h"
#include "lists.h"
#include "parse.h"
#include "compile.h"
#include "rules.h"
#include "variable.h"
#include "regexp.h"
#include "headers.h"
#include "newstr.h"
#include "hdrmacro.h"

#ifdef OPT_HEADER_CACHE_EXT
  #include "hcache.h"
#else
static LIST* headers1(const char* file, LIST* hdrscan);
#endif

/*
 * headers() - scan a target for include files and call HDRRULE
 */

#define MAXINC 25

void headers(TARGET* t)
{
  LIST* hdrscan;
  LIST* hdrrule;
  LOL lol;

  if (!(hdrscan = var_get("HDRSCAN")) || !(hdrrule = var_get("HDRRULE")))
    return;

  /* Doctor up call to HDRRULE rule */
  /* Call headers1() to get LIST of included files. */

  if (DEBUG_HEADER)
    printf("header scan %s\n", t->name);

  lol_init(&lol);

  lol_add(&lol, list_new(L0, t->name, 1));
#ifdef OPT_HEADER_CACHE_EXT
  lol_add(&lol, hcache(t, hdrscan));
#else
  lol_add(&lol, headers1(t->boundname, hdrscan));
#endif

  if (lol_get(&lol, 1))
    list_free(evaluate_rule(hdrrule->string, &lol, L0));

  /* Clean up */

  lol_free(&lol);
}

/*
 * headers1() - using regexp, scan a file and build include LIST
 */

#ifdef OPT_HEADER_CACHE_EXT
LIST *
#else
static LIST*
#endif
headers1(const char *file, LIST *hdrscan)
{
  FILE* f;
  int i;
  int rec = 0;
  LIST* result = 0;
  regexp* re[MAXINC];
  regexp* re_macros;
  char buf[1024];

  if (!(f = fopen(file, "r")))
    return result;

  var_set("HDROPENED", list_new(L0, file, 0), VAR_SET);

  while (rec < MAXINC && hdrscan) {
    re[rec++] = regcomp(hdrscan->string);
    hdrscan = list_next(hdrscan);
  }

  /* the following regexp is used to detect cases where a  */
  /* file is included through a line line "#include MACRO" */
  re_macros =
    regcomp("^[ 	]*#[ 	]*include[ 	]*([A-Za-z][A-Za-z0-9_]*).*$");

  while (fgets(buf, sizeof(buf), f)) {
    for (i = 0; i < rec; i++)
      if (regexec(re[i], buf) && re[i]->startp[1]) {
        /* Copy and terminate extracted string. */

        char buf2[MAXSYM];
        int l = re[i]->endp[1] - re[i]->startp[1];
        memcpy(buf2, re[i]->startp[1], l);
        buf2[l] = 0;
        result = list_new(result, buf2, 0);

        if (DEBUG_HEADER)
          printf("header found: %s\n", buf2);
      }

    /* special treatment for #include MACRO */
    if (regexec(re_macros, buf) && re_macros->startp[1]) {
      const char* header_filename;
      char buf2[MAXSYM];
      int l = re_macros->endp[1] - re_macros->startp[1];

      memcpy(buf2, re_macros->startp[1], l);
      buf2[l] = 0;

      if (DEBUG_HEADER)
        printf("macro header found: %s", buf2);

      header_filename = macro_header_get(buf2);
      if (header_filename) {
        if (DEBUG_HEADER)
          printf(" resolved to '%s'\n", header_filename);
        result = list_new(result, header_filename, 0);
      }
      else {
        if (DEBUG_HEADER)
          printf(" ignored !!\n");
      }
    }
  }

  free(re_macros);

  while (rec)
    free((char*)re[--rec]);

  fclose(f);

  return result;
}

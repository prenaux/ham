/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * execcmd.h - execute a shell script
 *
 * 05/04/94 (seiwald) - async multiprocess interface
 */

void execcmd(const char* string,
             void (*func)(void* closure, int status, const char* outputname),
             void* closure, const char* shellPath, int serialOutput);

int execwait();

void exec_init();
void exec_done();

#define EXEC_CMD_OK 0
#define EXEC_CMD_FAIL 1
#define EXEC_CMD_INTR 2

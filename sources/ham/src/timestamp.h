/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * timestamp.h - get the timestamp of a file or archive member
 */

void timestamp(char* target, time_t* time);
void donestamps();
const char* format_timestamp(const time_t* time, char* buffer,
                             size_t buffer_size);

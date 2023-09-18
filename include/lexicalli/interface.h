// interface.h
// Passthrough functions from main cpp program to Haskell
// in C because no current Haskell implementation can handle name mangling of stubs

#ifndef INTERFACE_H
#include <stdio.h>
#include "lexical.h"

#define INTERFACE_H

extern char *top_down_parse_in;

char get_char(int off);

void token_scanner();

#endif // INTERFACE_H
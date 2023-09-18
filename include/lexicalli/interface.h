// interface.h
// Passthrough functions from main cpp program to Haskell
// in C because no current Haskell implementation can handle name mangling of stubs

#ifndef INTERFACE_H
#include <stdio.h>
#include "lexical.h"

#define INTERFACE_H

extern char *scanner_input;

// Get the next character from the input file
char get_char();

// Place a symbol, its program segment, value, and location in the segment into the output file
int put_token(char *symbol, int token_class);

void scanner_init();
void scanner_release();

#endif // INTERFACE_H
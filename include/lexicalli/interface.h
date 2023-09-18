// interface.h
// Passthrough functions from main cpp program to Haskell
// in C because no current Haskell implementation can handle name mangling of stubs

#ifndef INTERFACE_H
#define INTERFACE_H

extern char *scanner_input;
extern char *scanner_output;

// Get the next character from the input file
char get_char();

// Place a symbol, its program segment, value, and location in the segment into the output file
void put_token(char *symbol, int token_class);

void scanner_init();
void scanner_release();

#endif // INTERFACE_H
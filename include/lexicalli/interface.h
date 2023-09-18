// interface.h
// Passthrough functions from main cpp program to Haskell
// in C because no current Haskell implementation can handle name mangling of stubs
// It should be said that I absolutely adore the ability to define extern chars
//  and then modify them in the files including them, it's like global customization flags
// I realize that they are global variables in any file that includes this one, 
//  and global variables are evil,
//  but the way C handles them is so elegant I can't resist

#include "lexical.h"

#ifndef INTERFACE_H
#define INTERFACE_H

// Scanner automaton interfacers
extern char *scanner_input; // Input file from command line
extern char *scanner_output; // Ideally will be (input file name)_(input file extension).lex
// Get the next character from the input file
char get_char();
// Place a symbol, its program segment, value, and location in the segment into the output file
void put_token(char *symbol, int token_class);
void scanner_init();
void scanner_release();

// Symbol table automaton interfacers
extern char *symbol_input; // Scanner output file name, typically 
extern char *symbol_output;
struct Token_t* next_token();
void symbol_init();
void symbol_release();

#endif // INTERFACE_H
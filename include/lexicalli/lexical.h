// lexical.h
// Common core declarations for lexical analyzer

#ifndef LEXICAL_H
#define LEXICAL_H

enum Token_t;
enum Symbol_t;
enum Segment_t;
struct Token;
struct Symbol;

/*
// Preliminary token classes
// Potentially unnecessary
enum Prelim_t
{
  QIDENT, // Starts with letter and contains letters+numbers
  QLIT, // Starts with quotes and stays until an end quote is found
  QNUM, // Starts with number and contains numbers only (char in here before delim is err)
  OP, // One of some specific tokens, could go to QIDENT/QLIT/QNUM without whitespace
  WHITESPACE, // Reset state guaranteed
  DELIM // Semicolon
};
*/

// Token classes
enum Token_t 
{ 
  XCLASS = -1, XVAR = -2, XCONST = -3, IDENT = 6, CLASS = 3, VAR = 7, CONST = 5
  INTEGER = 4,
  ASSIGN = 12, ADDOP = 21, MOP = 10, RELOP = 13, 
  LB = 25, RB = 27, COMMA = 29, SEMI = 17,
};

// Symbol table classes
enum Symbol_t 
{ 
  SSUB = 1, SVAR = 2, SCONST = 3, SNUM_LIT = 4
};
// Symbol table segments
enum Segment_t { CODE_SEGMENT = 0, DATA_SEGMENT = 1 };

struct Token
{
  char *token;
  enum Token_t tok_t;
};

struct Symbol
{
  char *token;
  enum Symbol_t sym_t;
  int *value;
  int address;
  enum Segment_t segment;
};  

#endif // LEXICAL_H
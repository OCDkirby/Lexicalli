// lexical.h
// Common core declarations for lexical analyzer

#ifndef LEXICAL_H
#define LEXICAL_H

enum Token_Class;
enum Symbol_Class;
enum Segment;
struct Token_t;
struct Symbol_t;

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
enum Token_Class
{ 
  XCLASS = -1, XVAR = -2, XCONST = -3, IDENT = 6,
  INTEGER = 4,
  ASSIGN = 12, ADDOP = 21, MOP = 10, RELOP = 13, 
  LB = 25, RB = 27, COMMA = 29, SEMI = 17,
};

// Symbol table classes
enum Symbol_Class
{ 
  SSUB = 1, SVAR = 2, SCONST = 3, SNUM_LIT = 4
};
// Symbol table segments
enum Segment { CODE_SEGMENT = 0, DATA_SEGMENT = 1 };

struct Token_t
{
  char *name;
  enum Token_Class tok_class;
};

struct Symbol_t
{
  char *name;
  enum Symbol_Class sym_class;
  char *value;
  int address;
  enum Segment segment;
};  

#endif // LEXICAL_H
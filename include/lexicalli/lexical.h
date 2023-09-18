// lexical.h
// Common core declarations for lexical analyzer

#ifndef LEXICAL_H
#define LEXICAL_H

// Token classes
enum Token_Class
{ 
  XCLASS = -1, XVAR = -2, XCONST = -3, IDENT = 6, 
  IF = -4, THEN = -5, XPROC = -6, WHILE = -7, DO = -8, CALL = -9, ODD = -10,
  INTEGER = 4,
  ASSIGN = 12, ADDOP = 21, MOP = 10, RELOP = 13, 
  LB = 25, RB = 27, COMMA = 29, SEMI = 17,
};

// Symbol table classes
enum Symbol_Class
{ 
  SCLASS = 1, SVAR = 2, SCONST = 3, SNUM_LIT = 4, SPROC = 5
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
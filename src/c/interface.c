// interface.c
// Implementations for the C functions that interface with Haskell

#include <stdio.h>
#include "lexicalli/interface.h"

char *scanner_input;
FILE *in_file;
FILE *pass_1_output;

void scanner_init()
{
  in_file = fopen(scanner_input, "r");
  pass_1_output = fopen("tokens.lex", "w");
  // TODO more here
}

void scanner_release()
{
  if(in_file)
    fclose(in_file);
  if(pass_1_output)
    fclose(pass_1_output);
}

char get_char()
{
  int i = 0;
  int c;
  if(in_file != NULL)
  {
    c = fgetc(in_file);
  } 
  else
    return EOF;
  return (char) c;
}

void put_token(char *symbol, int token_class)
{
  fprintf(pass_1_output, "%s %d\n", symbol, token_class);
}

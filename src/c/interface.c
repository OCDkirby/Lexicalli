// interface.c
// Implementations for the C functions that interface with Haskell

#include <stdio.h>
#include <stdlib.h>
#include "lexicalli/lexical.h"
#include "lexicalli/interface.h"

char *scanner_input, *scanner_output;
FILE *scanner_infile, *scanner_outfile;

char *symbol_input, *symbol_output;
FILE *symbol_infile, *symbol_outfile;

void scanner_init()
{
  scanner_infile = fopen(scanner_input, "r");
  scanner_outfile = fopen(scanner_output, "w");
  // TODO more here
}

void scanner_release()
{
  if(scanner_infile)
    fclose(scanner_infile);
  if(scanner_outfile)
    fclose(scanner_outfile);
}

char get_char()
{
  int i = 0;
  int c;
  if(scanner_infile != NULL)
  {
    c = fgetc(scanner_infile);
  } 
  else
    return EOF;
  return (char) c;
}

void put_token(char *symbol, int token_class)
{
  fprintf(scanner_outfile, "%s %d\n", symbol, token_class);
}

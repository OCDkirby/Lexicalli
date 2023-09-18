// interface.c
// Implementations for the C functions that interface with Haskell

#include <stdio.h>
#include "lexicalli/interface.h"

char *top_down_parse_in;
FILE *file;

void token_scanner()
{
  file = fopen(top_down_parse_in, "r");
  // TODO more here
}

char get_char(int skip)
{
  int i = 0;
  int c;
  if(file != NULL)
  {
    while(i <= skip)
    {
      c = fgetc(file);
      if(c == EOF) // Offset goes past the end of the file
        return c;
      ++i;
    }
  } 
  else
    return EOF;
  return (char) c;
}

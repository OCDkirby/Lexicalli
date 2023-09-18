#include <stdio.h>
#include "lexicalli/interface.h"

char *top_down_parse_in;
FILE *file;

void token_scanner()
{
  file = fopen(top_down_parse_in, "r");
  /*if(file != NULL)
  {
    char c;
    while((int) (c = get_char(0)) != EOF)
      printf("%c", c);
    printf("\n");
    fclose(file);
  }
  else
    printf("File not found.");*/
}

char get_char(int off)
{
  int i = 0;
  int c;
  if(file != NULL)
  {
    while(i <= off)
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

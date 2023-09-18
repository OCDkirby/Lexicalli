// test.c
// Initial testing of the Haskell portions of the analyzer and main routine
// Will be ported to C++ at a later date

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "HsFFI.h" // Foreign Function Interface, part of the Haskell standard
#include "lexicalli/interface.h"
#include "lexicalli/lexical.h"

// For use with the GHC Haskell implementation
#ifdef __GLASGOW_HASKELL__
#include "stubs/Scanner_stub.h"
#include "stubs/Safe_stub.h"
#endif

static char no_value[] = "$$$$";

char* format_output(char* input, char *extension)
{
  char *out = malloc(strlen(input) + 5); // length of inputname_inputextension.lex\0
  strcpy(out, input);
  char *dot = strchr(out, '.');
  *dot = '_';
  strcat(out, ".lex");
  return out;
}

//int mem_offset(Token_t token_class);

int main(int argc, char *argv[])
{
  char usage[188] = "Usage: %s file [-v, --verbose]\nfile: relative or absolute path to your input file, will have its tokens output in name_extension.lex format.\nv/verbose: prints additional debug output.\n";
  
  // Get arguments and create temp file names
  if(argc != 2)
  {
    printf(usage, argv[0]);
    return 1;
  }
  scanner_input = argv[1];
  scanner_output = format_output(scanner_input, ".lex");
  
  // Create token list
  // Initialize the Haskell env
  hs_init(&argc, &argv);
  scanner_init(); // Set up the environment for Haskell
  
  // Run scanner on the file
  run_scanner();
  
  scanner_release(); // Close intermediates
  hs_exit(); // Release Haskell 
  
  // Create symbol table
  /*
  int error = 0;
  int csp = 0; // Code segment pointer
  int dsp = 0; // Data segment pointer
  FILE *token_file = fopen("scanner_output", "r");
  FILE *symbol_file = fopen(format_output(scanner_input, ".sym"), "w");
  char *token;
  int token_id;
  
  
  while(!error && fscanf(token_file, "%s %d\n", token, token_id) != EOF)
  {
    Token_t token_class = (Token_t) token_id;
    
    if(token_class == XCLASS) // Next token is a subroutine name
    {
      char *procedure_name;
      int procedure_id;
      fscanf(token_file, "%s %d\n", procedure_name, prodedure_id);
      Token_t procedure_class = (Token_t) procedure_id;
      if(procedure_class == IDENT)
      {
        fprintf(symbol_file, "%s %d %s %d %d\n", procedure_name, procedure_id, no_value, csp, CODE_SEGMENT);
      }
      else
        error = 1; // Error: Expected identifier
    }
    else if(token_class == XVAR)
    
    csp += mem_offset(token_class);
  }*/
  
  
  return 0;
}
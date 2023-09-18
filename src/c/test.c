// test.c
// Initial testing of the Haskell portions of the analyzer and main routine
// Will be ported to C++ at a later date

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "HsFFI.h" // Foreign Function Interface, part of the Haskell standard
#include "lexicalli/interface.h"

// For use with the GHC Haskell implementation
#ifdef __GLASGOW_HASKELL__
#include "stubs/Lexical_stub.h"
#include "stubs/Safe_stub.h"
#endif

char* format_output(char* input)
{
  char *out = malloc(strlen(input) + 5); // length of inputname_inputextension.lex\0
  strcpy(out, input);
  char *dot = strchr(out, '.');
  *dot = '_';
  strcat(out, ".lex");
  return out;
}

int main(int argc, char *argv[])
{
  char usage[188] = "Usage: %s file [-v, --verbose]\nfile: relative or absolute path to your input file, will have its tokens output in name_extension.lex format.\nv/verbose: prints additional debug output.\n";
  
  // Get arguments and create temp file names
  // Use my personal arg parsing library to conveniently format args
  if(argc != 2)
  {
    printf(usage, argv[0]);
    return 1;
  }
  
  // Create token list
  // Initialize the Haskell env
  hs_init(&argc, &argv);
  scanner_input = argv[1]; // Set the file name to open
  scanner_output = format_output(scanner_input);
  scanner_init(); // Set up the environment for Haskell
  
  // Run scanner on the file
  run_scanner();
  
  scanner_release(); // Close intermediates
  hs_exit(); // Release Haskell 
  // Release allocated heap memory
  //free(scanner_output);
  //free(flags_out);
  //free(anons);
  
  // Create symbol table
  
  return 0;
}
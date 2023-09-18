// lexicalli.c
// Main program body, calls Haskell portions of the analyzer

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "HsFFI.h" // Foreign Function Interface, part of the Haskell standard
#include "lexicalli/interface.h"
#include "lexicalli/lexical.h"

// For use with the GHC Haskell implementation
#ifdef __GLASGOW_HASKELL__
#include "stubs/Scanner_stub.h"
#endif

FILE *inf, *of;
int csp = 0, dsp = 0;

// Change a file name string from name.originalextension to name_originalextension.newextension
char* format_output(char* input, char *extension);

// Provide the offset in the symbol table memory locations
int mem_offset(enum Symbol_Class cls);

// Write a symbol to the table
void put_symbol(struct Symbol_t sym);

// Find and extract symbols from the token list
int program_symbol_check();
int var_symbol_check();
int const_symbol_check();

int main(int argc, char **argv)
{
  char usage[188] = "Usage: %s file\nfile: relative or absolute path to your input file, will have its tokens output in name_extension.lex format.\n";
  
  // Initialize the Haskell env
  hs_init(&argc, &argv);
  
  // Get arguments
  // Will be replaced with a much more elegant solution in the final compiler
  if(argc != 2)
  {
    printf(usage, argv[0]);
    return 1;
  }
  // Create token list
  scanner_input = argv[1];
  scanner_output = format_output(scanner_input, ".lex");
  scanner_init(); // Set up the environment for Haskell
  run_scanner(); // (Haskell) Run scanner on the input file
  scanner_release(); // Close intermediates
  hs_exit(); // Release Haskell 
  
  // Create symbol table
  printf("Lexicalli: Starting symbol table generation\n");
  char *symbol_output = format_output(argv[1], ".sym");
  inf = fopen(scanner_output, "r");
  of = fopen(symbol_output, "w");
  struct Token_t token;
  char name[128]; // I recognize this is bad design.
  int cls;
  int error = 0;
  
  while(!error && fscanf(inf, "%s %d\n", name, &cls) != EOF)
  {
    token.name = name;
    token.tok_class = cls;
    //printf("%s %d\n", name, cls);
    // Symbol table checks
    if(token.tok_class == XCLASS)
    {
      error = program_symbol_check();
    }
    else if(token.tok_class == XVAR)
    {
      error = var_symbol_check();
    }
    else if(token.tok_class == XCONST)
    {
      error = const_symbol_check();
    }
    else if(token.tok_class == INTEGER)
    {
      // Place in symbol table
      char symbol[strlen(token.name) + 4]; // INT[token.name]\0
      strcpy(symbol, "INT");
      strcat(symbol, token.name);
      
      put_symbol((struct Symbol_t) { .name = symbol, .sym_class = SNUM_LIT, .value = token.name, .address = dsp, .segment = DATA_SEGMENT });
    }
  }
  
  if(error)
    printf("Lexicalli: Symbol Table Failure\n"); // Do error checking here
  else
    printf("Lexicalli: Symbol Table Success\n");
  
  fclose(inf);
  fclose(of);
  free(scanner_output);
  free(symbol_output);
    
  printf("Lexicalli: Exit\n");
  return 0;
}

char* format_output(char* input, char *extension)
{
  char *out = malloc(strlen(input) + 5); // length of inputname_inputextension.lex\0
  strcpy(out, input);
  char *dot = strchr(out, '.');
  *dot = '_';
  strcat(out, extension);
  return out;
}

int mem_offset(enum Symbol_Class cls)
{
  if(cls == SSUB)
    return 8; // Size of class signature
  else if(cls == SVAR || cls == SCONST || cls == SNUM_LIT)
    return 4; // Size of integer
}

void put_symbol(struct Symbol_t sym)
{
  fprintf(of, "%s %d %s %d %d\n", sym.name, sym.sym_class, sym.value, sym.address, sym.segment);
  if(sym.segment == CODE_SEGMENT)
    csp += mem_offset(sym.sym_class);
  else
    dsp += mem_offset(sym.sym_class);
}

int program_symbol_check()
{
  char name[128];
  int cls;
  if(fscanf(inf, "%s %d\n", name, &cls) != EOF)
  {
    if(cls == IDENT)
    {
      put_symbol((struct Symbol_t) { .name = name, .sym_class = SSUB, .value = "-", .address = csp, .segment = CODE_SEGMENT });
      return 0;
    }
    else
      return 1; // Wrong token after CLASS
  }
  return EOF;
}

int var_symbol_check()
{
  char name[128];
  char val[128];
  int cls;
  while(fscanf(inf, "%s %d\n", name, &cls) != EOF)
  {
    if(cls == COMMA)
      continue;
      
    if(cls == IDENT)
    {
      if(fscanf(inf, "%s %d\n", val, &cls) != EOF)
      {
        if(cls == ASSIGN)
        {
          if(fscanf(inf, "%s %d\n", val, &cls) != EOF)
          {
            if(cls == INTEGER)
            {
              put_symbol((struct Symbol_t) { .name = name, .sym_class = SVAR, .value = val, .address = dsp, .segment = DATA_SEGMENT });
            }
            else if(cls == IDENT) // That's a semantic analyzer's job
            {
              put_symbol((struct Symbol_t) { .name = name, .sym_class = SVAR, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
            } 
            else
              return 3; // Expected numeric literal or expression
          }
        }
        else if(cls == COMMA)
        {
          put_symbol((struct Symbol_t) { .name = name, .sym_class = SVAR, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
        }
        else if(cls == SEMI)
        {
          put_symbol((struct Symbol_t) { .name = name, .sym_class = SVAR, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
          return 0; // Finished statement
        }
        else
          return 4; // Expected assignment operator, comma, or semicolon
      } 
    }
    else if(cls == SEMI)
      return 0; // Finished statement
    else
      return 2; // Expected identifier
  }
  return EOF;
}

int const_symbol_check()
{
  char name[128];
  char val[128];
  int cls;
  while(fscanf(inf, "%s %d\n", name, &cls) != EOF)
  {
    if(cls == COMMA)
      continue;
      
    if(cls == IDENT)
    {
      if(fscanf(inf, "%s %d\n", val, &cls) != EOF)
      {
        if(cls == ASSIGN)
        {
          if(fscanf(inf, "%s %d\n", val, &cls) != EOF)
          {
            if(cls == INTEGER)
            {
              put_symbol((struct Symbol_t) { .name = name, .sym_class = SCONST, .value = val, .address = dsp, .segment = DATA_SEGMENT });
            }
            else if(cls == IDENT) // That's a semantic analyzer's job
            {
              put_symbol((struct Symbol_t) { .name = name, .sym_class = SCONST, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
            } 
            else
              return 3; // Expected numeric literal or expression
          }
        }
        else if(cls == COMMA)
        {
          put_symbol((struct Symbol_t) { .name = name, .sym_class = SCONST, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
        }
        else if(cls == SEMI)
        {
          put_symbol((struct Symbol_t) { .name = name, .sym_class = SCONST, .value = "-", .address = dsp, .segment = DATA_SEGMENT });
          return 0; // Finished statement
        }
        else
          return 4; // Expected assignment operator, comma, or semicolon
      } 
    }
    else if(cls == SEMI)
      return 0; // Finished statement
    else
      return 2; // Expected identifier
  }
  return EOF;
}

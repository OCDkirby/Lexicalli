// test.c
// Initial testing of the Haskell portions of the analyzer and main routine
// Will be ported to C++ at a later date

#include <stdio.h>
#include "HsFFI.h" // Foreign Function Interface, part of the Haskell standard
#include "kirbparse.h"
#include "lexicalli/interface.h"

// For use with the GHC Haskell implementation
#ifdef __GLASGOW_HASKELL__
#include "stubs/Lexical_stub.h"
#include "stubs/Safe_stub.h"
#endif

int main(int argc, char *argv[])
{
  printf("Starting up lexical analyzer\n");
  
  // Initialize the Haskell env
  hs_init(&argc, &argv);
  top_down_parse_in = "testo.txt"; // Set the file name to open
  token_scanner(); // Set up the environment for Haskell
  run();
  /*
    int i;

    // Call the foreign function
    i = fibonacci_hs(6);
    printf("Fibonacci: %d\n", i);
  */
  hs_exit(); // Release Haskell 
  return 0;
}
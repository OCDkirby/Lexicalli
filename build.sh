#!/usr/bin/bash

ghc -c -O src/haskell/** -outputdir tmp -stubdir include/stubs -Iinclude && \
ghc --make src/c/* src/haskell/* -no-hs-main -outputdir tmp -Iinclude -o bin/calli
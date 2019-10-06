#!/bin/sh
which -s stack || (curl -sSL https://get.haskellstack.org/ | sh && stack setup)
mkdir -p .shake
stack build shake safe Cabal cabal-install
stack ghc -- --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && stack exec .shake/build -- "$@"

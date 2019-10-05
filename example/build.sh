#!/bin/sh
# Precondition: stack install shake # shake-0.18.3
mkdir -p .shake
stack ghc --package safe -- --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake/build "$@"

# require("google-closure-library");
# goog.require('goog.math.Long');
# goog.require('goog.crypt.Md5');
# goog.require('goog.crypt.Hash');

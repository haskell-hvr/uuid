#!/bin/sh

cd $1

cabal configure --enable-tests --enable-benchmarks -v2
cabal build
cabal test
cabal check
cabal sdist
export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
cd dist/;
if [ -f "$SRC_TGZ" ]; then
  cabal install --force-reinstalls "$SRC_TGZ";
else
  echo "expected '$SRC_TGZ' not found";
  exit 1;
fi

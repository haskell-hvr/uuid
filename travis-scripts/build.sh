#!/usr/bin/env sh
echo "==========================================="
echo "= Building and installing 'uuid-types'... ="
echo "==========================================="

# We install here because "uuid" is going to need it.
(cd "uuid-types" && \
        cabal configure --enable-tests && \
        cabal build && \
        cabal test && \
        cabal install) || exit 1

echo "========================================="
echo "= Installing dependencies for 'uuid'... ="
echo "========================================="

(cd "uuid" && \
        cabal install --dependencies-only --enable-tests) || exit 1

echo "======================"
echo "= Building 'uuid'... ="
echo "======================"

(cd "uuid" && \
        cabal configure --enable-tests && \
        cabal build && \
        cabal test) || exit 1

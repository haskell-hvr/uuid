#!/usr/bin/env sh

# We can only pre-install dependencies for the "uuid-types" project
# since the "uuid" project depends on "uuid-types". We could use
# sandboxing and "add-source", but it seems quite wasteful of
# CPU-cycles to compile everything twice for each version of GHC.

echo "==============================================="
echo "= Installing dependencies for 'uuid-types'... ="
echo "==============================================="

(cd "uuid-types" && \
        cabal install --dependencies-only --enable-tests) || exit 1

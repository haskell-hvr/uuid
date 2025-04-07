1.3.17

- Added support for UUIDv7 in `Data.UUID.V7`.

1.3.16

- Support GHC-8.6.5...9.10.1

1.3.15

- Add (Template Haskell) `Lift UUID` instance

1.3.14

- Use more compact heap object representation which saves 16 bytes on 64bit platforms.
- Add `toWords64`/`fromWords64` functions

1.3.13

- Optimize `V4.nextRandom` (~3x speed increase)
- Optimize UUID V3 & V5 generation (~2x speed increase)
- Use `cryptohash-md5`/`cryptohash-sha1`/`entropy` instead
  of `memory`/`cryptonite` for better performance and stability,
  but GHCJS is now no longer supported.
- Update cabal-spec to version 1.10

1.3.12

- Update package dependencies
- Use `cryptonite` for crypto
- The function `V4.nextRandom` is now implemented with functions from
  `Crypto.Random` in package `cryptonite`. This does slow-down random
  UUID generation but provides for far greater randomness. The
  `Random` instance for `UUID` can be used to re-gain the old
  behavior.

1.3.11

- Add `toText`/`fromText`

1.3.10

- Update dependencies in tests and benchmarks.

1.3.9

- Split definition of UUID data type into separate Cabal package to
  enable 3rd party libraries to avoid some of the larger dependencies.

1.3.8

- Allow building against newer `deepseq`.

1.3.7

- Allow building against newer `time`.

1.3.6

- Move to GitHub.

1.3.5

- Allow building against newer `random`.

1.3.4

- Allow building against newer `QuickCheck`.

1.3.3

- More complex version constraints on `hashable`, to avoid building against
  versions less than 1.1.1.0 or equal to 1.2.0.*.

1.3.2

- Fix for building against `bytestring` 0.9.*

1.3.1

- Allow building against `hashable` 1.1.* in addition to 1.2.*

1.3.0

- New functions for parsing and printing UUIDs to and from ASCII BytesStrings
- New module `Data.UUID.Util`. This module includes the type `UnpackedUUID`,
  whose fields correspond to the UUID fields described in RFC 4122.
- The `Storable` instance now stores a UUID in host byte-order instead of
  big endian.
- There is now an instance for `Hashable UUID`.

1.2.13

Benchmark only changes:

- Allow `criterion` 0.8.*

1.2.12

Test only changes:

- Allow `QuickCheck` 2.6.*

1.2.11

- Allow `binary` 0.7.*

1.2.10

- Allow `cryptohash` version 0.9.*
- Cleanup tests

1.2.9

- Bumped dependency on `cryptohash`.

1.2.8

- Bumped various dependencies and cleaned up dependencies in general.

1.2.7

- Added stricter constraints on `random` package.

1.2.6

- Add module `V4` to direct attention to our Random instance

- In module `V1` seed the generator with a random number
  if the hardware MAC address could not be discovered.

- Fix and cleanup various haddocks.

- In module docs, warn about MD5 use in Data.UUID.V3 and
  encourage the reader to use Data.UUID.V5 instead.

1.2.5

- Use `cryptohash` package for MD5 and SHA1 instead of `Crypto`

1.2.4

- Unpack Word32 values into UUID constructor.

- Update test suite to QuickCheck 2

- Bump other dependencies in tests/benchmarks

1.2.3

- The Read instance now drops leading spaces in the string to be parsed.
Thanks to Marc Ziegert for reporting this bug.

- The tests have moved over to the new Cabal test running framework.

1.2.2

- Add functions fromWords/toWords

The goal was to have a total function that can be used to construct
a UUID, primarily for use by uuid-th.

1.2.1

- Fix concurrency bug in Data.UUID.V1 (thanks to Neil Mitchell for reporting
and a test case)

1.2.0
  (Contributors: Antoine Latter & Mark Lentczner)

- added functions toByteString and fromByteString
- added `nil` UUID
- added unit tests and benchmarks, built when configured -ftest
- major speed up of to/from functions (as well as in general)
- added version-3 generation (deterministic based on MD5)
- major changes to internal representation
	- now uses four strict Word32 values
	- internal ByteSource classes for easy construction (see Builder.hs)
- Storable instance now stores in memory as system libraries in C do:
    16 bytes derived from the network order of the fields, no matter what
    the host native endianess is.
- fixed bugs in V1 time and clock stepping, and V1 generated values
- builds cleanly under GHC's -Wall
- added CHANGES file

1.1.1

- no longer exporting `null` from the prelude
- add `null` predicate on UUIDs
- documentation fix (thanks Mark Lentczner)

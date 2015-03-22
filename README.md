# uuid  [![Build Status](https://travis-ci.org/aslatter/uuid.svg?branch=master)](https://travis-ci.org/aslatter/uuid)

This library is useful for creating, comparing, parsing and printing Universally
Unique Identifiers. See http://en.wikipedia.org/wiki/UUID for the general idea.

We offer two different packages: `uuid` and `uuid-types`

The `uuid` package aims to be a fully-featured library for working with UUIDs.

However if you're developing a library and only want the `UUID` type to show up
in your APIs you may want to use the `uuid-types` library to minimize the number
of dependencies needed by users of your library.

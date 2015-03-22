# Versioning

Because many consumers of the `UUID` type receive the type
through the `uuid` package and not the `uuid-types` package
we need to be careful about removing instances from the `uuid`
type.

If the `uuid-types` package has a major-version bump because of
the removal of a class instance then we must also bump the major
version of the `uuid` package when we update the package
description to pull in the new version of `uuid-types`.

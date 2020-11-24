# Enums

This is an implementation of [SRFI 209](https://srfi.schemers.org/srfi-209),
which provides enumerated types for Scheme.

# Requirements

This library should be portable to any R7RS-small implementation
with the following libraries from R7RS-large:

* `(scheme list)` ([SRFI 1](https://srfi.schemers.org/srfi-1))
* `(scheme comparator)` ([SRFI 128](https://srfi.schemers.org/srfi-128)
  with or without [SRFI 162](https://srfi.schemers.org/srfi-162))
* `(scheme mapping)` ([SRFI 146](https://srfi.schemers.org/srfi-146))
* `(scheme bitwise)` ([SRFI 151](https://srfi.schemers.org/srfi-151))

[SRFI 145](https://srfi.schemers.org/srfi-145) is an optional
dependency.  [SRFI 78](https://srfi.schemers.org/srfi-78) is used for
the test set, which can be run simply by loading `test.scm`.  (A basic
fallback framework is provided for Schemes without SRFI 78.)  A copy
of the test suite for [chibi-scheme](http://synthcode.com/scheme/chibi/),
using the `(chibi test)` module.

# Implementation notes

Enum-sets are implemented as integer bitmaps.  Though this gives
excellent performance in the case of small sets (e.g. those which
can be represented by a single fixnum), large sets may perform
less well.

# Implementation author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy minus the zy

# License

This is free software released under the MIT/X license.  See
LICENSE for details.

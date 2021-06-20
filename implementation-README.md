# Enums

This is an implementation of [SRFI 209](https://srfi.schemers.org/srfi-209),
which provides enumerated types for Scheme.

# Requirements

This library should be portable to any R7RS-small implementation
with the following libraries from R7RS-large:

* `(scheme list)` ([SRFI 1](https://srfi.schemers.org/srfi-1))
* `(scheme comparator)` ([SRFI 128](https://srfi.schemers.org/srfi-128)
  with or without [SRFI 162](https://srfi.schemers.org/srfi-162))
* `(scheme hash-table)` ([SRFI 125](https://srfi.schemers.org/srfi-125))
* [SRFI 178](https://srfi.schemers.org/srfi-178)

[SRFI 145](https://srfi.schemers.org/srfi-145) is an optional
dependency.

The included test suite can be run with
[SRFI 64](https://srfi.schemers.org/srfi-64) or
[SRFI 78](https://srfi.schemers.org/srfi-78) by loading the appropriate
shim file.  A basic fallback implementation of SRFI 78 is provided in
`test-srfi-78.scm` for Schemes without SRFI 78.  A shim is also provided
for [chibi-scheme](http://synthcode.com/scheme/chibi/),
using the `(chibi test)` module.

# Implementation notes

The side-effecting enum-set operations (`enum-set-adjoin!`,
`enum-set-delete!`, etc.) are primitive; their functional
counterparts are wrappers which copy their enum-set argument
before mutating it.  Thus, in performance-critical applications,
the side-effecting forms should be preferred.

This implementation provides `enum-set-filter!` and
`enum-set-remove!`, although they were not specified in the SRFI.

# Implementation author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy minus the zy

# License

This is free software released under the MIT/X license.  See
LICENSE for details.

# Enums

This is an implementation of enumerated types for Scheme, based on a
[pre-SRFI specification](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/EnumsCowan.md)
by John Cowan.

From the pre-SRFI's Rationale:

> Many procedures in many libraries accept arguments from a finite set
> (usually a fairly small one), or subsets of a finite set to describe
> one or more modes of operation.  Offering a mechanism for dealing with
> such values fosters portable and readable code, much as records do for
> compound values, or multiple values for procedures computing several
> results.
>
> This SRFI ...  provides something related to the *enums* of Java
> version 5 and later.  These are objects of a type disjoint from all
> others that are grouped into *enumeration types* (called *enum
> classes* in Java).  In Java, each enumeration type is allowed to
> declare the number and types of values associated with each object,
> but in this SRFI an enumeration object has exactly one value; this is
> useful when translating from C to record the numeric value, but has
> other uses as well.

# Requirements

This library should be portable to any R7RS-small implementation
with the following libraries from R7RS-large:

* `(scheme list)` ([SRFI 1](https://srfi.schemers.org/srfi-1))
* `(scheme hash-table)` ([SRFI 125](https://srfi.schemers.org/srfi-125))
* `(scheme comparator)` ([SRFI 128](https://srfi.schemers.org/srfi-128)
  with or without [SRFI 162](https://srfi.schemers.org/srfi-162))
* `(scheme mapping)` ([SRFI 146](https://srfi.schemers.org/srfi-146))

[SRFI 145](https://srfi.schemers.org/srfi-145) is an optional
dependency.

# Notes and issues

The current enums spec states that the set-theoretical enum set
procedures (`enum-set-union!`, etc.) may mutate their *first* enum-set
argument.  Enum sets are currently implemented in terms of SRFI 146
mappings, using their linear update set-theoretical procedures
(`mapping-union!`, etc.).  SRFI 146 does not specify which arguments
may be mutated by these procedures.

It is an error to pass `list->enum-set` an empty list, since in this
case there is no enum type to construct an enum set on.

The current spec has two mutually incompatible definitions of
`enum-set-project`, one taking two enum sets, and one taking an enum
type and an enum set.  This library implements the latter version.

# Author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy minus the zy

# License

This is free software released under the MIT/X license.  See
LICENSE for details.

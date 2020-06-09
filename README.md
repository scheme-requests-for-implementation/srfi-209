# Enums

This is an implementation of enumerated types for Scheme, based on a
[pre-SRFI specification](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/EnumsCowan.md)
by John Cowan.  It should be portable to any R7RS-small implementation
with SRFIs 1, 125, 128, and 146.

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

# Author

Wolfgang Corcoran-Mathe

Email: wcm at sigwinch dot xyzzy minus the zy

# License

This is free software released under the MIT/X license.  See
LICENSE for details.

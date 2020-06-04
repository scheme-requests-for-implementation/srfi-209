(define-library (enums)
  ;; TODO: cond-expands.
  (import (scheme base)
          (only (srfi 1) iota zip)
          (srfi 125)
          (srfi 128)
          (srfi 133)
          (srfi 145))

  (export enum-type? enum?
          make-enum-type
          enum-type enum-name enum-ordinal enum-value
          enum-name->enum
          enum-type-size enum-min enum-max enum-type-enums
          enum-type-names enum-type-values)

  (include "enums.scm"))

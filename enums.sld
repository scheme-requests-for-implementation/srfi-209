(define-library (enums)
  ;; TODO: cond-expands.
  (import (scheme base)
          (srfi 1)
          (srfi 128)
          (srfi 145))

  (export enum-type? enum? enum-type-contains? enum=? enum<? enum>?
          enum<=? enum>=?

          make-enum-type

          enum-type enum-name enum-ordinal enum-value

          enum-name->enum enum-ordinal->enum enum-name->ordinal
          enum-name->value enum-ordinal->name enum-ordinal->value

          enum-type-size enum-min enum-max enum-type-enums
          enum-type-names enum-type-values

          enum-next enum-prev
          )

  (include "enums.scm"))

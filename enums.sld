(define-library (enums)
  ;; TODO: cond-expands.
  (import (scheme base)
          (srfi 1)
          (srfi 125)
          (srfi 128)
          (srfi 145)
          (srfi 146))

  (cond-expand
    ((library (srfi 162))
     (import (only (srfi 162) real-comparator)))
    (else
     (begin
      (define real-comparator
        (make-comparator real? = < number-hash)))))

  (export enum-type? enum? enum-type-contains? enum=? enum<? enum>?
          enum<=? enum>=?

          make-enum-type

          enum-type enum-name enum-ordinal enum-value

          enum-name->enum enum-ordinal->enum enum-name->ordinal
          enum-name->value enum-ordinal->name enum-ordinal->value

          enum-type-size enum-min enum-max enum-type-enums
          enum-type-names enum-type-values

          enum-next enum-prev

          enum-type->enum-set enum-set list->enum-set enum-set-project
          enum-set-copy

          enum-set? enum-set-contains? enum-set=?

          enum-set-adjoin! enum-set-delete! enum-set-delete-all!

          enum-set-size enum-set->list enum-set-collect enum-set-for-each
          enum-set-fold

          enum-set-union! enum-set-intersection! enum-set-difference!
          enum-set-xor!

          make-enum-comparator
          )

  (include "enums.scm"))

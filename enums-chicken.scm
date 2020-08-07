(module
  (srfi 205)
  (enum-type? enum? enum-type-contains? enum=? enum<? enum>?
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

  (import (scheme)
          (only (chicken base) case-lambda include define-record-type
                               error)
          (srfi 1)
          (srfi 128)
 ;         (srfi 146 hash)
 ;         (srfi 146))
          )

  (import-for-syntax (srfi 145))

  (begin
   (define real-comparator
     (make-comparator real? = < number-hash)))

  (include "srfi/205.scm"))

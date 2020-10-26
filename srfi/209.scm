;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;

;;;; Utility

(define (exact-natural? obj)
  (and (exact-integer? obj) (not (negative? obj))))

;;;; Types

(define-record-type <enum-type>
  (make-raw-enum-type enum-vector name-table comparator)
  enum-type?
  (enum-vector enum-type-enum-vector set-enum-type-enum-vector!)
  (name-table enum-type-name-table set-enum-type-name-table!)
  (comparator enum-type-comparator set-enum-type-comparator!))

(define-record-type <enum>
  (make-enum type name ordinal value)
  enum?
  (type enum-type)
  (name enum-name)
  (ordinal enum-ordinal)
  (value enum-value))

(define (make-enum-type names+vals)
  (let* ((type (make-raw-enum-type #f #f #f))
         (enums (generate-enums type names+vals)))
    (set-enum-type-enum-vector! type (list->vector enums))
    (set-enum-type-name-table! type (make-name-table enums))
    (set-enum-type-comparator! type (make-enum-comparator type))
    type))

(define (generate-enums type names+vals)
  (map (lambda (elt ord)
         (cond ((and (pair? elt) (= 2 (length elt)) (symbol? (car elt)))
                (make-enum type (car elt) ord (cadr elt)))
               ((symbol? elt) (make-enum type elt ord ord))
               (else (error "make-enum-type: invalid argument" elt))))
       names+vals
       (iota (length names+vals))))

(define symbol-comparator
  (make-comparator symbol?
                   eqv?
                   (lambda (sym1 sym2)
                     (string<? (symbol->string sym1)
                               (symbol->string sym2)))
                   symbol-hash))

(define (make-name-table enums)
  (mapping-unfold null?
                  (lambda (enums)
                    (values (enum-name (car enums)) (car enums)))
                  cdr
                  enums
                  symbol-comparator))

(define (%enum-type=? etype1 etype2)
  (eqv? etype1 etype2))

(define (make-enum-comparator type)
  (make-comparator
   (lambda (obj)
     (and (enum? obj) (eq? (enum-type obj) type)))
   eq?
   (lambda (enum1 enum2)
     (< (enum-ordinal enum1) (enum-ordinal enum2)))
   (lambda (enum)
     (symbol-hash (enum-name enum)))))

;;;; Predicates

(define (enum-type-contains? type enum)
  (assume (enum-type? type))
  (assume (enum? enum))
  ((comparator-type-test-predicate (enum-type-comparator type)) enum))

(define (%enum-type-contains?/no-check type enum)
  ((comparator-type-test-predicate (enum-type-comparator type)) enum))

(define (%well-typed-enum? type obj)
  (and (enum? obj) (%enum-type-contains?/no-check type obj)))

(define (%compare-enums compare enums)
  (assume (and (pair? enums) (pair? (cdr enums)))
          "Invalid number of arguments")
  (assume (enum? (car enums)))
  (let ((type (enum-type (car enums))))
    (assume (every (lambda (e) (%well-typed-enum? type e)) (cdr enums))
            "All enums must be of the same enum type")
    (apply compare (enum-type-comparator type) enums)))

;; enum=? will probably see the most use out of the various enum
;; comparisons, so we provide a two-argument fast path.
(define (enum=? enum1 enum2 . enums)
  (assume (enum? enum1))
  (let* ((type (enum-type enum1))
         (comp (enum-type-comparator type)))
    (cond ((null? enums)                            ; fast path
           (assume (%well-typed-enum? type enum2))
           ((comparator-equality-predicate comp) enum1 enum2))
          (else                                     ; variadic path
           (assume (every (lambda (e) (%well-typed-enum? type e)) enums)
                   "Arguments must be of the same enum type")
           (apply =? comp enum1 enum2 enums)))))

(define (enum<? . enums) (%compare-enums <? enums))

(define (enum>? . enums) (%compare-enums >? enums))

(define (enum<=? . enums) (%compare-enums <=? enums))

(define (enum>=? . enums) (%compare-enums >=? enums))

;;;; Enum finders

;;; Core procedures

(define (enum-name->enum type name)
  (assume (enum-type? type))
  (assume (symbol? name))
  (mapping-ref/default (enum-type-name-table type) name #f))

(define (enum-ordinal->enum enum-type ordinal)
  (assume (enum-type? enum-type))
  (assume (exact-natural? ordinal))
  (and (< ordinal (enum-type-size enum-type))
       (vector-ref (enum-type-enum-vector enum-type) ordinal)))

;;; Derived procedures

(define (%enum-project type finder key proc)
  (assume (enum-type? type))
  (let ((enum (finder type key)))
    (assume (enum? enum))
    (proc enum)))

(define (enum-name->ordinal type name)
  (assume (symbol? name))
  (%enum-project type enum-name->enum name enum-ordinal))

(define (enum-name->value type name)
  (assume (symbol? name))
  (%enum-project type enum-name->enum name enum-value))

(define (enum-ordinal->name type ordinal)
  (assume (exact-natural? ordinal))
  (%enum-project type enum-ordinal->enum ordinal enum-name))

(define (enum-ordinal->value type ordinal)
  (assume (exact-natural? ordinal))
  (%enum-project type enum-ordinal->enum ordinal enum-value))

;;;; Enum type accessors

(define (enum-type-size type)
  (assume (enum-type? type))
  (vector-length (enum-type-enum-vector type)))

(define (enum-min type)
  (assume (enum-type? type))
  (vector-ref (enum-type-enum-vector type) 0))

(define (enum-max type)
  (assume (enum-type? type))
  (let ((vec (enum-type-enum-vector type)))
    (vector-ref vec (- (vector-length vec) 1))))

(define (enum-type-enums type)
  (assume (enum-type? type))
  (vector->list (enum-type-enum-vector type)))

(define (enum-type-names type)
  (assume (enum-type? type))
  (let ((vec (enum-type-enum-vector type)))
    (list-tabulate (vector-length vec)
                   (lambda (n) (enum-name (vector-ref vec n))))))

(define (enum-type-values type)
  (assume (enum-type? type))
  (let ((vec (enum-type-enum-vector type)))
    (list-tabulate (vector-length vec)
                   (lambda (n) (enum-value (vector-ref vec n))))))

;;;; Enum object procedures

(define (enum-next enum)
  (assume (enum? enum))
  (enum-ordinal->enum (enum-type enum) (+ (enum-ordinal enum) 1)))

(define (enum-prev enum)
  (assume (enum? enum))
  (let ((ord (enum-ordinal enum)))
    (and (> ord 0)
         (enum-ordinal->enum (enum-type enum) (- ord 1)))))

;;;; Enum set constructors

(define-record-type <enum-set>
  (make-enum-set type mapping)
  enum-set?
  (type enum-set-type)
  (mapping enum-set-mapping))

(define (%enum-list->enum-set type enums)
  (make-enum-set
   type
   (mapping-unfold null?
                   (lambda (enums)
                     (let ((enum (car enums)))
                       (assume (enum-type-contains? type enum))
                       (values (enum-ordinal enum) enum)))
                   cdr
                   enums
                   real-comparator)))

(define (enum-type->enum-set type)
  (assume (enum-type? type))
  (%enum-list->enum-set type (enum-type-enums type)))

(define (enum-set . enums) (list->enum-set enums))

(define (list->enum-set enums)
  (assume (pair? enums))
  (%enum-list->enum-set (enum-type (car enums)) enums))

(define (enum-set-project type eset)
  (assume (enum-type? type))
  (assume (enum-set? eset))
  (make-enum-set
   type
   (mapping-map (lambda (_ enum)
                  (let ((enum* (enum-name->enum type (enum-name enum))))
                    (values (enum-ordinal enum*) enum*)))
                real-comparator
                (enum-set-mapping eset))))

(define (enum-set-copy eset)
  (make-enum-set (enum-set-type eset)
                 (mapping-copy (enum-set-mapping eset))))

;;;; Enum set predicates

(define (enum-set-contains? eset enum)
  (assume (enum-set? eset))
  (assume (enum-type-contains? (enum-set-type eset) enum)
          "enum-set-contains?: ill-typed enum")
  (mapping-ref (enum-set-mapping eset)
               (enum-ordinal enum)
               (lambda () #f)
               (lambda (result)
                 (enum=? result enum))))

(define (%enum-set-type=? eset1 eset2)
  (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))

(define (enum-set-empty? eset)
  (assume (enum-set? eset))
  (mapping-empty? (enum-set-mapping eset)))

(define (enum-set-disjoint? eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (assume (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))
  (mapping-disjoint? (enum-set-mapping eset1) (enum-set-mapping eset2)))

(define (%compare-enum-sets compare eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (assume (%enum-set-type=? eset1 eset2) "enum sets have different types")
  (compare (enum-type-comparator (enum-set-type eset1))
           (enum-set-mapping eset1)
           (enum-set-mapping eset2)))

(define (enum-set=? eset1 eset2)
  (%compare-enum-sets mapping=? eset1 eset2))

(define (enum-set<? eset1 eset2)
  (%compare-enum-sets mapping<? eset1 eset2))

(define (enum-set>? eset1 eset2)
  (%compare-enum-sets mapping>? eset1 eset2))

(define (enum-set<=? eset1 eset2)
  (%compare-enum-sets mapping<=? eset1 eset2))

(define (enum-set>=? eset1 eset2)
  (%compare-enum-sets mapping>=? eset1 eset2))

(define (enum-set-any? pred eset)
  (assume (enum-set? eset))
  (mapping-any? (lambda (_ enum) (pred enum)) (enum-set-mapping eset)))

(define (enum-set-every? pred eset)
  (assume (enum-set? eset))
  (mapping-every? (lambda (_ enum) (pred enum)) (enum-set-mapping eset)))

;;;; Enum set mutators

(define (enum-set-adjoin! eset . enums)
  (assume (enum-set? eset))
  (let ((type (enum-set-type eset)))
    (make-enum-set
     type
     (fold (lambda (enum mapping)
             (assume (%enum-type-contains?/no-check type enum))
             (mapping-adjoin! mapping (enum-ordinal enum) enum))
           (enum-set-mapping eset)
           enums))))

(define (enum-set-delete! eset . enums)
  (assume (enum-set? eset))
  (enum-set-delete-all! eset enums))

(define (enum-set-delete-all! eset enum-lis)
  (assume (enum-set? eset))
  (make-enum-set
   (enum-set-type eset)
   (mapping-delete-all! (enum-set-mapping eset)
                        (map enum-ordinal enum-lis))))

;;;; Enum set operations

(define (enum-set-size eset)
  (assume (enum-set? eset))
  (mapping-size (enum-set-mapping eset)))

(define (enum-set->list eset)
  (assume (enum-set? eset))
  (mapping-values (enum-set-mapping eset)))

(define (enum-set-map->list proc eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-map->list (lambda (_ enum) (proc enum))
                     (enum-set-mapping eset)))

(define (enum-set-count pred eset)
  (assume (enum-set? eset))
  (mapping-count (lambda (_ enum) (pred enum))
                 (enum-set-mapping eset)))

(define (enum-set-filter pred eset)
  (assume (enum-set? eset))
  (make-enum-set (enum-set-type eset)
                 (mapping-filter (lambda (_ enum) (pred enum))
                                 (enum-set-mapping eset))))

(define (enum-set-remove pred eset)
  (assume (enum-set? eset))
  (make-enum-set (enum-set-type eset)
                 (mapping-remove (lambda (_ enum) (pred enum))
                                 (enum-set-mapping eset))))

(define (enum-set-for-each proc eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-for-each (lambda (_ enum) (proc enum))
                    (enum-set-mapping eset)))

(define (enum-set-fold proc nil eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-fold (lambda (_ enum state)
                  (proc enum state))
                nil
                (enum-set-mapping eset)))

;;;; Enum set logical operations

(define (%enum-set-logical-op! proc eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (assume (%enum-set-type=? eset1 eset2) "enum sets have different types")
  (make-enum-set
   (enum-set-type eset1)
   (proc (enum-set-mapping eset1) (enum-set-mapping eset2))))

(define (enum-set-union! eset1 eset2)
  (%enum-set-logical-op! mapping-union! eset1 eset2))

(define (enum-set-intersection! eset1 eset2)
  (%enum-set-logical-op! mapping-intersection! eset1 eset2))

(define (enum-set-difference! eset1 eset2)
  (%enum-set-logical-op! mapping-difference! eset1 eset2))

(define (enum-set-xor! eset1 eset2)
  (%enum-set-logical-op! mapping-xor! eset1 eset2))

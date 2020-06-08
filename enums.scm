;;;; Utility

(define (exact-natural? obj)
  (and (integer? obj) (exact? obj) (not (negative? obj))))

;;;; Types

(define-record-type <enum-type>
  (make-raw-enum-type enums comparator)
  enum-type?
  (enums enum-type-enums set-enum-type-enums!)
  (comparator enum-type-comparator set-enum-type-comparator!))

(define-record-type <enum>
  (make-enum type name ordinal value)
  enum?
  (type enum-type)
  (name enum-name)
  (ordinal enum-ordinal)
  (value enum-value))

;; This is a very simple representation which stores the enums
;; in a flat list.  TODO: Find a better representation and support
;; values.
(define (make-enum-type names)
  (let ((type (make-raw-enum-type #f #f)))
    (set-enum-type-enums! type
                          (map (lambda (name ord)
                                 (make-enum type name ord #f))
                               names
                               (iota (length names))))
    (set-enum-type-comparator! type (make-enum-comparator type))
    type))

(define (%enum-type=? etype1 etype2)
  (eqv? etype1 etype2))

;; TODO: Ensure this is good enough.
(define (make-enum-comparator type)
  (make-comparator
   (lambda (obj)
     (and (enum? obj) (eqv? (enum-type obj) type)))
   (lambda (enum1 enum2)
     (eqv? (enum-name enum1) (enum-name enum2)))
   (lambda (enum1 enum2)
     (< (enum-ordinal enum1) (enum-ordinal enum2)))
   (lambda (enum)
     (symbol-hash (enum-name enum)))))

;;;; Predicates

(define (enum-type-contains? type enum)
  (assume (enum-type? type))
  (assume (enum? enum))
  ((comparator-type-test-predicate (enum-type-comparator type)) enum))

(define (%compare-enums enums compare)
  (if (null? enums)
      #t
      (let ((type (enum-type (car enums))))
        ;; TODO: This type check is a bit bogus; improve it.
        (unless (every (lambda (enum)
                         (enum-type-contains? type enum))
                       enums)
          (error "enums must all be of the same type" enums))
        (apply compare (enum-type-comparator type) enums))))

(define (enum=? . enums) (%compare-enums enums =?))

(define (enum<? . enums) (%compare-enums enums <?))

(define (enum>? . enums) (%compare-enums enums >?))

(define (enum<=? . enums) (%compare-enums enums <=?))

(define (enum>=? . enums) (%compare-enums enums >=?))

;;;; Enum finders

;;; Core procedures

(define (enum-name->enum type name)
  (assume (enum-type? type))
  (assume (symbol? name))
  (find (lambda (enum)
          (eqv? (enum-name enum) name))
        (enum-type-enums type)))

(define (enum-ordinal->enum enum-type ordinal)
  (assume (enum-type? enum-type))
  (assume (exact-natural? ordinal))
  (and (< ordinal (enum-type-size enum-type))
       (list-ref (enum-type-enums enum-type) ordinal)))

;;; Derived procedures

(define (%enum-project type finder key proc)
  (assume (enum-type? type))
  (cond ((finder type key) => proc)
        (else (error "no matching enum found" type key))))

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

;;;; Enumeration type accessors

(define (enum-type-size type)
  (assume (enum-type? type))
  (length (enum-type-enums type)))

(define (enum-min type)
  (assume (enum-type? type))
  (car (enum-type-enums type)))

(define (enum-max type)
  (assume (enum-type? type))
  (last (enum-type-enums type)))

(define (enum-type-names type)
  (map enum-name (enum-type-enums type)))

(define (enum-type-values type)
  (map enum-value (enum-type-enums type)))

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
                       (unless (enum-type-contains? type enum)
                         (error "ill-typed enum" enum type))
                       (values (enum-ordinal enum) enum)))
                   cdr
                   enums
                   real-comparator)))

(define (enum-type->enum-set type)
  (assume (enum-type? type))
  (%enum-list->enum-set type (enum-type-enums type)))

(define (enum-set . enums) (list->enum-set enums))

;; Returns true if enum is of the enum-type that defines eset.
(define (%enum-matches-enum-set-type? eset enum)
  ((comparator-type-test-predicate
    (enum-type-comparator
     (enum-set-type eset)))
   enum))

(define (list->enum-set enums)
  ;; FIXME: This should probably not be an error.  Determine what
  ;; properties the empty enum set should have.
  (unless (pair? enums)
    (error "list->enum-set: empty list"))
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
  (assume (enum? enum))
  (unless (%enum-matches-enum-set-type? eset enum)
    (error "enum-set-contains?: ill-typed value" enum eset))
  (mapping-ref (enum-set-mapping eset)
               (enum-ordinal enum)
               (lambda () #f)
               (lambda (result)
                 (enum=? result enum))))

(define (%enum-set-type=? eset1 eset2)
  (%enum-type=? (enum-set-type eset1) (enum-set-type eset2)))

(define (enum-set=? eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (unless (%enum-set-type=? eset1 eset2)
    (error "enum-set=?: enum sets have different types" eset1 eset2))
  (mapping=? (enum-type-comparator (enum-set-type eset1))
             (enum-set-mapping eset1)
             (enum-set-mapping eset2)))

;;;; Enum set mutators

(define (%ensure-enums-match-enum-set-type eset enums)
  (cond ((any (lambda (enum)
                (not (%enum-matches-enum-set-type? eset enum)))
              enums) =>
         (lambda (enum) (error "ill-typed enum" enum enum-set)))))

(define (enum-set-adjoin! eset . enums)
  (assume (enum-set? eset))
  (%ensure-enums-match-enum-set-type eset enums)
  (make-enum-set
   (enum-set-type eset)
   (fold (lambda (enum mapping)
           (mapping-adjoin! mapping (enum-ordinal enum) enum))
         (enum-set-mapping eset)
         enums)))

(define (enum-set-delete! eset . enums)
  (assume (enum-set? eset))
  (enum-set-delete-all! eset enums))

(define (enum-set-delete-all! eset enum-lis)
  (assume (enum-set? eset))
  (%ensure-enums-match-enum-set-type eset enum-lis)
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

(define (enum-set-collect proc eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-map->list (lambda (_ enum) (proc enum))
                     (enum-set-mapping eset)))

(define (enum-set-for-each proc eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-for-each (lambda (_ enum) (proc enum))
                    (enum-set-mapping eset)))

;; FIXME: Order of proc's arguments?
(define (enum-set-fold proc nil eset)
  (assume (procedure? proc))
  (assume (enum-set? eset))
  (mapping-fold (lambda (_ enum state)
                  (proc enum state))
                nil
                (enum-set-mapping eset)))

;;;; Enum set logical operations

;;; FIXME: May want procedure names in error messages.

(define (%enum-set-logical-op! proc eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (unless (%enum-set-type=? eset1 eset2)
    (error "enum sets have different types" eset1 eset2))
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

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

(define (make-enum-comparator type)
  (make-comparator
   (lambda (obj)
     (and (enum? obj) (eqv? (enum-type obj) type)))
   (lambda (enum1 enum2)
     (eqv? (enum-name enum1) (enum-name enum2)))
   (lambda (enum1 enum2)
     (< (enum-ordinal enum1) (enum-ordinal enum2)))
   ;; TODO: Hash function.
   #f))

;;;; Predicates

(define (enum-type-contains? type enum)
  (assume (enum-type? type))
  (assume (enum? enum))
  ((comparator-type-test-predicate (enum-type-comparator type)) enum))

(define (%compare-enums enums compare)
  (if (null? enums)
      #t
      (let ((type (enum-type (car enums))))
        ;; TODO: Improve efficiency and error reporting.
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

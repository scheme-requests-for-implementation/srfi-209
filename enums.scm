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
  (make-enum-set type set)
  enum-set?
  (type enum-set-type)
  ;; FIXME: Better name.
  (set enum-set-set))

(define (enum-type->enum-set type)
  (assume (enum-type? type))
  (make-enum-set
   type
   (list->set (enum-type-comparator type) (enum-type-enums type))))

(define (enum-set . enums) (list->enum-set enums))

;; TODO: Type check.
(define (list->enum-set enums)
  ;; FIXME: This should probably not be an error.  Determine what
  ;; properties the empty enum set should have.
  (unless (pair? enums)
    (error "list->enum-set: empty list"))
  (let ((type (enum-type (car enums))))
    (make-enum-set
     type
     (list->set (enum-type-comparator type) enums))))

;; TODO: Type check.
(define (enum-set-project type eset)
  (assume (enum-type? type))
  (assume (enum-set? eset))
  (make-enum-set
   type
   (set-map (enum-type-comparator type)
            (lambda (enum)
              (enum-name->enum type (enum-name enum)))
            eset)))

(define (enum-set-copy eset)
  (make-enum-set (enum-set-type eset) (set-copy (enum-set-set eset))))

;;;; Enum set predicates

(define (enum-set-contains? eset enum)
  (assume (enum-set? eset))
  (unless (enum-type-contains? (enum-set-type eset) enum)
    (error "enum-set-contains?: ill-typed value" enum eset))
  (set-contains? (enum-set-set eset) enum))

(define (enum-set=? eset1 eset2)
  (assume (enum-set? eset1))
  (assume (enum-set? eset2))
  (unless (eqv? (enum-set-type eset1) (enum-set-type eset2))
    (error "enum-set=?: enum sets have different types" eset1 eset2))
  (set=? (enum-set-set eset1) (enum-set-set eset2)))

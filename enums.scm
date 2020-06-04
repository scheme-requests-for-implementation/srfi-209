;;;; Utility

(define (exact-natural? obj)
  (and (integer? obj) (exact? obj) (not (negative? obj))))

;;;; Types

;; Subject to change.
(define-record-type <enum-type>
  (make-raw-enum-type data nametab)
  enum-type?
  (data enum-type-data set-enum-type-data!)
  (nametab enum-type-nametab))

(define-record-type <enum>
  (make-enum type name ordinal value)
  enum?
  (type enum-type)
  (name enum-name)
  (ordinal enum-ordinal)
  (value enum-value))

(define (make-nametab names)
  (alist->hash-table
   (zip names (iota (length names)))
   ;; TODO: Use a symbol comparator.
   (make-default-comparator)))

;; TODO: Support values.
(define (make-enum-type names)
  (let ((type (make-raw-enum-type #f (make-nametab names))))
    (set-enum-type-data!
     type
     (vector-unfold (lambda (i names)
                      (values (make-enum type (car names) i #f)
                              (cdr names)))
                    (length names)
                    names))
    type))

;;;; Enum finders

;;; Core procedures

(define (enum-name->enum enum-type symbol)
  (assume (enum-type? enum-type))
  (assume (symbol? symbol))
  (hash-table-ref
   (enum-type-nametab enum-type)
   symbol
   (lambda () #f)
   (lambda (k) (vector-ref (enum-type-data enum-type) k))))

(define (enum-ordinal->enum enum-type ordinal)
  (assume (enum-type? enum-type))
  (assume (exact-natural? ordinal))
  (and (< ordinal (enum-type-size enum-type))
       (vector-ref (enum-type-data enum-type) ordinal)))

;;; Derived procedures

;;;; Enumeration type accessors

(define (enum-type-size type)
  (assume (enum-type? type))
  (vector-length (enum-type-data type)))

(define (enum-min type)
  (assume (enum-type? type))
  (vector-ref (enum-type-data type) 0))

(define (enum-max type)
  (assume (enum-type? type))
  (let ((enums (enum-type-data type)))
    (vector-ref enums (- (vector-length enums) 1))))

(define (%enum-map-type-data->list proc type)
  (assume (enum-type? type))
  (vector-fold-right
   (lambda (lis enum) (cons (proc enum) lis))
   '()
   (enum-type-data type)))

(define (enum-type-enums type)
  (assume (enum-type? type))
  (vector->list (enum-type-data type)))

(define (enum-type-names type)
  (%enum-map-type-data->list enum-name type))

(define (enum-type-values type)
  (%enum-map-type-data->list enum-value type))

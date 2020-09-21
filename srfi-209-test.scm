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

(import (scheme base))
(import (srfi 1))
(import (srfi 128))
(import (srfi 209))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define *tests-failed* 0)
      (define-syntax check
        (syntax-rules (=>)
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report)
        (if (zero? *tests-failed*)
            (begin
             (display "All tests passed.")
             (newline))
            (begin
             (display "TESTS FAILED: ")
             (display *tests-failed*)
             (newline)))))))

;;;; Utility

(define (print-header message)
  (newline)
  (display (string-append ";;; " message))
  (newline))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define always (constantly #t))
(define never (constantly #f))

;; Run a procedure on fresh copies of two enum sets.
(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1) (enum-set-copy eset2)))

;;;; Test types

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

(define color-blue (enum-name->enum color 'blue))

(define color-green (enum-name->enum color 'green))

(define color-set (enum-type->enum-set color))

(define reddish (list->enum-set
                 (map (lambda (name)
                        (enum-name->enum color name))
                      (take color-names 3))))

(define ~reddish (list->enum-set
                  (map (lambda (ord)
                         (enum-name->enum color ord))
                       (drop color-names 3))))

(define empty-colors
  (enum-set-delete-all! (enum-set-copy color-set)
                        (enum-type-enums color)))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi     "mushrooms")
    (bianca     "ricotta and mozzarella")
    (chicago    "deep-dish")
    (hawaiian   "pineapple and ham")))

(define pizza-names (map car pizza-descriptions))

(define pizza (make-enum-type pizza-descriptions))

(define pizza-chicago (enum-name->enum pizza 'chicago))
(define pizza-bianca (enum-name->enum pizza 'bianca))

;;;; Finders and enum accessors

;;; Later tests make heavy use of these, so test these first.

(define (check-finders-and-enum-accessors)
  (print-header "Running finder and accessor tests...")

  (check (enum-name (enum-name->enum color 'red))              => 'red)
  (check (enum-ordinal (enum-name->enum color 'red))           => 0)
  (check (eqv? color (enum-type (enum-name->enum color 'red))) => #t)
  (check (enum-name (enum-ordinal->enum color 0))              => 'red)
  (check (enum-ordinal (enum-ordinal->enum color 0))           => 0)
  (check (eqv? color (enum-type (enum-ordinal->enum color 0))) => #t)
  (check (eqv? (enum-name->enum color 'red) (enum-ordinal->enum color 0))
   => #t)
  (check (enum-value (enum-name->enum pizza 'chicago))
   => "deep-dish")

  (check (enum-name->ordinal color 'red)  => 0)
  (check (enum-name->ordinal color 'blue) => 6)
  (check (enum-name->value pizza 'funghi) => "mushrooms")
  (check (enum-name->value color 'blue)   => (enum-name->ordinal color 'blue))
  (check (enum-ordinal->name color 0)     => 'red)
  (check (enum-ordinal->name pizza 3)     => 'chicago)
  (check (enum-ordinal->value pizza 1)    => "mushrooms")
  (check (enum-ordinal->value color 6)    => 6)
)

;; Ensure make-enum-type accepts only valid name or name+value arguments.
(define (check-type-constructor)
  (print-header "Running enum type constructor tests...")

  ;; Mixing name and name+value args.
  (check (enum-type?
          (make-enum-type
           '(vanilla (chocolate 2) strawberry (pistachio 4)))) => #t))

;;;; Predicates

(define (check-predicates)
  (print-header "Running predicate tests...")

  ;; Ensure enums aren't just symbols.
  (check (enum? color-red)  => #t)
  (check (enum? 'z)         => #f)

  (check (every (lambda (e) (enum-type-contains? color e))
                (map (lambda (s) (enum-name->enum color s)) color-names))
   => #t)
  (check (any (lambda (e) (enum-type-contains? color e))
              (map (lambda (s) (enum-name->enum pizza s)) pizza-names))
   => #f)

  (check (enum=? color-red (enum-ordinal->enum color 0)) => #t)
  (check (enum=? color-red color-tangerine)              => #f)
  (check (enum=? color-red color-red color-red)          => #t)
  (check (enum=? color-red color-red color-tangerine)    => #f)

  (check (enum<? color-red color-tangerine)        => #t)
  (check (enum<? color-tangerine color-tangerine)  => #f)
  (check (enum<? color-tangerine color-red)        => #f)
  (check (enum<? color-red color-green color-blue) => #t)
  (check (enum<? color-red color-blue color-blue)  => #f)
  (check (enum>? color-red color-tangerine)        => #f)
  (check (enum>? color-tangerine color-tangerine)  => #f)
  (check (enum>? color-tangerine color-red)        => #t)
  (check (enum>? color-blue color-green color-red) => #t)
  (check (enum>? color-blue color-red color-red)   => #f)
  (check (enum<=? color-red color-tangerine)       => #t)
  (check (enum<=? color-tangerine color-tangerine) => #t)
  (check (enum<=? color-tangerine color-red)       => #f)
  (check (enum<=? color-red color-blue color-blue) => #t)
  (check (enum<=? color-blue color-blue color-red) => #f)
  (check (enum>=? color-red color-tangerine)       => #f)
  (check (enum>=? color-tangerine color-tangerine) => #t)
  (check (enum>=? color-tangerine color-red)       => #t)
  (check (enum>=? color-blue color-red color-red)  => #t)
  (check (enum>=? color-blue color-red color-blue) => #f))

;;;; Enum type accessors

(define (check-enum-type-accessors)
  (print-header "Running enum-type accessor tests...")

  (check (enum-type-size color)       => (length color-names))
  (check (enum-type-size pizza)        => (length pizza-names))
  (check (enum-name (enum-min color)) => 'red)
  (check (enum-name (enum-min pizza)) => 'margherita)
  (check (enum-name (enum-max color)) => 'violet)
  (check (enum-name (enum-max pizza)) => 'hawaiian)

  (check (length (enum-type-enums color)) => (enum-type-size color))
  (check (equal? (map enum-name (enum-type-enums color)) color-names)
   => #t)
  (check (equal? (map enum-ordinal (enum-type-enums color))
                 (iota (enum-type-size color)))
   => #t)
  (check (equal? (map enum-value (enum-type-enums pizza))
                 (map cadr pizza-descriptions))
   => #t)

  (check (equal? (enum-type-names color) color-names) => #t)
  (check (equal? (enum-type-names pizza) pizza-names) => #t)
  (check (equal? (enum-type-values pizza)
                 (map cadr pizza-descriptions))       => #t)
  (check (equal? (enum-type-values color)
                 (iota (enum-type-size color)))       => #t)
)

(define (check-enum-operations)
  (print-header "Running enum operation tests...")

  (check (enum=? (enum-next color-red) color-tangerine) => #t)
  (check (enum=? (enum-prev color-tangerine) color-red) => #t)
  (check (enum=? (enum-next pizza-bianca) pizza-chicago) => #t)
  (check (enum=? (enum-prev pizza-chicago) pizza-bianca) => #t)
  (check (enum-next (enum-max color))                   => #f)
  (check (enum-prev (enum-min color))                   => #f)
)

;;;; Enum comparators

(define (check-enum-comparators)
  (print-header "Running enum comparator tests...")

  (let ((pizza-comparator (make-enum-comparator pizza)))
    (check (comparator? pizza-comparator)          => #t)
    (check (comparator-ordered? pizza-comparator)  => #t)
    (check (comparator-hashable? pizza-comparator) => #t)

    (check (every (lambda (e) (comparator-test-type pizza-comparator e))
                 (enum-type-enums pizza))
     => #t)
    (check (any (lambda (e) (comparator-test-type pizza-comparator e))
                (enum-type-enums color))
     => #f)

   (check (=? pizza-comparator
              pizza-chicago
              (enum-name->enum pizza 'chicago))
    => #t)
   (check (=? pizza-comparator pizza-bianca pizza-chicago)  => #f)
   (check (<? pizza-comparator pizza-bianca pizza-chicago)  => #t)
   (check (<? pizza-comparator pizza-bianca pizza-bianca)   => #f)
   (check (<? pizza-comparator pizza-chicago pizza-bianca)  => #f)
   (check (>? pizza-comparator pizza-bianca pizza-chicago)  => #f)
   (check (>? pizza-comparator pizza-bianca pizza-bianca)   => #f)
   (check (>? pizza-comparator pizza-chicago pizza-bianca)  => #t)
   (check (<=? pizza-comparator pizza-bianca pizza-chicago) => #t)
   (check (<=? pizza-comparator pizza-bianca pizza-bianca)  => #t)
   (check (<=? pizza-comparator pizza-chicago pizza-bianca) => #f)
   (check (>=? pizza-comparator pizza-bianca pizza-chicago) => #f)
   (check (>=? pizza-comparator pizza-bianca pizza-bianca)  => #t)
   (check (>=? pizza-comparator pizza-chicago pizza-bianca) => #t))
)

(define (check-enum-set-basic)
  (print-header "Running basic enum set tests...")

  ;; Ensure that an enum set created from an enum type with
  ;; enum-type->enum-set contains every enum of the original type.
  (check (let ((pizza-set (enum-type->enum-set pizza)))
           (every (lambda (enum)
                    (enum-set-contains? pizza-set enum))
                  (enum-type-enums pizza)))
   => #t)

  (check (let ((pizza-set (list->enum-set (enum-type-enums pizza))))
           (every (lambda (enum)
                    (enum-set-contains? pizza-set enum))
                  (enum-type-enums pizza)))
   => #t)

  (check (let ((pizza-set (apply enum-set (enum-type-enums pizza))))
           (every (lambda (enum) (enum-set-contains? pizza-set enum))
                  (enum-type-enums pizza)))
   => #t)

  (check (enum-set-contains? (enum-set color-red color-blue) color-red)
   => #t)
  (check (enum-set-contains? (enum-set color-red color-blue)
                             color-tangerine)
   => #f)

  (check (enum-set-empty? empty-colors) => #t)
  (check (enum-set-empty? color-set)    => #f)

  (check (enum-set=? (enum-set-project color reddish) reddish) => #t)

  (check (eqv? color-set (enum-set-copy color-set)) => #f)
)

;;;; Enum set predicates

(define (check-enum-set-predicates)
  (check (enum-set-disjoint? color-set empty-colors) => #t)
  (check (enum-set-disjoint? color-set reddish)      => #f)
  (check (enum-set-disjoint? reddish ~reddish)       => #t)

  ;;; comparisons

  (check (enum-set=? color-set (enum-set-copy color-set)) => #t)

  (check (enum-set=? color-set empty-colors) => #f)
  (check (enum-set<? reddish color-set)      => #t)
  (check (enum-set<? color-set reddish)      => #f)
  (check (enum-set<? color-set color-set)    => #f)
  (check (enum-set>? reddish color-set)      => #f)
  (check (enum-set>? color-set reddish)      => #t)
  (check (enum-set>? color-set color-set)    => #f)
  (check (enum-set<=? reddish color-set)     => #t)
  (check (enum-set<=? color-set reddish)     => #f)
  (check (enum-set<=? color-set color-set)   => #t)
  (check (enum-set>=? reddish color-set)     => #f)
  (check (enum-set>=? color-set reddish)     => #t)
  (check (enum-set>=? color-set color-set)   => #t)

  ;;; any & every

  (check (enum-set-any? (lambda (e) (eq? 'green (enum-name e)))
                        color-set)
   => #t)
  (check (enum-set-any? (lambda (e) (eq? 'mauve (enum-name e)))
                        color-set)
   => #f)
  (check (enum-set-any? never empty-colors) => #f)
  (check (enum-set-every? (lambda (e) (eq? 'green (enum-name e)))
                          color-set)
   => #f)
  (check (enum-set-every? (lambda (e) (< (enum-ordinal e) 10))
                          color-set)
   => #t)
  (check (enum-set-every? never empty-colors) => #t)
)

;;;; Enum set mutators

(define (check-enum-set-mutators)
  (print-header "Running enum-set mutator tests...")

  (let ((reddish+green
         (enum-set-adjoin! (enum-set-copy reddish) color-green)))
    (check (enum-set<? reddish reddish+green)             => #t)
    (check (enum-set-contains? reddish+green color-green) => #t))

  (let ((reddish* (enum-set-delete! (enum-set-copy reddish)
                                    color-tangerine)))
    (check (enum-set<? reddish* reddish)                 => #t)
    (check (enum-set-contains? reddish* color-tangerine) => #f))

  (let ((reddish** (enum-set-delete-all! (enum-set-copy reddish)
                                         (list color-tangerine))))
    (check (enum-set<? reddish** reddish)                 => #t)
    (check (enum-set-contains? reddish** color-tangerine) => #f))

  (check (enum-set-empty?
          (enum-set-delete-all! color-set (enum-type-enums color)))
   => #t)
)

(define (check-enum-set-operations)
  (print-header "Running enum-set operations tests...")

  (check (enum-set-size color-set) => (length color-names))
  (check (enum-set-size empty-colors) => 0)

  (check (equal? (enum-set->list color-set) (enum-type-enums color)) => #t)
  (check (null? (enum-set->list empty-colors)) => #t)
  (check (= (enum-set-size color-set)
            (length (enum-set->list color-set)))
   => #t)

  (check (enum-set-map->list enum-name color-set)    => color-names)
  (check (enum-set-map->list enum-name empty-colors) => '())
  (check (equal? (enum-set-map->list enum-name color-set)
                 (map enum-name (enum-set->list color-set)))
   => #t)

  (check (enum-set-count (lambda (e) (enum=? e color-blue)) color-set)
   => 1)
  (check (enum-set-count (lambda (e) (enum=? e color-blue)) reddish)
   => 0)
  (check (enum-set-count (lambda (e) (string? (enum-value e)))
                         (enum-type->enum-set pizza))
   => (length pizza-descriptions))

  ;;; filter & remove

  (check (enum-set<? (enum-set-filter (lambda (e) (enum=? e color-red))
                                      color-set)
                     color-set)
   => #t)
  (check (enum-set-map->list (enum-set-filter (lambda (e) (enum=? e color-red))
                                              color-set))
         (filter (lambda (s) (eq? s 'red)) color-names)
   => #t)
  (check (enum-set=? (enum-set-filter always color-set) color-set)
   => #t)
  (check (enum-set-empty? (enum-set-filter never color-set)) => #t)
  (check (enum-set<? (enum-set-remove (lambda (e) (enum=? e color-red))
                                      color-set)
                     color-set)
   => #t)
  (check (enum-set-map->list (enum-set-remove (lambda (e) (enum=? e color-red))
                                              color-set))
         (remove (lambda (s) (eq? s 'red)) color-names)
   => #t)
  (check (enum-set=? (enum-set-remove never color-set) color-set)
   => #t)
  (check (enum-set-empty? (enum-set-remove always color-set)) => #t)

  (check (let ((n 0))
           (enum-set-for-each (lambda (_)
                                (set! n (+ n 1)))
                              color-set)
           n)
   => (length color-names))

  (check (enum-set-fold (lambda (enum lis)
                          (cons (enum-name enum) lis))
                        '()
                        color-set)
   => (reverse color-names))
)

(define (check-enum-set-logical)
  (print-header "Running enum-set logical operations tests...")

  (check (enum-set=? color-set
                     (fresh-sets enum-set-union! reddish ~reddish))
   => #t)
  (check (enum-set-empty?
          (fresh-sets enum-set-intersection! reddish ~reddish))
   => #t)
  (check (enum-set=? ~reddish
                     (fresh-sets enum-set-difference! color-set reddish))
   => #t)
  (check (enum-set=? color-set
                     (fresh-sets enum-set-xor! reddish ~reddish))
   => #t)
  (check (enum-set-empty?
          (fresh-sets enum-set-xor! reddish reddish))
   => #t))

(define (check-all)
  (check-finders-and-enum-accessors)
  (check-type-constructor)
  (check-predicates)
  (check-enum-type-accessors)
  (check-enum-operations)
  (check-enum-comparators)
  (check-enum-set-basic)
  (check-enum-set-predicates)
  (check-enum-set-mutators)
  (check-enum-set-operations)
  (check-enum-set-logical)

  (check-report))

(check-all)

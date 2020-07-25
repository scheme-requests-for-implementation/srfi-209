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
(import (enums))

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

;;; Since there's no exported accessor for the enum-type of an enum-set,
;;; we can't differentiate between empty enum-sets of different types.

(define (enum-set-empty? eset)
  (zero? (enum-set-size eset)))

;; Is eset1 a subset of eset2?
(define (enum-subset? eset1 eset2)
  (every (lambda (enum)
           (enum-set-contains? eset2 enum))
         (enum-set->list eset1)))

;; Run a procedure on fresh copies of two enum sets.
(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1) (enum-set-copy eset2)))

;;;; Test types

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

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

(define pizza (make-enum-type pizza-descriptions))

(define pizza-chicago (enum-name->enum pizza 'chicago))

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
  (check (enum-name->value pizza 'funghi) => "mushrooms")
  (check (enum-ordinal->name color 0)     => 'red)
  (check (enum-ordinal->value pizza 1)    => "mushrooms"))

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

  (check (enum-type? color) => #t)
  (check (enum-type? 'z)    => #f)
  (check (enum? color-red)  => #t)
  (check (enum? 'z)         => #f)

  (check (enum-type-contains? color color-red)     => #t)
  (check (enum-type-contains? color pizza-chicago) => #f)

  (check (enum=? (enum-name->enum color 'red)
                 (enum-ordinal->enum color 0))
   => #t)

  (check (enum<? color-red color-tangerine)        => #t)
  (check (enum<? color-tangerine color-tangerine)  => #f)
  (check (enum<? color-tangerine color-red)        => #f)
  (check (enum>? color-red color-tangerine)        => #f)
  (check (enum>? color-tangerine color-tangerine)  => #f)
  (check (enum>? color-tangerine color-red)        => #t)
  (check (enum<=? color-red color-tangerine)       => #t)
  (check (enum<=? color-tangerine color-tangerine) => #t)
  (check (enum<=? color-tangerine color-red)       => #f)
  (check (enum>=? color-red color-tangerine)       => #f)
  (check (enum>=? color-tangerine color-tangerine) => #t)
  (check (enum>=? color-tangerine color-red)       => #t))

;;;; Enum type accessors

(define (check-enum-type-accessors)
  (print-header "Running enum-type accessor tests...")

  (check (enum-type-size color)       => (length color-names))
  (check (enum-name (enum-min color)) => 'red)
  (check (enum-name (enum-max color)) => 'violet)

  (check (length (enum-type-enums color)) => (enum-type-size color))
  (check (equal? (map enum-name (enum-type-enums color)) color-names)
   => #t)
  (check (equal? (map enum-ordinal (enum-type-enums color))
                 (iota (enum-type-size color)))
   => #t)

  (check (equal? (enum-type-names color) color-names) => #t)
  (check (equal? (enum-type-names pizza)
                 (map car pizza-descriptions))        => #t)
  (check (equal? (enum-type-values pizza)
                 (map cadr pizza-descriptions))       => #t))

(define (check-enum-operations)
  (print-header "Running enum operation tests...")

  (check (enum=? (enum-next color-red) color-tangerine) => #t)
  (check (enum=? (enum-prev color-tangerine) color-red) => #t)
  (check (enum-next (enum-max color))                   => #f)
  (check (enum-prev (enum-min color))                   => #f))

(define (check-enum-set-basic)
  (print-header "Running basic enum set tests...")

  ;; Ensure that an enum set created from an enum type with
  ;; enum-type->enum-set contains every enum of the original type.
  (check (let ((pizza-set (enum-type->enum-set pizza)))
           (every (lambda (enum)
                    (enum-set-contains? pizza-set enum))
                  (enum-type-enums pizza)))
   => #t)

  (check (enum-set-contains? reddish (enum-name->enum color 'red))
   => #t)
  (check (enum-set-contains? reddish (enum-name->enum color 'tangerine))
   => #t)
  (check (enum-set-contains? reddish (enum-name->enum color 'blue))
   => #f)

  (check (eqv? color-set (enum-set-copy color-set)) => #f)

  (check (enum-set=? color-set (list->enum-set (enum-type-enums color)))
   => #t))

;;;; Enum set mutators

(define (check-enum-set-mutators)
  (print-header "Running enum-set mutator tests...")

  (let* ((color-green (enum-name->enum color 'green))
         (reddish+green
          (enum-set-adjoin! (enum-set-copy reddish) color-green)))
    (check (enum-subset? reddish reddish+green)          => #t)
    (check (enum-set-contains? reddish+green color-green) => #t))

  (let* ((color-tangerine (enum-name->enum color 'tangerine))
         (reddish* (enum-set-delete! (enum-set-copy reddish)
                                     color-tangerine)))
    (check (enum-subset? reddish* reddish)              => #t)
    (check (enum-set-contains? reddish* color-tangerine) => #f))

  (check (enum-set-empty? empty-colors) => #t))

(define (check-enum-set-operations)
  (print-header "Running enum-set operations tests...")

  (check (enum-set-size color-set) => (length color-names))

  (check (equal? (enum-set->list color-set) (enum-type-enums color)) => #t)

  (check (= (enum-set-size color-set)
            (length (enum-set->list color-set)))
   => #t)

  (check (enum-set-collect enum-name color-set)    => color-names)
  (check (enum-set-collect enum-name empty-colors) => '())

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

  (check (enum-set=? (enum-set-project color reddish) reddish) => #t))

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
  (check-enum-set-basic)
  (check-enum-set-operations)
  (check-enum-set-logical)

  (check-report))

(check-all)

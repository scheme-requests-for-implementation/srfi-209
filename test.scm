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
      (define-syntax check
        (syntax-rules (=>)
          ((check expr)
           (check expr => #t))
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report) #t))))

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

(define-syntax catch-exceptions
  (syntax-rules ()
    ((_ expr)
     (guard (_ (else 'exception)) expr))))

;;;; Test types

(define color-names
  '(red tangerine orange yellow green cyan blue violet))

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))

(define color-tangerine (enum-name->enum color 'tangerine))

(define color-set (enum-type->enum-set color))

(define topping
  (make-enum-type '(peppers onions mushrooms pepperoni)))

(define topping-onions (enum-name->enum topping 'onions))

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

  (check (enum-name->ordinal color 'red) => 0)
  (check (enum-ordinal->name color 0)    => 'red))

;;;; Predicates

(define (check-predicates)
  (print-header "Running predicate tests...")

  (check (enum-type? color) => #t)
  (check (enum-type? 'z)    => #f)
  (check (enum? color-red)  => #t)
  (check (enum? 'z)         => #f)

  (check (enum-type-contains? color color-red)      => #t)
  (check (enum-type-contains? color topping-onions) => #f)

  (check (enum=? (enum-name->enum color 'red)
                 (enum-ordinal->enum color 0))
   => #t)
  (check (catch-exceptions (enum=? color-red topping-onions))
   => 'exception)

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

  (check (equal? (enum-type-names color) color-names) => #t))

(define (check-enum-operations)
  (print-header "Running enum operation tests...")

  (check (enum=? (enum-next color-red) color-tangerine) => #t)
  (check (enum=? (enum-prev color-tangerine) color-red) => #t)
  (check (enum-next (enum-max color))                   => #f)
  (check (enum-prev (enum-min color))                   => #f))

(define (check-enum-set-basic)
  (define reddish (enum-set (enum-name->enum color 'red)
                            (enum-name->enum color 'tangerine)
                            (enum-name->enum color 'orange)))

  (print-header "Running basic enum set tests...")

  ;; Ensure that an enum set created from an enum type with
  ;; enum-type->enum-set contains every enum of the original type.
  (check (let ((topping-set (enum-type->enum-set topping)))
           (every (lambda (enum)
                    (enum-set-contains? topping-set enum))
                  (enum-type-enums topping)))
   => #t)

  (check (enum-set-contains? reddish (enum-name->enum color 'red))
   => #t)
  (check (enum-set-contains? reddish (enum-name->enum color 'tangerine))
   => #t)
  (check (enum-set-contains? reddish (enum-name->enum color 'blue))
   => #f)

  (check (catch-exceptions
           (enum-set (enum-name->enum color 'red)
                     (enum-name->enum toppings 'peppers))) => 'exception)

  (check (eqv? color-set (enum-set-copy color-set)) => #f)

  (check (enum-set=? color-set (list->enum-set (enum-type-enums color)))
   => #t))

(define (check-all)
  (check-finders-and-enum-accessors)
  (check-predicates)
  (check-enum-type-accessors)
  (check-enum-operations)
  (check-enum-set-basic)

  (check-report))

;(check-all)

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

;;;; Test types

(define color
  (make-enum-type
   '(red tangerine orange yellow green cyan blue violet)))

;;;; Enum finders

;;; Later tests make heavy use of these, so test these first.

(define (check-finders)
  (print-header "Running finders tests...")

  (check (enum-name (enum-name->enum color 'red))              => #t)
  (check (enum-ordinal (enum-name->enum color 'red))           => 0)
  (check (eqv? color (enum-type (enum-name->enum color 'red))) => #t)
  (check (enum-name (enum-name->ordinal color 0))              => #t)
  (check (enum-ordinal (enum-name->ordinal color 0))           => 0)
  (check (eqv? color (enum-type (enum-name->ordinal color 0))) => #t)
  (check (eqv? (enum-name->enum color 'red) (enum-ordinal->enum color 0))
   => #t))

;;;; Predicates

(define (check-predicates)
  (print-header "Running predicate tests...")

  (check (enum-type? color)                   => #t)
  (check (enum-type? 'z)                      => #f)
  (check (enum? (enum-name->enum color 'red)) => #t))

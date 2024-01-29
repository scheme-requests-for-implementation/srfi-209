;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 128)
        (srfi 209))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define *tests-failed* 0)
      (define-syntax check
        (syntax-rules (=>)
          ((check expr => expected)
           (check expr (=> equal?) expected))
          ((check expr (=> equal) expected)
           (if (equal expr expected)
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

;;;; SRFI 64 shim

(define-syntax test-equal
  (syntax-rules ()
    ((_ expected expr)
     (check expr => expected))))

(define-syntax test-eqv
  (syntax-rules ()
    ((_ expected expr)
     (check expr (=> eqv?) expected))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group name t ...)
     (begin
      (newline)
      (display ";;; Test group: ")
      (display name)
      (newline)
      (newline)
      t ...))))

(include "test-body.scm")

(check-report)

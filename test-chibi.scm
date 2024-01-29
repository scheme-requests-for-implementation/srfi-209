;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

;;; Test suite for chibi-scheme (http://synthcode.com/scheme/chibi)

(import (scheme base)
        (srfi 1)
        (srfi 128)
        (srfi 209)
        (chibi test))

;;;; SRFI 64 shim

;; chibi's test-equal is not SRFI 64's test-equal.
(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest)
     (test . rest))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eqv . rest)
     (test-equal eqv? . rest))))

(include "test-body.scm")

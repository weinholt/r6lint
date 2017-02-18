;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2017 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(library (r6lint tests check)
  (export check check-report check-passed?)
  (import (rnrs (6)))

  (define checks-passed 0)
  (define checks-ok? #t)

  (define (check-it test-expr test expected-expr expected)
    (let ((test-result (test))
          (expected-result (expected)))
      (cond ((equal? test-result expected-result)
             (set! checks-passed (+ checks-passed 1)))
            (else
             (newline)
             (write test-expr)
             (display "\n=>\n")
             (write test-result)
             (newline)
             (display "; Wrong! Expected:\n")
             (write expected-result)
             (newline)
             (set! checks-ok? #f)))))

  (define (check-report)
    (display "\n; ")
    (display checks-passed)
    (display " checks passed\n")
    (unless checks-ok?
      (display "; There are errors\n"))
    (flush-output-port (current-output-port)))

  (define (check-passed? n)
    (= checks-passed n))

  (define-syntax check
    (syntax-rules (=>)
      ((_ test => expected)
       (check-it 'test (lambda () test) 'expected (lambda () expected))))))

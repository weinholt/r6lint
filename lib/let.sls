;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>
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

;; R6RS linter -- recover let expressions

(library (r6lint lib let)
  (export
    pass-recover-let)
  (import
    (r6lint lib expressions)
    (r6lint lib letrec)
    (r6lint lib messages)
    (r6lint psyntax builders)
    (r6lint psyntax internal)
    (r6lint psyntax gensym)
    (rnrs (6)))

  (define (pass-recover-let code emit)
    (define (formals-match? proccase operand*)
      ;; Do the formals match the operands for this case-lambda case?
      (let ((len-formals (length (proccase-formals proccase)))
            (len-operand* (length operand*)))
        (if (proccase-proper? proccase)
            (= len-operand* len-formals)
            (>= len-operand* (- len-formals 1)))))
    (define (recover-bind location formals operand* body proper?)
      (let ((operand^*
             (if proper?
                 operand*
                 (let lp ((formals formals) (operand* operand*))
                   (if (null? (cdr formals))
                       (list (make-funcall location
                                           (make-primref location 'list)
                                           operand*
                                           #f))
                       (cons (car operand*)
                             (lp (cdr formals) (cdr operand*))))))))
        (make-bind location formals operand^* body)))
    (define (pass x)
      (cond ((rec*? x)
             (make-rec* (expr-location x)
                        (rec*-lhs* x)
                        (map pass (rec*-rhs* x))
                        (pass (rec*-body x))))
            ((rec? x)
             (make-rec (expr-location x)
                       (rec-lhs* x)
                       (map pass (rec-rhs* x))
                       (pass (rec-body x))))
            ((proc? x)
             (make-proc (expr-location x)
                        (map (lambda (x)
                               (make-proccase (expr-location x)
                                              (proccase-formals x)
                                              (proccase-proper? x)
                                              (pass (proccase-body x))))
                             (proc-case* x))
                        (proc-name x)))
            ((seq? x)
             (make-seq (expr-location x)
                       (pass (seq-e0 x))
                       (pass (seq-e1 x))))
            ((mutate? x)
             (make-mutate (expr-location x)
                          (mutate-name x)
                          (pass (mutate-expr x))))
            ((test? x)
             (make-test (expr-location x)
                        (pass (test-expr x))
                        (pass (test-then x))
                        (pass (test-else x))))
            ((and (funcall? x) (proc? (funcall-operator x)))
             (let ((op (funcall-operator x))
                   (operand* (funcall-operand* x)))
               (cond ((find (lambda (c) (formals-match? c operand*))
                            (proc-case* op))
                      => (lambda (c)
                           (for-each (lambda (c^)
                                       (unless (eq? c^ c)
                                         (expr-emit emit c^ 'refactor
                                                    'unreachable-code "Unreachable code")))
                                     (proc-case* op))
                           (recover-bind (expr-location x)
                                         (proccase-formals c)
                                         (map pass operand*)
                                         (pass (proccase-body c))
                                         (proccase-proper? c))))
                     (else
                      (expr-emit emit x 'error 'wrong-arguments "Wrong arguments to procedure")
                      (let ((loc (expr-location x)))
                        (make-funcall loc (make-primref loc 'assertion-violation)
                                      (list (make-const loc 'apply)
                                            (make-const loc "Wrong argument count")
                                            (make-const loc (records->core x)))
                                      (funcall-tail? x)))))))
            ((funcall? x)
             (make-funcall (expr-location x)
                           (pass (funcall-operator x))
                           (map pass (funcall-operand* x))
                           (funcall-tail? x)))
            ((const? x) x)
            ((ref? x) x)
            ((primref? x) x)
            (else
             (error 'pass-recover-let "Unknown type" x))))
    (pass code)))

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

;; R6RS linter -- static analysis.

(library (r6lint lib analyser)
  (export
    analyse-program analyse-library
    pass-recover-let pass-referenced-variables pass-fix-letrec
    code->records)
  (import
    (r6lint lib expressions)
    (r6lint lib let)
    (r6lint lib letrec)
    (r6lint lib messages)
    (r6lint psyntax builders)
    (r6lint psyntax internal)
    (r6lint psyntax gensym)
    (rnrs (6)))

  (define *DEBUG* #f)

  (define (analyse-program name* code* emit)
    (analyse name* code* #f emit))

  (define (analyse-library name* code* emit)
    (analyse name* code* #t emit))

  (define (analyse name* code* library? emit)
    (let lp ((name* name*)
             (code* code*))
      (unless (null? name*)
        (let ((name (car name*))
              (code (car code*)))
          (when *DEBUG*
            (display name)
            (display ":\n")
            (write code)
            (newline)
            (write (expanded->core code))
            (newline))
          (let ((rec (code->records code)))
            (when *DEBUG*
              (write rec)
              (newline))
            ;; Only emit warnings for the last code. That's the file
            ;; being checked.
            (when (null? (cdr code*))
              (let* ((rec (pass-recover-let rec emit))
                     (rec (pass-referenced-variables rec))
                     (rec (pass-fix-letrec rec emit)))
                (check-unused-variables rec library? emit)
                (check-misused-equality rec emit))))
          (lp (cdr name*) (cdr code*))))))


;;; Check for unused variables

  (define (pass-referenced-variables code)
    (define who 'pass-referenced-variables)
    (define (pass code)
      ;; XXX: Should use define-walker for these
      (cond
        ((variable? code)
         (values))
        ((ref? code)
         (variable-referenced?-set! (ref-name code) #t)
         (pass (ref-name code)))
        ((mutate? code)
         (variable-mutated?-set! (mutate-name code) #t)
         (pass (mutate-name code))
         (pass (mutate-expr code)))
        ((test? code)
         (pass (test-expr code))
         (pass (test-then code))
         (pass (test-else code)))
        ((const? code)
         (values))
        ((primref? code)
         (values))
        ((seq? code)
         (pass (seq-e0 code))
         (pass (seq-e1 code)))
        ((bind? code)
         (for-each pass (bind-lhs* code))
         (for-each pass (bind-rhs* code))
         (pass (bind-body code)))
        ((rec? code)
         (for-each pass (rec-lhs* code))
         (for-each pass (rec-rhs* code))
         (pass (rec-body code)))
        ((rec*? code)
         (for-each pass (rec*-lhs* code))
         (for-each pass (rec*-rhs* code))
         (pass (rec*-body code)))
        ((proc? code)
         (for-each
          (lambda (c)
            (for-each pass (proccase-formals c))
            (pass (proccase-body c)))
          (proc-case* code)))
        ((funcall? code)
         (pass (funcall-operator code))
         (for-each pass (funcall-operand* code)))
        (else
         (error who "Unknown expression" code))))
    (pass code)
    code)

  (define (check-unused-variables code library? emit)
    (define who 'check-unused-variables)
    (define (pass code)
      (cond
        ((variable? code)
         ;; Emit warnings for unused variables.
         (when (and (not (variable-referenced? code))
                    (not (variable-export-name code)))
           (unless (let ((name (variable->string code)))
                     ;; Ignore the dummy variable psyntax adds to the
                     ;; end of letrec* in top-level programs.
                     (or (and (not library?) (string=? name "dummy"))
                         ;; Ignore variables beginning with _.
                         (and (> (string-length name) 0)
                              (char=? (string-ref name 0) #\_))))
             (expr-emit emit code 'refactor 'unused-variable
                        (string-append "Unused variable: "
                                       (variable->string code))))))
        ((ref? code)
         (pass (ref-name code)))
        ((mutate? code)
         (pass (mutate-name code))
         (pass (mutate-expr code)))
        ((test? code)
         (pass (test-expr code))
         (pass (test-then code))
         (pass (test-else code)))
        ((const? code)
         (values))
        ((primref? code)
         (values))
        ((seq? code)
         (pass (seq-e0 code))
         (pass (seq-e1 code)))
        ((bind? code)
         (for-each pass (bind-lhs* code))
         (for-each pass (bind-rhs* code))
         (pass (bind-body code)))
        ((fix? code)
         (for-each pass (fix-lhs* code))
         (for-each pass (fix-rhs* code))
         (pass (fix-body code)))
        ((rec? code)
         (for-each pass (rec-lhs* code))
         (for-each pass (rec-rhs* code))
         (pass (rec-body code)))
        ((rec*? code)
         (for-each pass (rec*-lhs* code))
         (for-each pass (rec*-rhs* code))
         (pass (rec*-body code)))
        ((proc? code)
         (for-each
          (lambda (c)
            (for-each pass (proccase-formals c))
            (pass (proccase-body c)))
          (proc-case* code)))
        ((funcall? code)
         (pass (funcall-operator code))
         (for-each pass (funcall-operand* code)))
        (else
         (error who "Unknown expression" code))))
    (pass code))

  (define (variable->string var)
    (gensym-name (variable-name var)))

;;; Check for misused equality predicates

  (define-syntax define-walker
    (lambda (x)
      (syntax-case x ()
        ((_ (pass code) (test body ...) ...)
         #'(define (pass code)
             (cond
               (test body ...) ...
               ((variable? code)
                (values))
               ((ref? code)
                (pass (ref-name code)))
               ((mutate? code)
                (pass (mutate-name code))
                (pass (mutate-expr code)))
               ((test? code)
                (pass (test-expr code))
                (pass (test-then code))
                (pass (test-else code)))
               ((const? code)
                (values))
               ((primref? code)
                (values))
               ((seq? code)
                (pass (seq-e0 code))
                (pass (seq-e1 code)))
               ((bind? code)
                (for-each pass (bind-lhs* code))
                (for-each pass (bind-rhs* code))
                (pass (bind-body code)))
               ((fix? code)
                (for-each pass (fix-lhs* code))
                (for-each pass (fix-rhs* code))
                (pass (fix-body code)))
               ((rec? code)
                (for-each pass (rec-lhs* code))
                (for-each pass (rec-rhs* code))
                (pass (rec-body code)))
               ((rec*? code)
                (for-each pass (rec*-lhs* code))
                (for-each pass (rec*-rhs* code))
                (pass (rec*-body code)))
               ((proc? code)
                (for-each
                 (lambda (c)
                   (for-each pass (proccase-formals c))
                   (pass (proccase-body c)))
                 (proc-case* code)))
               ((funcall? code)
                (pass (funcall-operator code))
                (for-each pass (funcall-operand* code)))
               (else
                (error 'pass "Unknown expression" code))))))))

  ;; This checks for calls to eq?/eqv? with arguments that aren't
  ;; usually comparable with those predicates. This is a very naïve
  ;; analysis, of course, doesn't find anything but the obvious bad
  ;; calls. You gotta start somewhere.

  (define (check-misused-equality code emit)
    (define (arg-not-comparable-with-eqv? arg)
      (or (and (const? arg) (not-comparable-with-eqv? (const-value arg)))
          (proc? arg)))
    (define (not-comparable-with-eqv? obj)
      (or (pair? obj) (vector? obj) (string? obj) (bytevector? obj)
          (and (number? obj) (nan? obj))))
    (define (arg-not-comparable-with-eq? arg)
      (and (const? arg) (not-comparable-with-eq? (const-value arg))))
    (define (not-comparable-with-eq? obj)
      (or (not-comparable-with-eqv? obj) (number? obj) (char? obj)))
    (define (maybe-get-list arg)
      ;; If the argument is a list, then try to get the list elements.
      (cond ((and (const? arg) (list? (const-value arg)))
             (const-value arg))
            ((and (funcall? arg)
                  (primref? (funcall-operator arg))
                  (eq? (primref-name (funcall-operator arg)) 'list))
             (funcall-operand* arg))
            (else '())))
    (define-walker (check-misused-equality code)
      ((funcall? code)
       (let ((op (funcall-operator code))
             (arg* (funcall-operand* code)))
         (when (and (primref? op) (eq? (primref-name op) 'eq?)
                    (exists arg-not-comparable-with-eq? arg*))
           (expr-emit emit code 'warning 'bad-eq-argument
                      "Uncomparable arguments to eq?"))
         (when (and (primref? op) (eq? (primref-name op) 'eqv?)
                    (exists arg-not-comparable-with-eqv? arg*))
           (expr-emit emit code 'warning 'bad-eqv-argument
                      "Uncomparable arguments to eqv?"))
         (when (and (primref? op) (eq? (primref-name op) 'memq)
                    (= (length arg*) 2)
                    (or (arg-not-comparable-with-eq? (car arg*))
                        (exists not-comparable-with-eqv? (maybe-get-list (cadr arg*)))))
           (expr-emit emit code 'warning 'bad-memq-argument
                      (string-append "Uncomparable arguments to memq")))
         (when (and (primref? op) (eq? (primref-name op) 'memv)
                    (= (length arg*) 2)
                    (or (arg-not-comparable-with-eqv? (car arg*))
                        (exists not-comparable-with-eqv? (maybe-get-list (cadr arg*)))))
           (expr-emit emit code 'warning 'bad-memv-argument
                      (string-append "Uncomparable arguments to memv"))))
       (check-misused-equality (funcall-operator code))
       (for-each check-misused-equality (funcall-operand* code))))
    (check-misused-equality code)))

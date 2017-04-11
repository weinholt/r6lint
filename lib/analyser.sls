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
  (export analyse-program analyse-library)
  (import (r6lint psyntax builders)
          (r6lint psyntax internal)
          (only (r6lint lib reader) annotation-source->condition source-filename
                source-line source-column)
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
              (check-unused-variables rec library? emit)
              (check-misused-equality rec emit)))
          (lp (cdr name*) (cdr code*))))))

;;; Records for all expressions

  (define-record-type expr
    (fields location)
    (nongenerative) (sealed #f))

  (define-record-type primref           ;built-in primitive
    (parent expr)
    (fields name)
    (nongenerative) (sealed #t))

  (define-record-type const             ;quote
    (parent expr)
    (fields value)
    (nongenerative) (sealed #t))

  (define-record-type seq               ;begin
    (parent expr)
    (fields e0 e1)
    (nongenerative) (sealed #t))

  (define-record-type test              ;if
    (parent expr)
    (fields expr then else)
    (nongenerative) (sealed #t))

  (define-record-type variable          ;variable
    (parent expr)
    (fields name export-name (mutable referenced?) (mutable mutated?))
    (nongenerative) (sealed #t))

  (define-record-type ref               ;variable reference
    (parent expr)
    (fields name)
    (nongenerative) (sealed #t))

  (define-record-type mutate            ;set!
    (parent expr)
    (fields name expr)
    (nongenerative) (sealed #t))

  (define-record-type rec               ;letrec
    (parent expr)
    (fields lhs* rhs* body)
    (nongenerative) (sealed #t))

  (define-record-type rec*              ;letrec*
    (parent expr)
    (fields lhs* rhs* body)
    (nongenerative) (sealed #t))

  (define-record-type proc              ;case-lambda
    (parent expr)
    (fields case* name)
    (nongenerative) (sealed #t))

  (define-record-type proccase          ;case-lambda clause
    (parent expr)
    (fields formals proper? body)
    (nongenerative) (sealed #t))

  (define-record-type funcall           ;application
    (parent expr)
    (fields operator operand* tail?)
    (nongenerative) (sealed #t))

;;; Convert core code to records

  (define (maybe-expr-location expr)
    (and (expr? expr) (expr-location expr)))

  (define (code-source code)
    (cond
      ((expr-lexical-reference? code) (expr-lexical-reference-source code))
      ((expr-lexical-assignment? code) (expr-lexical-assignment-source code))
      ((expr-global-reference? code) (expr-global-reference-source code))
      ((expr-global-assignment? code) (expr-global-assignment-source code))
      ((expr-case-lambda? code) (expr-case-lambda-source code))
      ((expr-application? code) (expr-application-source code))
      ((expr-conditional? code) (expr-conditional-source code))
      ((expr-primref? code) (expr-primref-source code))
      ((expr-data? code) (expr-data-source code))
      ((expr-sequence? code) (expr-sequence-source code))
      ((expr-letrec? code) (expr-letrec-source code))
      ((expr-letrec*? code) (expr-letrec*-source code))
      ((expr-library-letrec*? code) (expr-library-letrec*-source code))
      (else
       (error 'code-source "Unknown code" code))))

  (define (code->records code)
    (define vars (make-eq-hashtable))
    (define (make-var location name export-name)
      (or (hashtable-ref vars name #f)
          (let ((ret (make-variable location name export-name #f #f)))
            (hashtable-set! vars name ret)
            ret)))
    (define (formals->list x)
      (cond ((null? x) '())
            ((pair? x) (cons (car x) (formals->list (cdr x))))
            (else (list x))))
    (define (pass code name tail?)
      (cond
        ((expr-lexical-reference? code)
         (let ((source (expr-lexical-reference-source code)))
           (make-ref source (make-var source (expr-lexical-reference-var code) #f))))
        ((expr-lexical-assignment? code)
         (let* ((source (expr-lexical-assignment-source code))
                (var (make-var source (expr-lexical-assignment-var code) #f)))
           (make-mutate source var
                        (pass (expr-lexical-assignment-exp code)
                              (expr-lexical-assignment-var code)
                              #f))))
        ((expr-global-reference? code)
         (let ((source (expr-global-reference-source code)))
           (make-ref source (make-var source (expr-global-reference-var code) #f))))
        ((expr-global-assignment? code)
         (let* ((source (expr-global-assignment-source code))
                (var (make-var source (expr-global-assignment-var code) #f)))
           (make-mutate source var
                        (pass (expr-global-assignment-exp code)
                              (expr-global-assignment-var code)
                              #f))))
        ((expr-case-lambda? code)
         (let* ((formals* (expr-case-lambda-vars* code))
                (body* (expr-case-lambda-exp* code))
                (source (expr-case-lambda-source code)))
           (make-proc source
                      (map (lambda (formals body)
                             (let ((var* (map (lambda (lhs)
                                                (make-var source lhs #f))
                                              (formals->list formals))))
                               (make-proccase source var* (list? formals)
                                              (pass body name 'tail))))
                           formals* body*)
                      name)))
        ((expr-application? code)
         (make-funcall (expr-application-source code)
                       (pass (expr-application-fun-exp code) name #f)
                       (map (lambda (op)
                              (pass op name #f))
                            (expr-application-arg-exp* code))
                       tail?))
        ((expr-conditional? code)
         (make-test (expr-conditional-source code)
                    (pass (expr-conditional-test-exp code) name #f)
                    (pass (expr-conditional-then-exp code) name tail?)
                    (if (expr-conditional-one-armed? code)
                        (make-primref (expr-conditional-source code) 'void)
                        (pass (expr-conditional-else-exp code) name tail?))))
        ((expr-primref? code)
         (make-primref (expr-primref-source code) (expr-primref-name code)))
        ((expr-data? code)
         (make-const (expr-data-source code) (expr-data-exp code)))
        ((expr-sequence? code)
         (let lp ((exp* (expr-sequence-exp* code)))
           (if (null? (cdr exp*))
               (pass (car exp*) name tail?)
               (make-seq (expr-sequence-source code)
                         (pass (car exp*) name #f) (lp (cdr exp*))))))
        ((expr-letrec? code)
         (let ((lhs* (expr-letrec-var* code))
               (rhs* (expr-letrec-val-exp* code))
               (body (expr-letrec-body-exp code)))
           (let* ((var* (map (lambda (lhs rhs)
                               (make-var (code-source rhs) lhs #f))
                             lhs* rhs*))
                  (rhs* (map (lambda (lhs rhs)
                               (pass rhs lhs #f))
                             lhs* rhs*)))
             (make-rec (expr-letrec-source code)
                       var* rhs*
                       (pass body name tail?)))))
        ((expr-letrec*? code)
         (let ((lhs* (expr-letrec*-var* code))
               (rhs* (expr-letrec*-val-exp* code))
               (body (expr-letrec*-body-exp code)))
           (let* ((var* (map (lambda (lhs rhs)
                               (make-var (code-source rhs) lhs #f))
                             lhs* rhs*))
                  (rhs* (map (lambda (lhs rhs)
                              (pass rhs lhs #f))
                            lhs* rhs*)))
             (make-rec* (expr-letrec*-source code)
                        var* rhs*
                        (pass body name tail?)))))
        ((expr-library-letrec*? code)
         (let ((lhs* (expr-library-letrec*-var* code))
               (loc* (expr-library-letrec*-loc* code))
               (rhs* (expr-library-letrec*-val-exp* code))
               (body (expr-library-letrec*-body-exp code)))
           (let* ((var* (map
                         (lambda (lhs loc rhs)
                           (make-var (code-source rhs) lhs
                                     (and (not (expr-library-letrec*-mix? code))
                                          loc)))
                         lhs* loc* rhs*))
                  (rhs* (map (lambda (lhs rhs)
                               (pass rhs lhs #f))
                             lhs* rhs*)))
             (make-rec* (expr-library-letrec*-source code)
                        var* rhs*
                        (pass body name tail?)))))
        (else
         (error 'code->records "Unknown code" code))))

    (pass code #f #f))

;;; Check for unused variables

  (define (expr-emit emit expr level id message)
    (cond ((expr-location expr) =>
           (lambda (location)
             (let ((con (annotation-source->condition location)))
               (emit (source-filename con)
                     (source-line con)
                     (source-column con)
                     level id message))))
          (else
           (emit #f 1 0 level id message))))

  (define (check-unused-variables code library? emit)
    (define who 'check-unused-variables)
    (define (pass0 code)
      (cond
        ((variable? code)
         (values))
        ((ref? code)
         (variable-referenced?-set! (ref-name code) #t)
         (pass0 (ref-name code)))
        ((mutate? code)
         (variable-mutated?-set! (mutate-name code) #t)
         (pass0 (mutate-name code))
         (pass0 (mutate-expr code)))
        ((test? code)
         (pass0 (test-expr code))
         (pass0 (test-then code))
         (pass0 (test-else code)))
        ((const? code)
         (values))
        ((primref? code)
         (values))
        ((seq? code)
         (pass0 (seq-e0 code))
         (pass0 (seq-e1 code)))
        ((rec? code)
         (for-each pass0 (rec-lhs* code))
         (for-each pass0 (rec-rhs* code))
         (pass0 (rec-body code)))
        ((rec*? code)
         (for-each pass0 (rec*-lhs* code))
         (for-each pass0 (rec*-rhs* code))
         (pass0 (rec*-body code)))
        ((proc? code)
         (for-each
          (lambda (c)
            (for-each pass0 (proccase-formals c))
            (pass0 (proccase-body c)))
          (proc-case* code)))
        ((funcall? code)
         (pass0 (funcall-operator code))
         (for-each pass0 (funcall-operand* code)))
        (else
         (error who "Unknown expression" code))))
    (define (pass1 code)
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
         (pass1 (ref-name code)))
        ((mutate? code)
         (pass1 (mutate-name code))
         (pass1 (mutate-expr code)))
        ((test? code)
         (pass1 (test-expr code))
         (pass1 (test-then code))
         (pass1 (test-else code)))
        ((const? code)
         (values))
        ((primref? code)
         (values))
        ((seq? code)
         (pass1 (seq-e0 code))
         (pass1 (seq-e1 code)))
        ((rec? code)
         (for-each pass1 (rec-lhs* code))
         (for-each pass1 (rec-rhs* code))
         (pass1 (rec-body code)))
        ((rec*? code)
         (for-each pass1 (rec*-lhs* code))
         (for-each pass1 (rec*-rhs* code))
         (pass1 (rec*-body code)))
        ((proc? code)
         (for-each
          (lambda (c)
            (for-each pass1 (proccase-formals c))
            (pass1 (proccase-body c)))
          (proc-case* code)))
        ((funcall? code)
         (pass1 (funcall-operator code))
         (for-each pass1 (funcall-operand* code)))
        (else
         (error who "Unknown expression" code))))
    (pass0 code)
    (pass1 code))

  (define (variable->string var)
    (gensym-name (variable-name var)))

;;; Check for misused equality predicates

  ;; This checks for calls to eq?/eqv? with arguments that aren't
  ;; usually comparable with those predicates. This is a very naïve
  ;; analysis, of course, doesn't find anything but the obvious bad
  ;; calls. You gotta start somewhere.

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

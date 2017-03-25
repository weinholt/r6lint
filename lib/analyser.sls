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
          (only (r6lint lib reader) annotation-source->condition source-filename
                source-line source-column)
          (rnrs (6)))

  (define *DEBUG* #f)

  (define (analyse-program name* core* emit)
    (analyse name* core* #f emit))

  (define (analyse-library name* core* emit)
    (analyse name* core* #t emit))

  (define (analyse name* core* library? emit)
    (let lp ((name* name*)
             (core* core*))
      (unless (null? name*)
        (let ((name (car name*))
              (core (car core*)))
          (let ((code (code->records core)))
            (when *DEBUG*
              (display name)
              (display ":\n")
              (write core)
              (newline)
              (write code)
              (newline))
            ;; Only emit warnings for the last code. That's the file
            ;; being checked.
            (when (null? (cdr core*))
              (check-unused-variables code library? emit)))
          (lp (cdr name*) (cdr core*))))))

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
    (fields name export-name (mutable referenced?) (mutable mutated?) global?)
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

  (define (code->records code)
    (define vars (make-eq-hashtable))
    (define (make-var location name global?)
      (or (hashtable-ref vars name #f)
          (let ((ret (make-variable location name #f #f #f global?)))
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
         (let ((source (expr-lexical-assignment-source code)))
           (make-mutate source (make-var source (expr-lexical-assignment-var code) #f)
                        (pass (expr-lexical-assignment-exp code)
                              (expr-lexical-assignment-var code)
                              #f))))
        ((expr-global-reference? code)
         (let ((source (expr-global-reference-source code)))
           (make-ref source (make-var source (expr-global-reference-var code) #t))))
        ((expr-global-assignment? code)
         ;; XXX: exported? Use library-letrec* instead.
         (let ((source (expr-global-assignment-source code)))
           (make-mutate source (make-var source (expr-global-assignment-var code) #t)
                        (pass (expr-global-assignment-exp code)
                              (expr-global-assignment-var code)
                              #f))))
        ((expr-case-lambda? code)
         (let* ((formals* (expr-case-lambda-vars* code))
                (body* (expr-case-lambda-exp* code))
                (source (expr-case-lambda-source code)))
           (make-proc source
                      (map (lambda (formals body)
                             (make-proccase source
                                            (map (lambda (lhs)
                                                   (make-var source lhs #f))
                                                 (formals->list formals))
                                            (list? formals)
                                            (pass body name 'tail)))
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
                    (pass (expr-conditional-else-exp code) name tail?)))
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
           (let ((rhs* (map (lambda (lhs rhs)
                              (pass rhs lhs #f))
                            lhs* rhs*)))
             (make-rec (expr-letrec-source code)
                       (map (lambda (lhs rhs)
                              (make-var (maybe-expr-location rhs) lhs #f))
                            lhs* rhs*)
                       rhs*
                       (pass body name tail?)))))
        ((expr-letrec*? code)
         (let ((lhs* (expr-letrec*-var* code))
               (rhs* (expr-letrec*-val-exp* code))
               (body (expr-letrec*-body-exp code)))
           (let ((rhs* (map (lambda (lhs rhs)
                              (pass rhs lhs #f))
                            lhs* rhs*)))
             (make-rec* (expr-letrec*-source code)
                        (map (lambda (lhs rhs)
                               (make-var (maybe-expr-location rhs) lhs #f))
                             lhs* rhs*)
                        rhs*
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
         ;; Emit warnings for unused variables. XXX: For now global
         ;; variables are assumed to be possibly used.
         (when (and (not (variable-referenced? code))
                    (not (variable-global? code)))
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
    (call-with-string-output-port
      (lambda (p)
        (write (string->symbol (symbol->string (variable-name var))) p)))))

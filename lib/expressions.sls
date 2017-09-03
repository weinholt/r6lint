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

;; Records for all expressions

(library (r6lint lib expressions)
  (export code->records records->core
          expr? expr-location
          primref? make-primref primref-name
          const? make-const const-value
          seq? make-seq seq-e0 seq-e1
          test? make-test test-expr test-then test-else
          variable? make-variable variable-name variable-export-name
          variable-referenced? variable-referenced?-set!
          variable-mutated? variable-mutated?-set!
          ref? make-ref ref-name
          mutate? make-mutate mutate-name mutate-expr
          bind? make-bind bind-lhs* bind-rhs* bind-body
          rec? make-rec rec-lhs* rec-rhs* rec-body
          rec*? make-rec* rec*-lhs* rec*-rhs* rec*-body
          proc? make-proc proc-case* proc-name
          proccase? make-proccase proccase-formals proccase-proper? proccase-body
          funcall? make-funcall funcall-operator funcall-operand* funcall-tail?)
  (import (r6lint psyntax builders)
          (rnrs (6)))

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

  (define-record-type bind              ;let
    (parent expr)
    (fields lhs* rhs* body)
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

  (define (records->core x)
    (define (f x)
      (cond ((const? x)
             (const-value x))
            ((variable? x)
             (variable-name x))
            ((primref? x)
             (primref-name x))
            ((ref? x)
             (f (ref-name x)))
            ((mutate? x)
             `(set! ,(f (mutate-name x)) ,(f (mutate-expr x))))
            ((funcall? x)
             `(,(f (funcall-operator x))
               ,@(map f (funcall-operand* x))))
            ((proc? x)
             (letrec ((get-formals (lambda (c)
                                     (let ((formals (map f (proccase-formals c))))
                                       (if (proccase-proper? c)
                                           formals
                                           (apply cons* formals))))))
               (if (null? (cdr (proc-case* x)))
                   (let ((c (car (proc-case* x))))
                     `(lambda ,(get-formals c) ,(f (proccase-body c))))
                   `(case-lambda
                      ,@(map (lambda (c)
                               (list (get-formals c) (f (proccase-body c))))
                             (proc-case* x))))))
            ((test? x)
             `(if ,(f (test-expr x)) ,(f (test-then x)) ,(f (test-else x))))
            ((seq? x)
             `(begin ,(f (seq-e0 x)) ,(f (seq-e1 x))))
            ((bind? x)
             `(let ,(map list (map f (bind-lhs* x)) (map f (bind-rhs* x)))
                ,(f (bind-body x))))
            ((rec? x)
             `(letrec ,(map list (map f (rec-lhs* x)) (map f (rec-rhs* x)))
                ,(f (rec-body x))))
            ((rec*? x)
             `(letrec* ,(map list (map f (rec*-lhs* x)) (map f (rec*-rhs* x)))
                ,(f (rec*-body x))))
            (else
             (error 'records->core "Unknown expression" x))))
    (f x))

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
         (let ((formals* (expr-case-lambda-vars* code))
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

    (pass code #f #f)))

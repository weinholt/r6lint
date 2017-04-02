;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (r6lint psyntax internal)
  (export current-primitive-locations compile-core-expr-to-port expanded->core)
  (import (rnrs)
          (r6lint psyntax compat)
          (r6lint psyntax builders)
          (r6lint psyntax config))

  (define current-primitive-locations
    (make-parameter
      (lambda _ #f)
      (lambda (p)
        (assert (procedure? p))
        p)))

  (define (mutable? x)
    (define (simple? x)
      (or (null? x)
          (char? x)
          (symbol? x)
          (boolean? x)
          (number? x)))
    (not (simple? x)))

  (define (rewriter quote-hack?)
    (if-wants-record-exprs
     (define (f x)
       (cond
         ;; The record-based intermediate language, if enabled in the config.
         ((expr-lexical-reference? x)
          (expr-lexical-reference-var x))
         ((expr-lexical-assignment? x)
          (list 'set! (expr-lexical-assignment-var x)
                (f (expr-lexical-assignment-exp x))))
         ((expr-global-reference? x)
          (if-use-r6rs-eval
           (list 'symbol-value (list 'quote (expr-global-reference-var x)))
           (expr-global-reference-var x)))
         ((expr-global-assignment? x)
          (if-use-r6rs-eval
           (list 'set-symbol-value! (list 'quote (expr-global-assignment-var x))
                 (f (expr-global-assignment-exp x)))
           (list 'set! (expr-global-assignment-var x)
                 (f (expr-global-assignment-exp x)))))
         ((expr-case-lambda? x)
          (cons 'case-lambda
                (map (lambda (vars exp)
                       (list vars (f exp)))
                     (expr-case-lambda-vars* x)
                     (expr-case-lambda-exp* x))))
         ((expr-conditional? x)
          (cons* 'if (f (expr-conditional-test-exp x))
                 (f (expr-conditional-then-exp x))
                 (if (expr-conditional-one-armed? x)
                     '()
                     (list (f (expr-conditional-else-exp x))))))
         ((expr-application? x)
          (cons (f (expr-application-fun-exp x))
                (map f (expr-application-arg-exp* x))))
         ((expr-primref? x)
          (let ((op (expr-primref-name x)))
            (cond
              (((current-primitive-locations) op) =>
               (lambda (loc)
                 loc))
              (else op))))
         ((expr-data? x)
          (cond
            ((and quote-hack? (mutable? (expr-data-exp x)))
             (let ((g (gensym)))
               (set-symbol-value! g (expr-data-exp x))
               (if-use-r6rs-eval
                (list 'hashtable-ref '*GLOBALS* (list 'quote g)
                      (f (build-application #f (build-void #f) '())))
                g)))
            (else (list 'quote (expr-data-exp x)))))
         ((expr-sequence? x)
          (cons 'begin (map f (expr-sequence-exp* x))))
         ((expr-letrec? x)
          (let ((lhs* (expr-letrec-var* x))
                (rhs* (expr-letrec-val-exp* x))
                (body (expr-letrec-body-exp x)))
            (list 'letrec
                  (map list lhs* (map f rhs*))
                  (f body))))
         ((expr-letrec*? x)
          (let ((lhs* (expr-letrec*-var* x))
                (rhs* (expr-letrec*-val-exp* x))
                (body (expr-letrec*-body-exp x)))
            (list 'letrec*
                  (map list lhs* (map f rhs*))
                  (f body))))
         (else
          (error 'rewrite "invalid form" x))))
     (define (f x)
       ;; The non-record language.
       ((pair? x)
        (case (car x)
          ((quote)
           (cond
             ((and quote-hack? (mutable? (cadr x)))
              (let ((g (gensym)))
                (set-symbol-value! g (cadr x))
                g))
             (else x)))
          ((case-lambda)
           (cons 'case-lambda
                 (map
                  (lambda (x)
                    (cons (car x) (map f (cdr x))))
                  (cdr x))))
          ((lambda)
           (cons* 'lambda (cadr x) (map f (cddr x))))
          ((letrec)
           (let ((bindings (cadr x)) (body* (cddr x)))
             (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
               (cons* 'letrec
                      (map list lhs* (map f rhs*))
                      (map f body*)))))
          ((letrec*)
           (let ((bindings (cadr x)) (body* (cddr x)))
             (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
               (cons* 'letrec*
                      (map list lhs* (map f rhs*))
                      (map f body*)))))
          ((begin)
           (cons 'begin (map f (cdr x))))
          ((set!)
           (list 'set! (cadr x) (f (caddr x))))
          ((primitive)
           (let ((op (cadr x)))
             (cond
               (((current-primitive-locations) op) =>
                (lambda (loc)
                  loc))
               (else op))))
          ((define) x)
          (else
           (if (list? x)
               (map f x)
               (error 'rewrite "invalid form ~s ~s" x (list? x))))))
       (else x)))
    f)

  (define need-quote-hack?
    (let ((x (cons 1 2)))
      (not (eq? (eval-core `',x) (eval-core `',x)))))

  (define (expanded->core x)
    ((rewriter need-quote-hack?) x))

  (define (compile-core-expr-to-port x p)
    (write ((rewriter #f) x) p)
    (newline p)))

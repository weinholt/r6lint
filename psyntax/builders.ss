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

(library (r6lint psyntax builders)
  (export build-lexical-assignment build-global-reference
    build-application build-conditional build-lexical-reference
    build-global-assignment build-lambda
    build-case-lambda build-let build-primref build-foreign-call
    build-data build-sequence build-void build-letrec build-letrec*
    build-global-define build-library-letrec*

    expr-lexical-reference? expr-lexical-reference-source
    expr-lexical-reference-var
    expr-lexical-assignment? expr-lexical-assignment-source
    expr-lexical-assignment-var expr-lexical-assignment-exp
    expr-global-reference? expr-global-reference-source
    expr-global-reference-var
    expr-global-assignment? expr-global-assignment-source
    expr-global-assignment-var expr-global-assignment-exp
    expr-conditional? expr-conditional-source
    expr-conditional-test-exp expr-conditional-then-exp expr-conditional-else-exp
    expr-application? expr-application-source
    expr-application-fun-exp expr-application-arg-exp*
    expr-lambda? expr-lambda-source
    expr-lambda-vars expr-lambda-exp
    expr-case-lambda? expr-case-lambda-source
    expr-case-lambda-vars* expr-case-lambda-exp*
    expr-primref? expr-primref-source
    expr-primref-level expr-primref-name
    expr-data? expr-data-source
    expr-data-exp
    expr-sequence? expr-sequence-source
    expr-sequence-exp*
    expr-letrec? expr-letrec-source
    expr-letrec-var* expr-letrec-val-exp* expr-letrec-body-exp
    expr-letrec*? expr-letrec*-source
    expr-letrec*-var* expr-letrec*-val-exp* expr-letrec*-body-exp
    expr-library-letrec*? expr-library-letrec*-source
    expr-library-letrec*-mix? expr-library-letrec*-name
    expr-library-letrec*-var* expr-library-letrec*-loc*
    expr-library-letrec*-val-exp* expr-library-letrec*-body-exp)
  (import (rnrs) (r6lint psyntax compat) (r6lint psyntax config))

  (define-record expr-lexical-reference (source var))
  (define-record expr-lexical-assignment (source var exp))
  (define-record expr-global-reference (source var))
  (define-record expr-global-assignment (source var exp))
  (define-record expr-application (source fun-exp arg-exp*))
  (define-record expr-conditional (source test-exp then-exp else-exp))
  (define-record expr-lambda (source vars exp))
  (define-record expr-case-lambda (source vars* exp*))
  (define-record expr-primref (source level name))
  (define-record expr-data (source exp))
  (define-record expr-sequence (source exp*))
  (define-record expr-letrec (source var* val-exp* body-exp))
  (define-record expr-letrec* (source var* val-exp* body-exp))
  (define-record expr-library-letrec* (source mix? name var* loc* val-exp* body-exp))

  (define (maybe-annotation-source ae)
    (and (annotation? ae) (annotation-source ae)))

  (define (build-global-define ae x)
    (if-wants-record-exprs
     (begin x (build-void ae))
     (if-wants-global-defines
      `(define ,x '#f)
      (build-void ae))))

  (define-syntax build-application
    (syntax-rules ()
      ((_ ae fun-exp arg-exps)
       (if-wants-record-exprs
        (make-expr-application (maybe-annotation-source ae) fun-exp arg-exps)
        `(,fun-exp . ,arg-exps)))))

  (define-syntax build-conditional
    (syntax-rules ()
      ((_ ae test-exp then-exp else-exp)
       (if-wants-record-exprs
        (make-expr-conditional (maybe-annotation-source ae) test-exp then-exp else-exp)
        `(if ,test-exp ,then-exp ,else-exp)))))

  (define-syntax build-lexical-reference
    (syntax-rules ()
      ((_ ae var)
       (if-wants-record-exprs
        (make-expr-lexical-reference (maybe-annotation-source ae) var)
        var))))

  (define-syntax build-lexical-assignment
    (syntax-rules ()
      ((_ ae var exp)
       (if-wants-record-exprs
        (make-expr-lexical-assignment (maybe-annotation-source ae) var exp)
        `(set! ,var ,exp)))))

  (define-syntax build-global-reference
    (syntax-rules ()
      ((_ ae var)
       (if-wants-record-exprs
        (make-expr-global-reference (maybe-annotation-source ae) var)
        var))))

  (define-syntax build-global-assignment
    (syntax-rules ()
      ((_ ae var exp)
       (if-wants-record-exprs
        (make-expr-global-assignment (maybe-annotation-source ae) var exp)
        `(set! ,var ,exp)))))

  (define build-lambda
    (lambda (ae vars exp)
      (if-wants-case-lambda
       (if-wants-record-exprs
        ;; TODO: Would be nice with more detailed source.
        (make-expr-case-lambda (maybe-annotation-source ae) (list vars) (list exp))
        `(case-lambda (,vars ,exp)))
       (if-wants-record-exprs
        (make-expr-lambda (maybe-annotation-source ae) vars exp)
        `(lambda ,vars ,exp)))))

  (define build-case-lambda
    (if-wants-case-lambda
     (lambda (ae vars* exp*)
       (if-wants-record-exprs
        (make-expr-case-lambda (maybe-annotation-source ae) vars* exp*)
        `(case-lambda . ,(map list vars* exp*))))
     (lambda (ae vars* exp*)
       (define (build-error ae)
         (build-application ae
           (build-primref ae 'error)
           (list (build-data ae 'apply)
                 (build-data ae "invalid arg count"))))
       (define (build-pred ae n vars)
         (let-values (((count pred)
                       (let f ((vars vars) (count 0))
                         (cond
                           ((pair? vars) (f (cdr vars) (+ count 1)))
                           ((null? vars) (values count '=))
                           (else (values count '>=))))))
           (build-application ae (build-primref ae pred)
             (list (build-lexical-reference ae n)
                   (build-data ae count)))))
       (define (build-apply ae g vars exp)
         (build-application ae (build-primref ae 'apply)
           (list (build-lambda ae vars exp)
                 (build-lexical-reference ae g))))
       (define (expand-case-lambda ae vars exp*)
         (let ((g (gensym)) (n (gensym)))
           `(lambda ,g
              ,(build-let ae
                 (list n) (list (build-application ae
                                  (build-primref ae 'length)
                                  (list (build-lexical-reference ae g))))
                 (let f ((vars* vars*) (exp* exp*))
                   (if (null? vars*)
                       (build-error ae)
                       (build-conditional ae
                         (build-pred ae n (car vars*))
                         (build-apply ae g (car vars*) (car exp*))
                         (f (cdr vars*) (cdr exp*)))))))))
       (if (= (length exp*) 1)
           (build-lambda ae (car vars*) (car exp*))
           (expand-case-lambda ae vars* exp*)))))

  (define build-let
    (lambda (ae lhs* rhs* body)
      (build-application ae (build-lambda ae lhs* body) rhs*)))

  (define-syntax build-primref
    (syntax-rules ()
      ((_ ae name)
       (build-primref ae 1 name))
      ((_ ae level name)
       (if-wants-record-exprs
        (make-expr-primref (maybe-annotation-source ae) level name)
        `(primitive ,name)))))

  (define-syntax build-foreign-call
    (syntax-rules ()
      ((_ ae name arg*) `(foreign-call ,name . ,arg*))))

  (define-syntax build-data
    (syntax-rules ()
      ((_ ae exp)
       (if-wants-record-exprs
        (make-expr-data (maybe-annotation-source ae) exp)
        `',data))))

  (define build-sequence
    (lambda (ae exps)
      (let loop ((exps exps))
        (if (null? (cdr exps))
            (car exps)
            (if (and (expr-primref? (car exps))
                     (eq? (expr-primref-name (car exps)) 'void))
                (loop (cdr exps))
                (if-wants-record-exprs
                 (make-expr-sequence (maybe-annotation-source ae) exps)
                 `(begin ,@exps)))))))

  (define build-void
    (lambda (ae)
      (build-primref ae 'void)))

  (define build-letrec
    (lambda (ae vars val-exps body-exp)
      (if (null? vars)
          body-exp
          (if-wants-record-exprs
           (make-expr-letrec (maybe-annotation-source ae) vars val-exps body-exp)
           `(letrec ,(map list vars val-exps) ,body-exp)))))

  (define build-letrec*
    (lambda (ae vars val-exps body-exp)
      (cond
        ((null? vars) body-exp)
        (else
         (if-wants-letrec*
          (if-wants-record-exprs
           (make-expr-letrec* (maybe-annotation-source ae) vars val-exps body-exp)
           `(letrec* ,(map list vars val-exps) ,body-exp))
          (build-let ae vars (map (lambda (x) (build-data ae #f)) vars)
            (build-sequence ae
              (append
               (map
                (lambda (lhs rhs)
                  (build-lexical-assignment ae
                    lhs (build-lexical-reference ae rhs)))
                vars val-exps)
               (list body-exp)))))))))

  (define build-library-letrec*
    (lambda (ae mix? name vars locs val-exps body-exp)
      (if-wants-library-letrec*
       (if-wants-record-exprs
        (make-expr-library-letrec* (maybe-annotation-source ae)
                                   mix? name vars locs val-exps body-exp)
        `(library-letrec* ,(map list vars locs val-exps) ,body-exp))
        (build-letrec* ae vars val-exps
          (if mix?
              body-exp
              (build-sequence ae
                (cons body-exp
                  (map
                    (lambda (var loc)
                      (build-global-assignment ae
                        loc (build-lexical-reference ae var)))
                    vars locs)))))))))

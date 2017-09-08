;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011-2017 Göran Weinholt <goran@weinholt.se>
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

;; R6RS linter -- letrec optimization pass.

;; Implements the transformation described by Ghuloum and Dybvig in
;; "Fixing Letrec (reloaded)" (2009).

;; rec and rec* are replaced with bind, mutate and fix.

(library (r6lint lib letrec)
  (export pass-fix-letrec)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (r6lint lib expressions)
    (r6lint lib messages))

  (define (iota n)
    (let lp ((i 0))
      (if (fx=? n i)
          '()
          (cons i (lp (fx+ i 1))))))

  (define-record-type vertex
    (sealed #t) (opaque #t) (nongenerative)
    (fields lhs rhs idx (mutable adjacency) (mutable number) (mutable lowlink))
    (protocol
     (lambda (p)
       (lambda (lhs rhs idx)
         (p lhs rhs idx '() -1 -1)))))

  (define (vertex-edge! v w)
    (vertex-adjacency-set! v (cons w (vertex-adjacency v))))

  ;; Robert Tarjan's algorithm for finding strongly connected
  ;; components in a graph.
  (define (tarjan! vertices)
    (let ((ret '())
          (i 0)
          (stack '()))                  ;empty stack of points
      (define (strong-connect! v)
        (define (span! pred? head)
          (let lp ((tail head) (prev #f))
            (cond ((null? tail)
                   (values head tail))
                  ((pred? (car tail))
                   (lp (cdr tail) tail))
                  ((not prev)
                   (values '() head))
                  (else
                   (set-cdr! prev '())
                   (values head tail)))))
        (set! i (+ i 1))
        (vertex-lowlink-set! v i)
        (vertex-number-set! v i)
        (set! stack (cons v stack))
        (for-each (lambda (w)
                    (cond ((= (vertex-number w) -1)
                           ;; (v,w) is a tree arc
                           (strong-connect! w)
                           (vertex-lowlink-set! v (min (vertex-lowlink v)
                                                       (vertex-lowlink w))))
                          ((and (< (vertex-number w) (vertex-number v))
                                ;; (v,w) is a frond or cross-link
                                (memq w stack))
                           (vertex-lowlink-set! v (min (vertex-lowlink v)
                                                       (vertex-number w))))))
                  (vertex-adjacency v))
        (when (= (vertex-lowlink v) (vertex-number v))
          ;; v is the root of a component; start the new strongly
          ;; connected component
          (let-values (((scc stack*)
                        (span! (lambda (w)
                                 (>= (vertex-number w) (vertex-number v)))
                               stack)))
            ;; scc is a strongly connect component.
            (set! stack stack*)
            (set! ret (cons scc ret)))))
      (for-each (lambda (w)
                  (when (= (vertex-number w) -1)
                    (strong-connect! w)))
                vertices)
      (reverse ret)))

  ;; From fixing letrec:

  ;; unreferenced: if x is unreferenced, else
  ;; simple: if x is unassigned and e is a simple expression, else
  ;; lambda: if x is unassigned and e is a lambda expression, and
  ;; complex: if it does not fall into any of the other categories.

  (define (unreferenced? id)
    (not (variable-referenced? id)))

  (define (unassigned? id)
    (not (variable-mutated? id)))

  ;; Does lhs appear free in x?
  (define (free-variable? lhs x)
    (define who 'free-variable?)
    (define (free-variable? lhs x)
      (cond ((bind? x)
             (or (exists (lambda (x) (free-variable? lhs x))
                         (bind-rhs* x))
                 (free-variable? lhs (bind-body x))))
            ((rec*? x)
             (or (exists (lambda (x) (free-variable? lhs x))
                         (rec*-rhs* x))
                 (free-variable? lhs (rec*-body x))))
            ((rec? x)
             (or (exists (lambda (x) (free-variable? lhs x))
                         (rec-rhs* x))
                 (free-variable? lhs (rec-body x))))
            ((fix? x)
             (or (exists (lambda (x) (free-variable? lhs x))
                         (fix-rhs* x))
                 (free-variable? lhs (fix-body x))))
            ((proc? x)
             (exists (lambda (x)
                       (free-variable? lhs (proccase-body x)))
                     (proc-case* x)))
            ((seq? x)
             (or (free-variable? lhs (seq-e0 x))
                 (free-variable? lhs (seq-e1 x))))
            ((mutate? x)
             (or (eq? (mutate-name x) lhs)
                 (free-variable? lhs (mutate-expr x))))
            ((test? x)
             (or (free-variable? lhs (test-expr x))
                 (free-variable? lhs (test-then x))
                 (free-variable? lhs (test-else x))))
            ((funcall? x)
             (or (free-variable? lhs (funcall-operator x))
                 (exists (lambda (x) (free-variable? lhs x))
                         (funcall-operand* x))))
            ((const? x) #f)
            ((ref? x) (eq? lhs (ref-name x)))
            ((primref? x) #f)
            (else
             (error who "Unknown type" x))))
    (if (not (variable-referenced? lhs))
        #f
        (free-variable? lhs x)))

  (define (might-cause-side-effects? x)
    ;; Is it possible that the expression x causes side effects?
    (define (P x)
      (cond ((bind? x)
             (or (exists P (bind-rhs* x))
                 (P (bind-body x))))
            ((rec*? x)
             (or (exists P (rec*-rhs* x))
                 (P (rec*-body x))))
            ((rec? x)
             (or (exists P (rec-rhs* x))
                 (P (rec-body x))))
            ((fix? x)
             (or (exists P (fix-rhs* x))
                 (P (fix-body x))))
            ((proc? x)
             #f)
            ((seq? x)
             (or (P (seq-e0 x))
                 (P (seq-e1 x))))
            ((mutate? x)
             #t)
            ((test? x)
             (or (P (test-expr x))
                 (P (test-then x))
                 (P (test-else x))))
            ((funcall? x)
             ;; TODO: Be less conservative. cons with two arguments
             ;; never has a side-effect unless the arguments might
             ;; have side-effects.
             #t)
            ((const? x) #f)
            ((ref? x) #f)
            ((primref? x) #f)
            (else
             (error 'might-cause-side-effects? "Unknown expression type" x))))
    (P x))

  (define (make-fixes location vertices body)
    (if (null? vertices)
        body
        (make-fix location
                  (map vertex-lhs vertices)
                  (map vertex-rhs vertices)
                  body)))

  (define (make-mutations location vertices body emit)
    (let lp ((V vertices))
      (if (null? V)
          body
          (let ((var (vertex-lhs (car V)))
                (init (vertex-rhs (car V))))
            (when (not (variable-mutated? var))
              (expr-emit emit var 'refactor 'complex-binding
                         (string-append
                          "Complex bindning: "
                          (symbol->string (variable-name var))))
              (variable-mutated?-set! var (or (variable-export-name var) #t)))
            (make-seq location (make-mutate location var init)
                      (lp (cdr V)))))))

  (define (unassigned-procedure? b)
    ;; XXX: There are some easy ways to mess up this analysis, e.g.
    ;; by putting in a begin
    (and (unassigned? (vertex-lhs b))
         (proc? (vertex-rhs b))))

  (define (fix1 location scc fixes body let-type emit)
    ;; body is the body to be used in the fix, let, etc
    (if (null? (cdr scc))
        ;; An SCC containing one binding
        (let ((b (car scc)))
          (let ((var (vertex-lhs b))
                (init (vertex-rhs b)))
            (cond ((unassigned-procedure? b)
                   ;; "If init is a lambda, and var is unassigned"
                   (values (cons b fixes) body))
                  ((not (free-variable? var init))
                   ;; "If var is not free in init". Consumes the
                   ;; fixes previously saved up.
                   (values '()
                           (make-bind location
                                      (list var)
                                      (list init)
                                      (make-fixes location fixes body))))
                  (else
                   ;; "Otherwise, we resort to assignment". Also
                   ;; consumes any fixes saved up.
                   (values '()
                           (make-bind location
                                      (list var)
                                      (list (make-void location))
                                      (make-mutations location
                                                      (list b)
                                                      (make-fixes location fixes body)
                                                      emit)))))))
        ;; An SCC with multiple bindings.
        (let-values (((l* c*) (partition unassigned-procedure? scc)))
          ;; l* has lambda bindings, c* has complex bindings
          (if (null? c*)
              ;; <var_λ,init_λ> if init is a lambda expression and
              ;; var is unassigned.
              (values (append l* fixes) body)
              (let ((c* (if (eq? let-type 'letrec*)
                            (list-sort (lambda (a b)
                                         (< (vertex-idx a) (vertex-idx b)))
                                       c*)
                            c*)))
                ;; <var_c,init_c> otherwise.
                (values '()
                        (make-bind location
                                   (map vertex-lhs c*)
                                   (map (lambda (_) (make-void location)) c*)
                                   (make-fixes location (append l* fixes)
                                               (make-mutations location c* body emit)))))))))

  (define (fixing location scc* body let-type emit)
    (let lp ((scc* scc*))
      (if (null? scc*)
          (values '() body)
          (let-values (((fixes body) (lp (cdr scc*))))
            (fix1 location (car scc*) fixes body let-type emit)))))

  (define (letrec-dependencies V)
    (for-each (lambda (v)
                (for-each
                 (lambda (w)
                   (when (and (not (eq? w v))
                              (free-variable? (vertex-lhs v)
                                              (vertex-rhs w)))
                     (vertex-edge! w v)))
                 V))
              V))

  (define (letrec*-dependencies V)
    ;; Add letrec* dependencies
    (let lp ((w V))
      (cond ((null? w))
            ((pair? (cdr w))
             (let ((xj (cadr w))
                   (xi (car w)))
               ;; TODO: only needed if xi might have a side-effect?
               (when (might-cause-side-effects? (vertex-rhs xi))
                 (vertex-edge! xj xi)))
             (lp (cdr w))))))

  (define (fixing-letrec* location lhs* rhs* body emit)
    (let ((V (map make-vertex lhs* rhs* (iota (length lhs*)))))
      (letrec-dependencies V)
      (letrec*-dependencies V)
      (let-values (((fixes body) (fixing location (tarjan! V) body 'letrec* emit)))
        (make-fixes location fixes body))))

  (define (fixing-letrec location lhs* rhs* body emit)
    (let ((V (map make-vertex lhs* rhs* (iota (length lhs*)))))
      (letrec-dependencies V)
      (let-values (((fixes body) (fixing location (tarjan! V) body 'letrec emit)))
        (make-fixes location fixes body))))

  (define (pass-fix-letrec x emit)
    (define who 'pass-letrec)
    (define (pass x)
      (cond ((rec*? x)
             (pass (fixing-letrec* (expr-location x)
                                   (rec*-lhs* x)
                                   (rec*-rhs* x)
                                   (rec*-body x)
                                   emit)))
            ((rec? x)
             (pass (fixing-letrec (expr-location x)
                                  (rec-lhs* x)
                                  (rec-rhs* x)
                                  (rec-body x)
                                  emit)))
            ((fix? x)
             (make-fix (expr-location x)
                       (fix-lhs* x)
                       (map pass (fix-rhs* x))
                       (pass (fix-body x))))
            ((bind? x)
             (if (null? (bind-lhs* x))
                 (pass (bind-body x))
                 (make-bind (expr-location x)
                            (bind-lhs* x)
                            (map pass (bind-rhs* x))
                            (pass (bind-body x)))))
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
            ((funcall? x)
             (make-funcall (expr-location x)
                           (pass (funcall-operator x))
                           (map pass (funcall-operand* x))
                           (funcall-tail? x)))
            ((const? x) x)
            ((ref? x) x)
            ((primref? x) x)
            (else
             (error who "Unknown type" x))))
    (pass x)))

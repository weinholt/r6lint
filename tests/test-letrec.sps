#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>

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

(import
  (r6lint lib expander)
  (r6lint lib reader)
  (r6lint lib expressions)
  (only (r6lint lib analyser)
        pass-recover-let
        pass-referenced-variables
        pass-fix-letrec)
  (r6lint tests check)
  (rnrs (6)))

(letrec ()
  (define (process-library input)
    (define filename "<test>")
    (define errors '())
    (define (emit filename line col level id _message)
      (set! errors (cons (vector filename line col level id) errors)))
    (let ((input
           (call-with-string-output-port
             (lambda (p)
               (display "#!r6rs\n" p)
               (write input p)
               (newline p)))))
      (let ((reader (make-reader (open-string-input-port input) filename)))
        (let ((form (read-annotated reader)))
          (let-values (((name* code*) (expand-libraries filename (list form))))
            (let lp ((name* name*) (code* code*))
              (let ((code (car code*)))
                (cond ((null? (cdr code*))
                       (let* ((rec (code->records code))
                              (rec (pass-recover-let rec emit))
                              (rec (pass-referenced-variables rec))
                              (rec (pass-fix-letrec rec emit)))
                         (records->core rec)))
                      (else
                       (lp (cdr name*) (cdr code*)))))))))))

  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (let ()
         (define q 8)
         (define f (lambda (x) (+ x q)))
         (define r (f q))
         (define s (+ r (f 2)))
         (define g (lambda () (+ r s)))
         (define t (g))
         t)))
   =>
   '(let ([q 8])
      (fix ([f (lambda (x) (+ x q))])
           (let ([r (f q)])
             (let ([s (+ r (f 2))])
               (fix ([g (lambda () (+ r s))])
                    (let ([t (g)])
                      t)))))))


  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (letrec ([x (list (lambda () x))])
         x)))
   => '(let ([x (void)])
         (begin
           (set! x (list (lambda () x)))
           x)))

  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (let ()
         (define x (list (lambda () y)))
         (define f (lambda () (cons x y)))
         (define y (list (lambda () x)))
         (define t (f))
         t)))
   => '(let ((x (void))
             (y (void)))
         (fix ((f (lambda () (cons x y))))
           (begin
             (set! x (list (lambda () y)))
             (begin
               (set! y (list (lambda () x)))
               (let ((t (f)))
                 t))))))

  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (let ()
         (letrec ((x (list (lambda () y)))
                  (f (lambda () (cons x y)))
                  (y (list (lambda () x)))
                  (t (f)))
           t))))
   => '(let ((y (void))
             (x (void)))
         (fix ((f (lambda () (cons x y))))
           (begin
             (set! y (list (lambda () x)))
             (begin
               (set! x (list (lambda () y)))
               (let ((t (f)))
                 t))))))

  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (define x 12)
       (define (f i) i)
       (define (g i) i)
       (let ()
         (define y (f x))
         (define z (g x))
         (list y z))))
   => '(let ((x 12))
         (fix ((f (lambda (i) i))
               (g (lambda (i_1) i_1)))
           (let ((y (f x)))
             (let ((z (g x)))
               (list y z))))))

  (check
   (process-library
    '(library (x)
       (export)
       (import (rnrs))
       (letrec* ((y (lambda (x) (set! y x)))
                 (z (lambda () 123))
                 (g (list z)))
         (list y z g))))
   => '(let ((y (void)))
         (begin
           (set! y (lambda (x) (set! y x)))
           (fix ((z (lambda () 123)))
             (let ((g (list z)))
               (list y z g)))))))

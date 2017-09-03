#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
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

(import (r6lint lib linter)
        (r6lint psyntax compat)
        (r6lint psyntax library-manager)
        (r6lint tests check)
        (rnrs (6)))

(define (lint-it input)
  (let ((input (if (string? input)
                   input
                   (call-with-string-output-port
                     (lambda (p)
                       (display "#!r6rs\n" p)
                       (write input p)
                       (newline p)))))
        (errors '()))
    (letrec ((emit
              (lambda (filename line col level id _message)
                ;; (write (vector line col level id _message) (current-error-port))
                ;; (newline (current-error-port))
                (set! errors (cons (vector filename line col level id) errors)))))
      (lint "<test>" (open-string-input-port input) emit))
    (reverse errors)))

;;; Programs
(letrec ()
  (check (lint-it "#!r6rs\n") =>
         '(#("<test>" 1 0 error file-empty)))

  (check (lint-it "(display \"hello world\")\n") =>
         '(#("<test>" 1 0 error unknown-filetype)))

  (check (lint-it "#!/usr/bin/env scheme-script\n") =>
         '(#("<test>" 1 0 error top-level-import-missing)))

  (check (lint-it "#!/usr/bin/env scheme-script\n#!r6rs\n(import (rnrs))") =>
         '())

  (check (lint-it " #!/usr/bin/env scheme-script\n") =>
         '(#("<test>" 1 1 error lexical-violation)
           #("<test>" 1 0 error top-level-import-missing)))

  (check (lint-it "#!/usr/bin/env scheme-script\n#!r6rs\n(display \"Hello world\")") =>
         '(#("<test>" 1 0 error top-level-import-missing)))

  ;; FIXME: source location for the error
  ;; (check (lint-it "(import (nonexistent-library))") =>
  ;;        '(#("<test>" 1 8 error library-not-found)))

  )

;;; Libraries
(letrec ()
  (check (lint-it '(library (foo)
                     (export)
                     (import)))
         => '())
  (check (lint-it '(library (foo)
                     (export)
                     (import)
                     #vu8()))
         => '())
  (check (lint-it '(library (foo)
                     (export)
                     (import (rnrs))))
         => '())
  (check (lint-it '(library (foo)
                     (export foo)
                     (import (rnrs))
                     (define foo 1)))
         => '())
  (parameterize ((library-path '("..")))
    ;; Verify that the expander can use code from loaded libraries.
    (check (lint-it '(library (foo)
                       (export foo)
                       (import (rnrs) (r6lint tests check))
                       (define-syntax foo
                         (lambda (x)
                           (syntax-case x ()
                             ((_)
                              (let lp ((n 0))
                                (if (check-passed? n)
                                    n
                                    (lp (+ n 1))))))))
                       (foo)))
           => '()))
  ;; Verify that the expander can expand file-options.
  (check (lint-it '(library (foo)
                     (export)
                     (import (rnrs))
                     (display (file-options no-fail))))
         => '())
  ;; Verify that the expander can use file-options during expansion.
  (check (lint-it '(library (foo)
                     (export)
                     (import (rnrs))
                     (define-syntax foo
                       (lambda (x)
                         (syntax-case x ()
                           ((_)
                            (begin
                              (when (file-exists? "/dev/null")
                                (call-with-port
                                    (open-file-input-port "/dev/null"
                                                          (file-options no-fail))
                                  (lambda (p) #f)))
                              #f)))))
                     (foo)))
         => '())

  (check (lint-it "#!r6rs
(library (foo)
  (export)
  (import (rnrs))
  (display 123))\n")
         => '())

  (check (lint-it "#!r6rs
(library (foo)
  (export)
  (import (rnrs))
  test)\n")
         => '(#("<test>" 5 2 error unbound-identifier)))

  (check (lint-it "#!r6rs
(library (foo)
  (export unbound-export)
  (import (rnrs)))\n")
         =>
         '(#("<test>" 3 10 error unbound-export)))

  (check (lint-it "#!r6rs
(library (foo)
  (export)
  (import (rnrs)))
()")
         =>
         '(#("<test>" 5 0 convention library-trailing-data))))

;;; General syntax violations

(letrec ()
  (check (lint-it "
(library (foo)
  (export)
  (import (rnrs))
  (define-syntax foo
    (lambda (x)
      (syntax-case x ()
        ((_ n) #'(+ n 1)))))
  (foo))")
         => '(#("<test>" 9 2 error invalid-syntax))))

;;; General lexical violations

(letrec ()
  (check (lint-it
          "(import (rnrs))
(let ([x 1)) (display x))")
         ;; FIXME: Should be 2 11
         => '(#("<test>" 2 10 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
'\\x;")
         => '(#("<test>" 2 3 error lexical-violation)))
  ;; String inline hex escapes
  (check (lint-it
          "(import (rnrs))
(display \"#\\xd7ff;\")")
         => '())
  (check (lint-it
          "(import (rnrs))
(display \"#\\xd800;\")")
         => '(#("<test>" 2 13 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
(display \"#\\xd800;\")
(display \"#\\xd801;\")")
         => '(#("<test>" 2 13 error lexical-violation)
              #("<test>" 3 13 error lexical-violation)))
  ;; Unexpected EOF
  (check (lint-it
          "(import (rnrs))
(display \"#\\x")
         => '(#("<test>" 2 13 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
#\\")
         => '(#("<test>" 2 0 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
'(x . ")
         => '(#("<test>" 2 7 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
'x \\")
         => '(#("<test>" 2 3 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
'\\x")
         => '(#("<test>" 2 3 error lexical-violation)))
  (check (lint-it
          "(import (rnrs))
`")
         => '(#("<test>" 2 1 error lexical-violation))))

;;; Lexical style checks

(letrec ()
  (check (lint-it "(library (foo) (export) (import))")
         => '(#("<test>" 1 33 convention no-newline-eof)))

  (check (lint-it "(library(foo) (export) (import))\n")
         => '(#("<test>" 1 8 convention no-space-before-paren)))

  (check (lint-it "(library (foo)(export) (import))\n")
         => '(#("<test>" 1 14 convention no-space-before-paren)))

  (check (lint-it
          "(library (foo)\n  (export)\n  (import)\n)\n")
         => '(#("<test>" 4 0 convention hanging-brace)))

  (check (lint-it
          "(library (foo)\n  (export) \n  (import))\n")
         => '(#("<test>" 2 12 convention trailing-whitespace)))

  (check (lint-it
          "(library (foo)\n  (export)\n  (import (rnrs))\n \n  '())\n")
         => '(#("<test>" 4 2 convention trailing-whitespace)))

  (check (lint-it
          "(library (foo)\n  (export)\n  (import (rnrs))\n\t\n  '())\n")
         => '(#("<test>" 4 2 convention trailing-whitespace)))

  ;; \f (#\page or ^L) is ok.
  (check (lint-it
          "(library (foo)\n  (export)\n  (import (rnrs))\n\f\n  '())\n")
         => '())

  (check (lint-it
          "(library (foo)
  (export)
  (import (rnrs))

  (define (test)
    (display 1)
    ;; (display 2)
    ))
")
         => '())

  (check (lint-it
          "(library (foo)
  (export foos)
  (import (rnrs))

  (define foos
    '#(((bool) foo? obj))))
")
         => '()))

;;; Code checks for programs

(letrec ()
  ;; FIXME: Get more accurate positions.
  (check (lint-it "(import (rnrs))\n\n(define (foo) #f)\n")
         => '(#("<test>" 3 0 refactor unused-variable)))

  (check (lint-it "(import (rnrs))\n\n(define (foo x) #f)\n(foo 1)\n")
         => '(#("<test>" 3 0 refactor unused-variable)))

  (check (lint-it "(import (rnrs))\n\n(guard (exn (else 'error)) 'ok)\n")
         => '(#("<test>" 3 0 refactor unused-variable)))

  (check (lint-it "(import (rnrs))\n\n(define (is-zero? y) (eq? y 0))\n(is-zero? 0)\n")
         => '(#("<test>" 3 21 warning bad-eq-argument)))

  (check (lint-it "(import (rnrs))\n\n(define (is-zero? y) (eqv? y \"zero\"))\n(is-zero? 0)\n")
         => '(#("<test>" 3 21 warning bad-eqv-argument)))

  (check (lint-it "(import (rnrs))\n\n(case 1 ((\"list\") 'list) (else #f))\n")
         => '(#("<test>" 3 0 warning bad-eqv-argument)))

  (check (lint-it "(import (rnrs))\n\n(case 1 ((\"1\" \"one\") 1) (else #f))\n")
         => '(#("<test>" 3 0 warning bad-memv-argument)))

  (check (lint-it "(import (rnrs))\n\n(memq 1 (list 1 2))\n")
         => '(#("<test>" 3 0 warning bad-memq-argument)))

  (check (lint-it "(import (rnrs))\n\n(memv \"x\" (list \"x\" \"y\"))\n")
         => '(#("<test>" 3 0 warning bad-memv-argument)))

  (check (lint-it "(import (rnrs))\n\n(eqv? (lambda (x) x) (lambda (x) x))\n")
         => '(#("<test>" 3 0 warning bad-eqv-argument)))

  (check (lint-it "(import (rnrs))\n\n(eqv? +nan.0 +nan.0)\n")
         => '(#("<test>" 3 0 warning bad-eqv-argument)))

  (check (lint-it "(import (rnrs))\n\n(eq? #\\a #\\a)\n")
         => '(#("<test>" 3 0 warning bad-eq-argument))))

;;; Code checks for libraries

(letrec ()
  (check (lint-it "(library (foo)
  (export foo)
  (import (rnrs))
(define (foo) 1))\n")
         => '())

  (check (lint-it "(library (foo)
  (export)
  (import (rnrs))
(define (foo _x y _z)
  (display y)))\n")
         => '())

  (check (lint-it "(library (foo)
  (export)
  (import (rnrs))
(define (foo x y z)
  (display y)))\n")
         ;; FIXME: Get more accurate positions.
         => '(#("<test>" 4 0 refactor unused-variable)
              #("<test>" 4 0 refactor unused-variable))))

;;; Side-effects of the let recovery pass

(letrec ()
  (check (lint-it "(import (rnrs))\n\n((lambda (x) x))\n")
         => '(#("<test>" 3 0 error wrong-arguments)))

  (check (lint-it "(import (rnrs))\n\n((case-lambda ((x . y) x)))\n")
         => '(#("<test>" 3 0 error wrong-arguments)))

  (check (lint-it "(import (rnrs))\n\n((case-lambda (x x)))\n")
         => '())

  (check (lint-it "(import (rnrs))\n\n((case-lambda ((x . y) (cons x y))) 'x)\n")
         => '())

  (check (lint-it "(import (rnrs))\n\n((case-lambda ((x . y) (cons x y))) 'x 'y)\n")
         => '())

  (check (lint-it "(import (rnrs))\n\n((case-lambda (x x) (() #f)))\n")
         ;; FIXME: more accurate position
         => '(#("<test>" 3 1 refactor unreachable-code))))


(check-report)
(exit (if (check-passed? 60) 0 1))

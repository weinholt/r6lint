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
              (lambda (filename line col level id message)
                ;; (write (vector line col level id message) (current-error-port))
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
         ;; FIXME: Should be column 3
         => '(#("<test>" 9 6 error invalid-syntax))))

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
         => '(#("<test>" 1 33 convention no-newline-eof))))

(check-report)
(exit (if (check-passed? 29) 0 1))

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
        (r6lint tests check)
        (rnrs (6)))

(define (lint-it input)
  (let ((errors '()))
    (letrec ((emit
              (lambda (filename line col level id message)
                (write (vector line col level id message))
                (newline)
                (set! errors (cons (vector filename line col level id) errors)))))
      (lint "<test>" (open-string-input-port input) emit))
    (reverse errors)))

;; Programs
(letrec ()
  (check (lint-it "#!r6rs\n") =>
         '(#("<test>" 1 0 error file-empty)))

  (check (lint-it "(display \"hello world\")\n") =>
         '(#("<test>" 1 0 error unknown-filetype)))

  (check (lint-it "#!/usr/bin/env scheme-script\n") =>
         '(#("<test>" 1 0 error top-level-import-missing)))

  (check (lint-it "#!/usr/bin/env scheme-script\n#!r6rs\n(import (rnrs))") =>
         '())

  )

;; Libraries
(letrec ()
  (check (lint-it "#!r6rs\n(library (foo) (export) (import (rnrs)))") =>
         '())

  (check (lint-it "#!r6rs\n\
(library (foo)\n\
  (export)\n\
  (import (rnrs))\n\
  (display 123))\n") =>
  '())

  (check (lint-it "#!r6rs\n\
(library (foo)\n\
  (export)\n\
  (import (rnrs))\n\
  test)\n") =>
         '(#("<test>" 0 51 error unbound-identifier)))

  )

(check-report)
(exit (if (check-passed? 7) 0 1))

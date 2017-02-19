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

;; R6RS linter.

(library (r6lint lib linter)
  (export lint)
  (import (r6lint lib expander)
          (r6lint lib reader)
          (r6lint psyntax library-manager)
          (rnrs (6)))

  ;; The main entry point of the linter. Lints an open file, emitting messages.
  (define (lint filename port emit)
    (guard (exn
            (else
             (emit filename 1 0 'fatal 'internal-error
                              "Internal error while linting the file")
             (for-each (lambda (condition)
                         (emit filename 1 0 'error 'internal-error condition))
                       (simple-conditions exn))))
      (let ((filetype (detect-scheme-file-type port)))
        (set-port-position! port 0)
        (case filetype
          ((r6rs-library)
           (lint-r6rs-library filename port emit))
          ((r6rs-top-level-program)
           (lint-r6rs-top-level-program filename port emit))
          ((empty)
           (emit filename 1 0 'warning #f "The file is empty"))
          (else
           (emit filename 1 0 'warning #f "Unable to detect the file type"))))))

  #;
  (define (expand-dummy-script filename)
    ;; This creates a script with a dummy import of the given library.
    ;; It's not really how it should be done.
    (let ((source (read-file filename)))
      (let ((lib (map annotation-stripped source)))
        (let ((forms (expand-top-level `((import ,(cadar lib))

                                         ))))
          (display forms (current-error-port))
          (newline (current-error-port))))))

  (define (lint-r6rs-library filename port emit)
    (read-port filename port)
    (let ((lexeme (get-lexeme port)))
      (unless (eof-object? lexeme)
        (emit filename 0 0 'convention 'library-trailing-data
              "Trailing data after the library form"))))

  (define (lint-r6rs-top-level-program filename port emit)
    (let* ((forms (read-port filename port))
           (stripped (map annotation-stripped forms)))
      (cond ((or (null? stripped)
                 (not (pair? (car stripped)))
                 (not (eq? (caar stripped) 'import)))
             (emit filename 1 0 'error 'top-level-import-missing
                   "Top-level program missing the import form")
             (expand-top-level `((import (rnrs) ,@forms))))
            (else
             (expand-top-level forms))))))

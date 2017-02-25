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

  (define *DEBUG* #f)

  ;; The main entry point of the linter. Lints an open file, emitting messages.
  (define (lint filename port emit)
    (guard (exn
            ((maybe-translate-condition emit exn))
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
          ((r6rs-program)
           (lint-r6rs-program filename port emit))
          ((empty)
           (emit filename 1 0 'error 'file-empty "The file is empty"))
          (else
           (emit filename 1 0 'error 'unknown-filetype "Unable to detect the filetype"))))))

  ;; Lint an R6RS library.
  (define (lint-r6rs-library filename port emit)
    (let ((reader (make-reader port filename)))
      (let ((form (read-annotated reader)))
        (expand-library (list form))
        (let ((lexeme (get-lexeme reader)))
          (unless (eof-object? lexeme)
            (reader-error reader "Trailing data after library"))))))

  ;; Lint an R6RS program.
  (define (lint-r6rs-program filename port emit)
    (let* ((forms (read-port port filename))
           (stripped (map annotation-stripped forms)))
      (cond ((or (null? stripped)
                 (not (pair? (car stripped)))
                 (not (eq? (caar stripped) 'import)))
             (emit filename 1 0 'error 'top-level-import-missing
                   "Top-level program missing the import form")
             (expand-top-level `((import (rnrs)) ,@forms)))
            (else
             (expand-top-level forms)))))

  ;; Translate the source-condition. The format of source-character is
  ;; constructed by read-annotated.
  (define (emit-with-source emit exn level id message)
    (cond ((source-condition? exn)
           (let ((filename (source-filename exn))
                 (line (source-line exn))
                 (column (source-column exn)))
             (emit filename line column level id message)))
          (else
           (emit "<no-source>" 1 0 level id message))))

  (define (->string datum)
    (call-with-string-output-port
      (lambda (p) (display datum p))))

  ;; Translates a condition raised during linting.
  (define (maybe-translate-condition emit exn)
    (when *DEBUG*
      (print-condition exn (current-error-port)))
    (cond
      ((and (who-condition? exn) (message-condition? exn)
            (eq? (condition-who exn) 'file-location)
            (string=? (condition-message exn) "cannot find library"))
       (emit-with-source emit exn 'error 'library-not-found
                         (string-append "Library not found: "
                                        (->string (car (condition-irritants exn))))))

      ((and (who-condition? exn) (message-condition? exn)
            (string=? (condition-message exn) "unbound identifier"))
       (emit-with-source emit exn 'error 'unbound-identifier
                         (string-append "Unbound identifier: "
                                        (symbol->string (condition-who exn)))))

      ((and (lexical-violation? exn) (message-condition? exn))
       (cond
         ((string=? (condition-message exn) "Trailing data after library")
          (emit-with-source emit exn 'convention 'library-trailing-data
                            (condition-message exn)))
         (else
          (emit-with-source emit exn 'error 'lexical-violation
                            (string-append "Lexical violation: "
                                           (condition-message exn))))))

      ((and (syntax-violation? exn))
       (cond
         ((and (message-condition? exn)
               (string=? (condition-message exn) "cannot export unbound identifier"))
          (emit-with-source emit exn 'error 'unbound-export
                            (string-append "Unbound export: "
                                           (->string (syntax-violation-form exn)))))
         (else
          (emit-with-source emit exn 'error 'invalid-syntax
                            (string-append "Invalid syntax, form: "
                                           (->string (syntax-violation-form exn))
                                           ", subform: "
                                           (->string (syntax-violation-subform exn)))))))

      ((source-condition? exn)
       ;; Fallthrough: an unknown condition with a source-condition.
       (print-condition exn (current-error-port))
       (emit-with-source emit exn 'error 'unknown-error
                         (if (message-condition? exn)
                             (string-append "Unknown error: "
                                            (condition-message exn))
                             "Unknown error")))
      (else #f)))

  ;; R6RS condition printer.
  (define (print-condition exn p)
    (define (write/limit datum p)
      (define limit 100)
      (let ((str (call-with-string-output-port
                   (lambda (p) (write datum p)))))
        (cond ((> (string-length str) limit)
               (display (substring str 0 limit) p)
               (display "…" p))
              (else
               (display str p)))))
    (display ";;; Exception:\n" p)
    (cond ((condition? exn)
           (let ((c* (simple-conditions exn)))
             (do ((i 1 (fx+ i 1))
                  (c* c* (cdr c*)))
                 ((null? c*))
               (let* ((c (car c*))
                      (rtd (record-rtd c)))
                 (display ";; " p) (display i p) (display ". " p)
                 (let loop ((rtd rtd))
                   (display (record-type-name rtd) p)
                   (cond ((record-type-parent rtd) =>
                          (lambda (rtd)
                            (unless (eq? rtd (record-type-descriptor &condition))
                              (display #\space p)
                              (loop rtd))))))
                 (let loop ((rtd rtd))
                   (do ((f* (record-type-field-names rtd))
                        (i 0 (fx+ i 1)))
                       ((fx=? i (vector-length f*))
                        (cond ((record-type-parent rtd) => loop)))
                     (display "\n;;     " p)
                     (display (vector-ref f* i) p)
                     (display ": " p)
                     (let ((x ((record-accessor rtd i) c)))
                       (cond ((and (eq? rtd (record-type-descriptor &irritants))
                                   (pair? x) (list? x))
                              (display #\( p)
                              (write/limit (car x) p)
                              (for-each (lambda (x)
                                          (display "\n                   " p)
                                          (write/limit x p))
                                        (cdr x))
                              (display #\) p))
                             (else
                              (write/limit x p)))))))
               (newline p))))
          (else
           (display ";;; A non-condition object was raised:\n" p)
           (write exn p)
           (newline p)))
    (display ";;; End of condition.\n" p)))

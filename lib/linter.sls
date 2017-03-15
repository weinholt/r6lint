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

  ;; The main entry point of the linter. Lints an input port, emitting messages.
  (define (lint filename port emit)
    (let ((emit (emit-filter-repeats emit)))
      (guard (con (else #f))
        (with-exception-handler
          (lambda (con)
            (cond
              ((maybe-translate-condition emit con))
              (else
               (unless *DEBUG*
                 (print-condition con (current-error-port)))
               (emit filename 1 0 'fatal 'internal-error "Error while linting the file")
               (for-each (lambda (condition)
                           (emit filename 1 0 'error 'internal-error condition))
                         (simple-conditions con))))
            (unless (warning? con)
              (raise con)))
          (lambda ()
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
                 (emit filename 1 0 'error 'unknown-filetype "Unable to detect the filetype")))))))))

  ;; Filter out repeated messages.
  (define (emit-filter-repeats emit)
    (let ((previous #f))
      (lambda current
        (unless (equal? current previous)
          (apply emit current)
          (set! previous current)))))

  ;; Lint an R6RS library.
  (define (lint-r6rs-library filename port emit)
    (let ((reader (make-reader port filename)))
      (reader-tolerant-set! reader #t)
      (let ((form (read-annotated reader)))
        (expand-library (list form))
        ;; Check what is after the closing brace of the library form.
        (let lp ((i 0))
          (let ((lexeme (get-lexeme reader)))
            (cond ((eof-object? lexeme)
                   (when (= i 0)
                     (reader-warning reader "No newline at end of file")))
                  ((and (pair? lexeme) (memq (car lexeme) '(comment whitespace)))
                   (lp (+ i 1)))
                  (else
                   (reader-warning reader "Trailing data after library form"))))))))

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

  (define (read-port p filename)
    (let ((reader (make-reader p filename)))
      (reader-tolerant-set! reader #t)
      (let f ()
        (let ((x (read-annotated reader)))
          (if (or (eof-object? x)
                  (and (annotation? x) (eof-object? (annotation-expression x))))
              '()
              (cons x (f)))))))

  ;; Translate the source-condition. The format of source-character is
  ;; constructed by read-annotated.
  (define (emit-with-source emit con level id message)
    (cond ((source-condition? con)
           (let ((filename (source-filename con))
                 (line (source-line con))
                 (column (source-column con)))
             (emit filename line column level id message)))
          (else
           (emit #f 1 0 level id message)))
    #t)

  (define (->string datum)
    (call-with-string-output-port
      (lambda (p) (display datum p))))

  ;; Translates a condition raised during linting.
  (define (maybe-translate-condition emit con)
    (when *DEBUG*
      (print-condition con (current-error-port)))
    (cond
      ((and (who-condition? con) (message-condition? con)
            (eq? (condition-who con) 'file-location)
            (string=? (condition-message con) "cannot find library"))
       (emit-with-source emit con 'error 'library-not-found ;XXX: should be fatal?
                         (string-append "Library not found: "
                                        (->string (car (condition-irritants con))))))

      ((and (who-condition? con) (message-condition? con)
            (string=? (condition-message con) "unbound identifier"))
       (emit-with-source emit con 'error 'unbound-identifier
                         (string-append "Unbound identifier: "
                                        (->string (condition-who con)))))

      ((and (lexical-violation? con) (message-condition? con))
       (cond
         ;; XXX: These need to be rethought. Can't add more and more here.
         ((string=? (condition-message con) "Trailing data after library form")
          (emit-with-source emit con 'convention 'library-trailing-data
                            (condition-message con)))
         ((string=? (condition-message con) "No newline at end of file")
          (emit-with-source emit con 'convention 'no-newline-eof
                            (condition-message con)))
         (else
          (emit-with-source emit con 'error 'lexical-violation
                            (condition-message con)))))

      ((and (syntax-violation? con))
       (cond
         ((and (message-condition? con)
               (string=? (condition-message con) "cannot export unbound identifier"))
          (emit-with-source emit con 'error 'unbound-export
                            (string-append "Unbound export: "
                                           (->string (syntax-violation-form con)))))
         (else
          (emit-with-source emit con 'error 'invalid-syntax
                            (string-append "Form: "
                                           (->string (syntax-violation-form con))
                                           ", subform: "
                                           (->string (syntax-violation-subform con)))))))

      ((source-condition? con)
       ;; Fallthrough: an unknown condition with a source-condition.
       (unless *DEBUG*
         (print-condition con (current-error-port)))
       (emit-with-source emit con 'error 'unknown-error
                         (if (message-condition? con)
                             (string-append "Unknown error: "
                                            (condition-message con))
                             "Unknown error")))
      (else #f)))

  ;; R6RS condition printer.
  (define (print-condition con p)
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
    (cond ((condition? con)
           (let ((c* (simple-conditions con)))
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
           (write con p)
           (newline p)))
    (display ";;; End of condition.\n" p)))

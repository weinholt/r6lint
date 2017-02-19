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

;; R6RS lexer and reader with source annotations.

(library (r6lint lib reader)
  (export get-lexeme
          detect-scheme-file-type
          make-reader
          read-file

          read-annotated
          annotation? annotation-expression annotation-stripped annotation-source)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs control)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs lists)
          (only (rnrs io ports) lookahead-char get-char get-line
                port-has-port-position? port-position eof-object?
                current-input-port)

          (only (rnrs io simple) display newline ;debugging
                call-with-input-file with-input-from-file)
          (rnrs records syntactic)
          (rnrs unicode))

  ;; Detects the type of Scheme source: r6rs-library, r6rs-top-level-program, empty or unknown.
  (define (detect-scheme-file-type port)
    (let lp ()
      (let ((lexeme (get-lexeme port)))
        (cond ((eof-object? lexeme)
               'empty)
              ((and (pair? lexeme) (eq? (car lexeme) 'shebang))
               'r6rs-top-level-program)
              ((and (pair? lexeme) (eq? (car lexeme) 'directive))
               (lp))
              ((memq lexeme '(openp openb)) ;a pair
               (let ((lexeme (get-lexeme port)))
                 (cond ((and (pair? lexeme) (eq? (car lexeme) 'identifier))
                        (case (cdr lexeme)
                          ((import) 'r6rs-top-level-program)
                          ((library) 'r6rs-library)
                          (else 'unknown))))))
              (else 'unknown)))))

  (define-record-type reader
    (fields port)
    (sealed #t) (opaque #f) (nongenerative)
    (protocol
     (lambda (p)
       (lambda (port)
         (p port)))))

  ;; As wanted by psyntax
  (define-record-type annotation
    (fields expression source stripped)
    (sealed #t) (opaque #f) (nongenerative))

  (define read-annotated
    (case-lambda
      (()
       (read-annotated (current-input-port)))
      ((reader)
       (let ((p (if (reader? reader) (reader-port reader) reader)))
         (define (annotate port stripped datum)
           (make-annotation datum (port-position port) stripped))
         (define (annotate-not port stripped datum)
           (make-annotation datum #f stripped))
         (let-values (((d d*) (handle-lexeme p (get-lexeme p)
                                             ;; TODO: should always annotate
                                             (if (port-has-port-position? p)
                                                 annotate
                                                 annotate-not))))
           d*)))))

  (define (get-datum p)
    (let-values (((d d*) (handle-lexeme p (get-lexeme p) (lambda x #f))))
      d))

  (define (read-file filename)
    (call-with-input-file filename
      (lambda (p)
        (let ((reader (make-reader p)))
          (let f ()
            (let ((x (read-annotated reader)))
              (if (eof-object? x)
                  '()
                  (cons x (f)))))))))

;;; Lexeme reader

  (define lerror
    (case-lambda
      ((port msg . irritants)
       ;; TODO: add a &source-position condition.
       (raise
         (condition (make-lexical-violation)
                    (make-message-condition msg)
                    (make-irritants-condition
                     (cons port
                           (if (port-has-port-position? port)
                               (cons (port-position port) irritants)
                               irritants)))
                    #;
                    (make-i/o-read-error))))))

  (define (get-char-skipping-whitespace p)
    (let ((c (get-char p)))
      (cond ((eof-object? c) c)
            ((char-whitespace? c)
             (get-char-skipping-whitespace p))
            (else c))))

  (define (char-delimiter? c)
    ;; Treats the eof-object as a delimiter
    (or (eof-object? c)
        (memv c '(#\( #\) #\[ #\] #\" #\; #\#))
        (char-whitespace? c)))

  (define (get-inline-hex-escape p)
    (let lp ((digits '()))
      (let ((c (get-char p)))
        (cond ((eof-object? c)
               (lerror p "eof in escape"))
              ((or (char<=? #\0 c #\9)
                   (char-ci<=? #\a c #\f))
               (lp (cons c digits)))
              ((char=? c #\;)
               (guard (_ (else
                          ;; XXX: do this without guard
                          (lerror p "invalid character in escape"
                                  (list->string (reverse digits)))))
                 (integer->char (string->number (list->string (reverse digits)) 16))))
              (else (lerror p "invalid escape" c))))))

  (define (get-identifier p initial-chars)
    (let lp ((chars initial-chars))
      (let ((c (lookahead-char p)))
        (cond ((char-delimiter? c)
               (cons 'identifier (string->symbol (list->string (reverse chars)))))
              ((or (char-ci<=? #\a c #\Z)
                   (char<=? #\0 c #\9)
                   (memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                             #\+ #\- #\. #\@))
                   (and (> (char->integer c) 127)
                        (memq (char-general-category c)
                              '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
                                   Nd Mc Me))))
               (lp (cons (get-char p) chars)))
              ((char=? c #\\)           ;\xUUUU;
               (get-char p)             ;consume #\\
               (unless (eqv? #\x (lookahead-char p))
                 (lerror p "invalid character following \\"))
               (get-char p)             ;consume #\x
               (lp (cons (get-inline-hex-escape p) chars)))
              (else
               (lerror p "invalid character in identifier" c))))))

  (define (get-number p initial-chars)
    (let lp ((chars initial-chars))
      (let ((c (lookahead-char p)))
        (cond ((and (not (eqv? c #\#))
                    (char-delimiter? c))
               ;; XXX: some standard numbers are not supported
               ;; everywhere, should use a number lexer.
               (or (string->number (list->string (reverse chars)))
                   (lerror p "not a proper number"
                           (list->string (reverse chars)))))
              (else
               (lp (cons (get-char p) chars)))))))

  (define (get-string p)
    (let lp ((chars '()))
      (let ((c (lookahead-char p)))
        (cond ((eof-object? c)
               (lerror p "end of file in the middle of a string"))
              ((char=? c #\")
               (get-char p)
               (list->string (reverse chars)))
              ((char=? c #\\)           ;escapes
               (get-char p)             ;consume #\\
               (let ((c (lookahead-char p)))
                 (cond ((eof-object? c)
                        (lerror p "end of file in escape"))
                       ((or (memq c '(#\tab #\linefeed #\x0085 #\x2028))
                            (eq? (char-general-category c) 'Zs))
                        ;; \<intraline whitespace>*<line ending>
                        ;; <intraline whitespace>*
                        (letrec ((skip-intraline-whitespace*
                                  (lambda ()
                                    (let ((c (lookahead-char p)))
                                      (cond ((eof-object? c)
                                             (lerror p "end of file in escape"))
                                            ((or (char=? c '#\tab)
                                                 (eq? (char-general-category c) 'Zs))
                                             (get-char p)
                                             (skip-intraline-whitespace*))))))
                                 (skip-newline
                                  (lambda ()
                                    (let ((c (get-char p)))
                                      ;; XXX: it appears that the port
                                      ;; transcoder is meant to
                                      ;; replace all these linefeeds
                                      ;; with #\linefeed.
                                      (cond ((eof-object? c) c)
                                            ((memv c '(#\linefeed #\x0085 #\x2028)))
                                            ((char=? c #\return)
                                             (when (memv (lookahead-char p)
                                                         '(#\linefeed #\x0085))
                                               (get-char p)))
                                            (else
                                             (lerror p "expected a line ending" c)))))))
                          (skip-intraline-whitespace*)
                          (skip-newline)
                          (skip-intraline-whitespace*)
                          (lp chars)))
                       (else
                        (lp (cons
                             (case (get-char p)
                               ((#\") #\")
                               ((#\\) #\\)
                               ((#\a) #\alarm)
                               ((#\b) #\backspace)
                               ((#\t) #\tab)
                               ((#\n) #\linefeed)
                               ((#\v) #\vtab)
                               ((#\f) #\page)
                               ((#\r) #\return)
                               ((#\x) (get-inline-hex-escape p))
                               (else
                                (lerror p "invalid escape in string" c)))
                             chars))))))
              (else
               (lp (cons (get-char p) chars)))))))

  (define (get-lexeme p)
    (let lp ((c (get-char-skipping-whitespace p)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\;)                 ;a comment like this one
         (let lp* ()
           (let ((c (get-char p)))
             (cond ((eof-object? c) c)
                   ((memv c '(#\linefeed #\x0085 #\x2028 #\x2029))
                    (get-lexeme p))
                   ((char=? c #\return)
                    ;; Weird line ending again.
                    (when (memv (lookahead-char p) '(#\linefeed #\x0085))
                      (get-char p))
                    (get-lexeme p))
                   (else (lp*))))))
        ((char=? c #\#)                 ;the mighty octothorpe
         (let ((c (get-char p)))
           (case c
             ((#\() 'vector)
             ((#\') '(abbrev . syntax))
             ((#\`) '(abbrev . quasisyntax))
             ((#\,)
              (case (lookahead-char p)
                ((#\@)
                 (get-char p)
                 '(abbrev . unsyntax-splicing))
                (else '(abbrev . unsyntax))))
             ((#\v)
              (unless (eqv? #\u (get-char p)) (lerror p "expected #vu8("))
              (unless (eqv? #\8 (get-char p)) (lerror p "expected #vu8("))
              (unless (eqv? #\( (get-char p)) (lerror p "expected #vu8("))
              'bytevector)
             ((#\;)                     ;s-expr comment
              (get-datum p)
              (get-lexeme p))
             ((#\|)                     ;nested comment
              (letrec ((skip
                        (lambda ()
                          (let lp ()
                            (let ((c (get-char p)))
                              (cond ((eof-object? c) (lerror p "end of file in #|-comment"))
                                    ((and (char=? c #\|) (eqv? (lookahead-char p) #\#))
                                     (get-char p))
                                    ((and (char=? c #\#) (eqv? (get-char p) #\|))
                                     (skip) (lp))
                                    (else (lp))))))))
                (skip)
                (get-lexeme p)))
             ((#\!)                     ;#!r6rs etc
              (if (memv (lookahead-char p) '(#\/ #\space))
                  (let ((position (- (port-position p) 2)))
                    `(shebang ,position . ,(get-line p)))  ; XXX: get-line is cheating
                  (let ((id (get-lexeme p)))
                    (cond ((and (pair? id) (eq? (car id) 'identifier))
                           (case (cdr id)
                             ((r6rs)
                              ;; r6rs.pdf
                              'TODO)
                             ((fold-case)
                              ;; r6rs-app.pdf
                              (lerror p "TODO: The #!fold-case directive is not supported"))
                             ((no-fold-case)
                              ;; r6rs-app.pdf
                              #t)
                             (else
                              (lerror p "This is an unsupported directive" (cdr id))))
                           (cons 'directive (cdr id)))
                          (else
                           (lerror p "Expected an identifier after #!"))))))
             ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
              (get-number p (list c #\#)))
             ((#\t #\T)
              (unless (char-delimiter? (lookahead-char p))
                (lerror p "read as #t and expected a delimiter to follow"))
              #t)
             ((#\f #\F)
              (unless (char-delimiter? (lookahead-char p))
                (lerror p "read as #f and expected a delimiter to follow"))
              #f)
             ((#\\)
              (let lp ((chars '()))
                (let ((c (lookahead-char p)))
                  (cond ((and (pair? chars) (char-delimiter? c))
                         (let ((chars (reverse chars)))
                           (cond ((null? chars) (lerror p "empty character"))
                                 ((null? (cdr chars)) (car chars))
                                 ((char=? (car chars) #\x)
                                  (unless (for-all (lambda (c)
                                                     (or (char<=? #\0 c #\9)
                                                         (char-ci<=? #\a c #\f)))
                                                   (cdr chars))
                                    (lerror p "non-hex character in hex-escaped character"
                                            (list->string (cdr chars))))
                                  (let ((sv (string->number (list->string (cdr chars))
                                                           16)))
                                    (when (not (or (<= 0 sv #xD7FF)
                                                   (<= #xE000 sv #x10FFFF)))
                                      (lerror p "hex-escaped character outside valid range" sv))
                                    (integer->char sv)))
                                 (else
                                  (cond ((assoc (list->string chars)
                                                '(("nul" . #\nul)
                                                  ("alarm" . #\alarm)
                                                  ("backspace" . #\backspace)
                                                  ("tab" . #\tab)
                                                  ("linefeed" . #\linefeed)
                                                  ("newline" . #\linefeed)
                                                  ("vtab" . #\vtab)
                                                  ("page" . #\page)
                                                  ("return" . #\return)
                                                  ("esc" . #\esc)
                                                  ("space" . #\space)
                                                  ("delete" . #\delete)))
                                         => cdr)
                                        (else (lerror p "unknown character name"
                                                      (list->string chars))))))))
                        (else
                         (lp (cons (get-char p) chars)))))))
             (else (lerror p "unknown #-syntax" c)))))
        ((char=? c #\")
         (get-string p))
        ((memq c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (get-number p (list c)))
        ((memq c '(#\- #\+))            ;peculiar identifier
         (cond ((and (char=? c #\-) (eqv? #\> (lookahead-char p))) ;->
                (get-identifier p (list c)))
               ((char-delimiter? (lookahead-char p))
                (cons 'identifier (string->symbol (string c))))
               (else (get-number p (list c)))))
        ((char=? c #\.)                 ;peculiar identifier
         (cond ((eqv? #\. (lookahead-char p))
                (get-char p)            ;consume second dot
                (unless (eqv? #\. (get-char p)) ;consume third dot
                  (lerror p "expecting ... identifier"))
                (unless (char-delimiter? (lookahead-char p))
                  (lerror p "expecting ... identifier"))
                (cons 'identifier '...))
               ((char-delimiter? (lookahead-char p))
                'dot)
               (else
                (get-number p (list c)))))
        ((or (char-ci<=? #\a c #\Z) ;<constituent> and <special initial>
             (memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
             (and (> (char->integer c) 127)
                  (memq (char-general-category c)
                        '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))
         (get-identifier p (list c)))
        ((char=? c #\\)                 ;<inline hex escape>
         (unless (eqv? #\x (lookahead-char p))
           (lerror p "invalid character following \\"))
         (get-char p)                   ;consume #\x
         (get-identifier p (list (get-inline-hex-escape p))))
        (else
         (case c
           ((#\() 'openp)
           ((#\)) 'closep)
           ((#\[) 'openb)
           ((#\]) 'closeb)
           ((#\') '(abbrev . quote))
           ((#\`) '(abbrev . quasiquote))
           ((#\,)
            (case (lookahead-char p)
              ((#\@)
               (get-char p)
               '(abbrev . unquote-splicing))
              (else '(abbrev . unquote))))
           (else
            (lerror p "completely unexpected character" c)))))))

;;; Datum reader

  ;; <datum> → <lexeme datum>
  ;;          | <compound datum>
  ;; <lexeme datum> → <boolean> | <number>
  ;;          | <character> | <string> | <symbol>
  ;; <symbol> → <identifier>
  ;; <compound datum> → <list> | <vector> | <bytevector>
  ;; <list> → (<datum>*) | [<datum>*]
  ;;          | (<datum>+ . <datum>) | [<datum>+ . <datum>]
  ;;          | <abbreviation>
  ;; <abbreviation> → <abbrev prefix> <datum>
  ;; <abbrev prefix> → ’ | ‘ | , | ,@
  ;;          | #’ | #‘ | #, | #,@
  ;; <vector> → #(<datum>*)
  ;; <bytevector> → #vu8(<u8>*)
  ;; <u8> → 〈any <number> representing an exact
  ;;                    integer in {0, ..., 255}〉

  (define (get-compound-datum p terminator type annotate)
    (let lp ((data '()) (data* '()))
      (let ((x (get-lexeme p)))
        (cond ((eof-object? x)
               (lerror p "end of file in compound datum" terminator))
              ((eq? x terminator)
               (case type
                 ((vector)
                  (let ((s (list->vector (reverse data))))
                    (values s (annotate p s (list->vector (reverse data*))))))
                 ((list)
                  (let ((s (reverse data)))
                    (values s (annotate p s (reverse data*)))))
                 ((bytevector)
                  (let ((s (u8-list->bytevector (reverse data))))
                    (values s (annotate p s s))))
                 (else
                  (lerror p "Internal error in get-compound-datum"))))
              ((memq x '(closep closeb))
               (lerror p "mismatching closing parenthesis" x terminator))
              ((eq? x 'dot)
               (unless (eq? type 'list)
                 (lerror p "dot used in non-list datum"))
               (let-values (((x x*) (handle-lexeme p (get-lexeme p) annotate)))
                 (let ((t (get-lexeme p)))
                   (unless (eq? t terminator)
                     (lerror p "multiple dots in list" terminator t)))
                 (let ((s (append (reverse data) x)))
                   (values s (annotate p s (append (reverse data*) x*))))))
              (else
               (let-values (((d d*) (handle-lexeme p x annotate)))
                 (case type
                   ((bytevector)
                    (unless (and (number? d) (exact? d) (<= 0 d 255))
                      (lerror p "Invalid datum in bytevector" d))
                    (lp (cons d data) data*))
                   (else
                    (lp (cons d data) (cons d* data*))))))))))

  (define (handle-lexeme p x annotate)
    (case x
      ((openp)
       (get-compound-datum p 'closep 'list annotate))
      ((openb)
       (get-compound-datum p 'closeb 'list annotate))
      ((vector)
       (get-compound-datum p 'closep 'vector annotate))
      ((bytevector)
       (get-compound-datum p 'closep 'bytevector annotate))
      (else
       (cond ((or (char? x) (string? x) (boolean? x)
                  (number? x) (bytevector? x))
              (values x (annotate p x x)))
             ((eof-object? x)
              (values x x))
             ((and (pair? x) (eq? (car x) 'identifier))
              (values (cdr x) (annotate p (cdr x) (cdr x))))
             ((and (pair? x) (eq? (car x) 'abbrev))
              (let ((lex (get-lexeme p)))
                (when (eof-object? lex)
                  (lerror p "Unexpected end of file after abbreviation"))
                (let-values (((d d*) (handle-lexeme p lex annotate)))
                  (let ((s (list (cdr x) d)))
                    (values s (annotate p s (list (cdr x) d*)))))))
             ((and (pair? x) (eq? (car x) 'directive))
              ;; Ignore directives like #!r6rs at this level.
              (handle-lexeme p (get-lexeme p) annotate))
             ((and (pair? x) (eq? (car x) 'shebang) (zero? (cadr x)))
              ;; Ignore the shebang ("#!/" or "#! " at the start of files).
              ;; FIXME: should only work for programs.
              (handle-lexeme p (get-lexeme p) annotate))
             (else
              (lerror p "unexpected lexeme" x)))))))

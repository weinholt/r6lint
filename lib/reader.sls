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
  (export get-token
          detect-scheme-file-type
          make-reader reader-error reader-warning
          reader-tolerant reader-tolerant-set!
          reader-line reader-column
          reader-saved-line reader-saved-column
          read-annotated
          annotation? annotation-expression annotation-stripped annotation-source
          annotation-source->condition source-condition? source-filename
          source-line source-column)
  (import (rnrs base (6))
          (rnrs bytevectors (6))
          (rnrs control (6))
          (rnrs conditions (6))
          (rnrs arithmetic fixnums (6))
          (rnrs exceptions (6))
          (rnrs lists (6))
          (prefix (only (rnrs io ports (6)) lookahead-char get-char put-char eof-object?
                        call-with-string-output-port)
                  rnrs:)
          (only (rnrs io simple (6)) write display newline current-error-port) ;debugging
          (rnrs records syntactic (6))
          (rnrs unicode (6)))

  (define eof-object? rnrs:eof-object?)

  ;; Peek at the next char from the reader.
  (define (lookahead-char reader)
    (rnrs:lookahead-char (reader-port reader)))

  ;; Get a char from the reader.
  (define (get-char reader)
    (let ((c (rnrs:get-char (reader-port reader))))
      (when (eqv? c #\linefeed)
        (reader-line-set! reader (+ (reader-line reader) 1))
        (reader-column-set! reader -1))
      (reader-column-set! reader (+ (reader-column reader) 1))
      c))

  ;; Detects the (intended) type of Scheme source: r6rs-library,
  ;; r6rs-program, empty or unknown.
  (define (detect-scheme-file-type port)
    (let ((reader (make-reader port "<unknown>")))
      (let ((lexeme (get-lexeme reader)))
        (cond ((eof-object? lexeme)
               'empty)
              ((and (pair? lexeme) (eq? (car lexeme) 'shebang))
               'r6rs-program)
              ((memq lexeme '(openp openb)) ;a pair
               (let ((lexeme (get-lexeme reader)))
                 (cond ((and (pair? lexeme) (eq? (car lexeme) 'identifier))
                        (case (cdr lexeme)
                          ((import) 'r6rs-program)
                          ((library) 'r6rs-library)
                          (else 'unknown)))
                       (else 'unknown))))
              (else 'unknown)))))

  (define-record-type reader
    (fields port filename
            (mutable line) (mutable column)
            (mutable saved-line) (mutable saved-column)
            (mutable fold-case)         ;boolean
            (mutable mode)              ;default or r6rs (unused)
            (mutable tolerant))         ;tolerant to errors
    (sealed #t) (opaque #f) (nongenerative)
    (protocol
     (lambda (p)
       (lambda (port filename)
         (p port filename 1 0 1 0 #f 'default #f)))))

  (define (reader-mark reader)
    (reader-saved-line-set! reader (reader-line reader))
    (reader-saved-column-set! reader (reader-column reader)))

  ;; As wanted by psyntax
  (define-record-type annotation
    (fields expression source stripped)
    (sealed #t) (opaque #f) (nongenerative))

  (define-condition-type &source-information &condition
    make-source-condition source-condition?
    (file-name source-filename)
    (line source-line)
    (column source-column))

  (define (annotation-source->condition x)
    (if (vector? x)
        (apply make-source-condition (vector->list x))
        (condition)))

  (define (reader-source reader)
    (vector (reader-filename reader)
            (reader-saved-line reader)
            (reader-saved-column reader)))

  (define (annotate source stripped datum)
    #;(assert (reader? reader))
    (assert (vector? source))
    (make-annotation datum
                     source
                     stripped))

  (define (read-annotated reader)
    (assert (reader? reader))
    (let-values (((_ d^) (handle-lexeme reader (get-lexeme reader))))
      d^))

  (define (get-datum reader)
    (assert (reader? reader))
    (let-values (((d _) (handle-lexeme reader (get-lexeme reader))))
      d))

;;; Lexeme reader

  (define (lexical-condition reader msg irritants)
    (condition
     (make-lexical-violation)
     (make-message-condition msg)
     (make-source-condition (reader-filename reader)
                            (reader-saved-line reader)
                            (reader-saved-column reader))
     (make-irritants-condition irritants)))

  (define (reader-error reader msg . irritants)
    ;; Non-recoverable errors.
    (raise (lexical-condition reader msg irritants)))

  (define (reader-warning reader msg . irritants)
    ;; Recoverable if the reader is in tolerant mode.
    (if (reader-tolerant reader)
        (raise-continuable
          (condition
           (make-warning)
           (lexical-condition reader msg irritants)))
        (apply reader-error reader msg irritants)))

  (define (eof-warning reader)
    (reader-warning reader "Unexpected EOF"))

  (define (unicode-scalar-value? sv)
    (and (fx<=? 0 sv #x10FFFF)
         (not (fx<=? #xD800 sv #xDFFF))))

  (define (char-delimiter? c)
    ;; Treats the eof-object as a delimiter
    (or (eof-object? c)
        (memv c '(#\( #\) #\[ #\] #\" #\; #\#))
        (char-whitespace? c)))

  ;; Get a line from the reader.
  (define (get-line reader)
    (rnrs:call-with-string-output-port
      (lambda (out)
        (do ((c (get-char reader) (get-char reader)))
            ((or (eqv? c #\linefeed) (eof-object? c)))
          (rnrs:put-char out c)))))

  ;; Gets whitespace from the reader.
  (define (get-whitespace reader char)
    (rnrs:call-with-string-output-port
     (lambda (out)
       (let lp ((char char))
         (rnrs:put-char out char)
         (let ((char (lookahead-char reader)))
           (when (and (char? char) (char-whitespace? char))
             (lp (get-char reader))))))))

  ;; Get an inline hex escape (escaped character inside an identifier).
  (define (get-inline-hex-escape p)
    (reader-mark p)
    (let lp ((digits '()))
      (let ((c (get-char p)))
        (cond ((eof-object? c)
               (eof-warning p)
               #\xFFFD)
              ((or (char<=? #\0 c #\9)
                   (char-ci<=? #\a c #\f))
               (lp (cons c digits)))
              ((and (char=? c #\;) (pair? digits))
               (let ((sv (string->number (list->string (reverse digits)) 16)))
                 (cond ((unicode-scalar-value? sv)
                        (integer->char sv))
                       (else
                        (reader-warning p "Inline hex escape outside valid range" sv)
                        #\xFFFD))))
              (else
               (reader-warning p "Invalid inline hex escape" c)
               #\xFFFD)))))

  (define (get-identifier p initial-chars)
    (let lp ((chars initial-chars))
      (let ((c (lookahead-char p)))
        (cond ((char-delimiter? c)
               (let ((id (list->string (reverse chars))))
                 (if (reader-fold-case p)
                     (cons 'identifier (string->symbol (string-foldcase id)))
                     (cons 'identifier (string->symbol id)))))
              ((or (char-ci<=? #\a c #\Z)
                   (char<=? #\0 c #\9)
                   (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                             #\+ #\- #\. #\@))
                   (and (> (char->integer c) 127)
                        (memq (char-general-category c)
                              '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
                                   Nd Mc Me))))
               (lp (cons (get-char p) chars)))
              ((char=? c #\\)           ;\xUUUU;
               (get-char p)             ;consume #\\
               (let ((c (get-char p)))  ;should be #\x
                 (cond ((eqv? c #\x)
                        (lp (cons (get-inline-hex-escape p) chars)))
                       (else
                        (cond ((eof-object? c)
                               (eof-warning p))
                              (else
                               (reader-warning p "Invalid character following \\")))
                        (lp chars)))))
              (else
               (reader-warning p "Invalid character in identifier" c)
               (get-char p)
               (lp chars))))))

  ;; Get a number from the reader.
  (define (get-number p initial-chars)
    (let lp ((chars initial-chars))
      (let ((c (lookahead-char p)))
        (cond ((and (not (eqv? c #\#)) (char-delimiter? c))
               ;; TODO: some standard numbers are not supported
               ;; everywhere, should use a number lexer.
               (let ((str (list->string (reverse chars))))
                 (cond ((string->number str))
                       (else
                        (reader-warning p "Invalid number syntax" str)
                        0))))
              (else
               (lp (cons (get-char p) chars)))))))

  ;; Get a string datum from the reader.
  (define (get-string p)
    (let lp ((chars '()))
      (let ((c (lookahead-char p)))
        (cond ((eof-object? c)
               (eof-warning p)
               c)
              ((char=? c #\")
               (get-char p)
               (list->string (reverse chars)))
              ((char=? c #\\)           ;escapes
               (get-char p)             ;consume #\\
               (let ((c (lookahead-char p)))
                 (cond ((eof-object? c)
                        (eof-warning p)
                        c)
                       ((or (memv c '(#\tab #\linefeed #\x0085 #\x2028))
                            (eq? (char-general-category c) 'Zs))
                        ;; \<intraline whitespace>*<line ending>
                        ;; <intraline whitespace>*
                        (letrec ((skip-intraline-whitespace*
                                  (lambda ()
                                    (let ((c (lookahead-char p)))
                                      (cond ((eof-object? c)
                                             (eof-warning p)
                                             c)
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
                                             (reader-warning p "Expected a line ending" c)))))))
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
                                (reader-warning p "Invalid escape in string" c)
                                #\xFFFD))
                             chars))))))
              (else
               (lp (cons (get-char p) chars)))))))

  ;; Gets a nested comment from the reader.
  (define (get-nested-comment reader)
    ;; The reader is immediately after "#|".
    (rnrs:call-with-string-output-port
     (lambda (out)
       (let lp ((levels 1) (c0 (get-char reader)))
         (let ((c1 (get-char reader)))
           (cond ((eof-object? c0)
                  (eof-warning reader))
                 ((and (eqv? c0 #\|) (eqv? c1 #\#))
                  (unless (eqv? levels 1)
                    (rnrs:put-char out c0)
                    (rnrs:put-char out c1)
                    (lp (- levels 1) (get-char reader))))
                 ((and (eqv? c0 #\#) (eqv? c1 #\|))
                  (rnrs:put-char out c0)
                  (rnrs:put-char out c1)
                  (lp (+ levels 1) (get-char reader)))
                 (else
                  (rnrs:put-char out c0)
                  (lp levels c1))))))))

  ;; Get a comment from the reader (including the terminating whitespace).
  (define (get-comment reader)
    ;; The reader is immediately after #\;.
    (rnrs:call-with-string-output-port
     (lambda (out)
       (let lp ()
         (let ((c (get-char reader)))
           (unless (eof-object? c)
             (rnrs:put-char out c)
             (cond ((memv c '(#\linefeed #\x0085 #\x2028 #\x2029)))
                   ((char=? c #\return)
                    ;; Weird line ending. This lookahead is what forces
                    ;; the procedure to include the terminator.
                    (when (memv (lookahead-char reader) '(#\linefeed #\x0085))
                      (rnrs:put-char out (get-char reader))))
                   (else
                    (lp)))))))))

  ;; Whitespace and comments can appear anywhere.
  (define (atmosphere? x)
    (and (pair? x)
         (memq (car x) '(directive whitespace comment inline-comment nested-comment))))

  ;; Get the next lexeme from the reader, ignoring anything that is
  ;; like a comment.
  (define (get-lexeme p)
    (let ((lexeme (get-token p)))
      (if (atmosphere? lexeme)
          (get-lexeme p)
          lexeme)))

  ;; Get the next token. Can be a lexeme, directive, whitespace or comment.
  (define (get-token p)
    (assert (reader? p))
    (reader-mark p)
    (let ((c (get-char p)))
      (cond
        ((eof-object? c) c)
        ((char-whitespace? c)
         `(whitespace . ,(get-whitespace p c)))
        ((char=? c #\;)                 ;a comment like this one
         `(comment . ,(get-comment p)))
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
              (let* ((c1 (and (eqv? (lookahead-char p) #\u) (get-char p)))
                     (c2 (and (eqv? c1 #\u) (eqv? (lookahead-char p) #\8) (get-char p)))
                     (c3 (and (eqv? c2 #\8) (eqv? (lookahead-char p) #\() (get-char p))))
                (cond ((and (eqv? c1 #\u) (eqv? c2 #\8) (eqv? c3 #\())
                       'bytevector)
                      (else
                       (reader-warning p "Expected #vu8(")
                       (get-token p)))))
             ((#\;)                     ;s-expr comment
              ;; XXX: This includes the atmosphere between the "#;" and the datum.
              (let lp ((atmosphere '()))
                (let ((token (get-token p)))
                  (cond ((eof-object? token)
                         (eof-warning p)
                         `(inline-comment ,(reverse atmosphere) . ,p))
                        ((atmosphere? token)
                         (lp (cons token atmosphere)))
                        (else
                         (let-values (((d _) (handle-lexeme p token)))
                           `(inline-comment ,(reverse atmosphere) . ,d)))))))
             ((#\|)                     ;nested comment
              `(nested-comment . ,(get-nested-comment p)))
             ((#\!)                     ;#!r6rs etc
              (if (memv (lookahead-char p) '(#\/ #\space))
                  (let ((line (reader-saved-line p))
                        (column (reader-saved-column p)))
                    `(shebang ,line ,column . ,(get-line p)))
                  (let ((id (get-token p)))
                    (cond ((and (pair? id) (eq? (car id) 'identifier))
                           (case (cdr id)
                             ((r6rs)    ;r6rs.pdf
                              (reader-mode-set! p 'r6rs))
                             ((fold-case) ;r6rs-app.pdf
                              (reader-fold-case-set! p #t))
                             ((no-fold-case) ;r6rs-app.pdf
                              (reader-fold-case-set! p #f))
                             (else
                              (reader-warning p "Invalid directive" (cdr id))))
                           (cons 'directive (cdr id)))
                          (else
                           (reader-warning p "Expected an identifier after #!")
                           (get-token p))))))
             ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
              (get-number p (list c #\#)))
             ((#\t #\T)
              (unless (char-delimiter? (lookahead-char p))
                (reader-warning p "A delimiter is expected after #t"))
              #t)
             ((#\f #\F)
              (unless (char-delimiter? (lookahead-char p))
                (reader-warning p "A delimiter is expected after #f"))
              #f)
             ((#\\)
              (let lp ((char* '()))
                (let ((c (lookahead-char p)))
                  (cond ((and (pair? char*) (char-delimiter? c))
                         (let ((char* (reverse char*)))
                           (cond ((null? char*)
                                  (reader-warning p "Empty character name")
                                  #\xFFFD)
                                 ((null? (cdr char*)) (car char*))
                                 ((char=? (car char*) #\x)
                                  (cond ((for-all (lambda (c)
                                                    (or (char<=? #\0 c #\9)
                                                        (char-ci<=? #\a c #\f)))
                                                  (cdr char*))
                                         (let ((sv (string->number (list->string (cdr char*)) 16)))
                                           (cond ((unicode-scalar-value? sv)
                                                  (integer->char sv))
                                                 (else
                                                  (reader-warning p "Hex-escaped character outside valid range" sv)
                                                  #\xFFFD))))
                                        (else
                                         (reader-warning p "Invalid character in hex-escaped character"
                                                         (list->string (cdr char*)))
                                         #\xFFFD)))
                                 (else
                                  (let ((char-name (list->string char*))
                                        (char-names '(("nul" . #\nul)
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
                                                      ("delete" . #\delete))))
                                    (cond
                                      ((assoc char-name char-names) => cdr)
                                      ((and (reader-fold-case p)
                                            (assoc (string-foldcase char-name) char-names)) => cdr)
                                      (else
                                       (reader-warning p "Invalid character name" char-name)
                                       #\xFFFD)))))))
                        ((and (null? char*) (eof-object? c))
                         (reader-warning p "Unexpected EOF")
                         #\xFFFD)
                        (else
                         (lp (cons (get-char p) char*)))))))
             (else
              (reader-warning p "Invalid #-syntax" c)
              (get-token p)))))
        ((char=? c #\")
         (get-string p))
        ((memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (get-number p (list c)))
        ((memv c '(#\- #\+))            ;peculiar identifier
         (cond ((and (char=? c #\-) (eqv? #\> (lookahead-char p))) ;->
                (get-identifier p (list c)))
               ((char-delimiter? (lookahead-char p))
                (cons 'identifier (string->symbol (string c))))
               (else (get-number p (list c)))))
        ((char=? c #\.)                 ;peculiar identifier
         (cond ((eqv? #\. (lookahead-char p))
                (get-char p)            ;consume second dot
                (unless (eqv? #\. (get-char p)) ;consume third dot
                  (reader-warning p "Expected the ... identifier"))
                (unless (char-delimiter? (lookahead-char p))
                  (reader-warning p "Expected the ... identifier"))
                (cons 'identifier '...))
               ((char-delimiter? (lookahead-char p))
                'dot)
               (else
                (get-number p (list c)))))
        ((or (char-ci<=? #\a c #\Z) ;<constituent> and <special initial>
             (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
             (and (> (char->integer c) 127)
                  (memq (char-general-category c)
                        '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))
         (get-identifier p (list c)))
        ((char=? c #\\)                 ;<inline hex escape>
         (let ((c (get-char p)))
           (cond ((eqv? c #\x)
                  (get-identifier p (list (get-inline-hex-escape p))))
                 (else
                  (cond ((eof-object? c)
                         (eof-warning p))
                        (else
                         (reader-warning p "Invalid character following \\")))
                  (get-token p)))))
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
            (reader-warning p "Invalid character" c)
            (get-token p)))))))

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

  (define (get-compound-datum p src terminator type)
    (let lp ((data '()) (data^ '()))
      (let ((x (get-lexeme p)))
        (cond ((or (eof-object? x) (eq? x terminator) (memq x '(closep closeb)))
               (unless (eq? x terminator)
                 (cond ((eof-object? x)
                        (eof-warning p))
                       (else
                        (reader-warning p "Mismatched parenthesis/brackets" x terminator))))
               (case type
                 ((vector)
                  (let ((s (list->vector (reverse data))))
                    (values s (annotate src s (list->vector (reverse data^))))))
                 ((list)
                  (let ((s (reverse data)))
                    (values s (annotate src s (reverse data^)))))
                 ((bytevector)
                  (let ((s (u8-list->bytevector (reverse data))))
                    (values s (annotate src s s))))
                 (else
                  (reader-error p "Internal error in get-compound-datum" type))))
              ((eq? x 'dot)             ;a dot like in (1 . 2)
               (cond ((eq? type 'list)
                      (let-values (((x x^) (handle-lexeme p (get-lexeme p))))
                        (let ((t (get-lexeme p)))
                          (cond ((eq? t terminator))
                                ((eof-object? t)
                                 (eof-warning p)
                                 t)
                                (else
                                 (reader-warning p "Improperly terminated dot list"))))
                        (let ((s (append (reverse data) x)))
                          (values s (annotate src s (append (reverse data^) x^))))))
                     (else
                      (reader-warning p "Dot used in non-list datum")
                      (lp data data^))))
              (else
               (let-values (((d d^) (handle-lexeme p x)))
                 (case type
                   ((bytevector)
                    (cond ((and (fixnum? d) (fx<=? 0 d 255))
                           (lp (cons d data) data^))
                          (else
                           (reader-warning p "Invalid datum in bytevector" d)
                           (lp data data^))))
                   (else
                    (lp (cons d data) (cons d^ data^))))))))))

  (define (handle-lexeme p x)
    (let ((src (reader-source p)))
      (case x
        ((openp)
         (get-compound-datum p src 'closep 'list))
        ((openb)
         (get-compound-datum p src 'closeb 'list))
        ((vector)
         (get-compound-datum p src 'closep 'vector))
        ((bytevector)
         (get-compound-datum p src 'closep 'bytevector))
        (else
         (cond ((or (char? x) (string? x) (boolean? x)
                    (number? x) (bytevector? x))
                (values x (annotate src x x)))
               ((eof-object? x)
                (values x (annotate src x x)))
               ((and (pair? x) (eq? (car x) 'identifier))
                (values (cdr x) (annotate src (cdr x) (cdr x))))
               ((and (pair? x) (eq? (car x) 'abbrev))
                (let ((lex (get-lexeme p)))
                  (cond ((eof-object? lex)
                         (eof-warning p)
                         (values lex lex))
                        (else
                         (let-values (((d d*) (handle-lexeme p lex)))
                           (let ((s (list (cdr x) d)))
                             (values s (annotate src s (list (cdr x) d*)))))))))
               ((and (pair? x) (eq? (car x) 'shebang) (eqv? (cadr x) 1) (eqv? (caddr x) 0))
                ;; Ignore the shebang ("#!/" or "#! " at the start of files).
                ;; FIXME: should only work for programs.
                (handle-lexeme p (get-lexeme p)))
               (else
                (reader-warning p "Unexpected lexeme" x)
                (handle-lexeme p (get-lexeme p)))))))))

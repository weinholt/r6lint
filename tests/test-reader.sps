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

(import (r6lint lib reader)
        (r6lint tests check)
        (rnrs (6)))

;; Lexing
(letrec ((get-all (lambda (input)
                    (guard (con
                            ((lexical-violation? con)
                             '&lexical))
                      (let ((reader (make-reader (open-string-input-port input) "<test>")))
                        (let lp ((token* '()))
                          (let ((token (get-token reader)))
                            (if (eof-object? token)
                                (reverse token*)
                                (lp (cons token token*))))))))))
  (check (get-all "") => '())
  (check (get-all "#!/usr/bin/env scheme-script\n#f") => '((shebang 1 0 . "/usr/bin/env scheme-script") #f))
  (check (get-all " #!/usr/bin/env scheme-script\n#f") => '((whitespace . " ") (shebang 1 1 . "/usr/bin/env scheme-script") #f))
  (check (get-all " #f ") => '((whitespace . " ") #f (whitespace . " ")))
  (check (get-all "#!r6rs #f") => '((directive . r6rs) (whitespace . " ") #f))

  ;; Comment to line ending or paragraph separator.
  (check (get-all ";comment\x000A;(") => '((comment . "comment\x000A;") openp))
  (check (get-all ";comment\x000D;(") => '((comment . "comment\x000D;") openp))
  (check (get-all ";comment\x000D;\x000A;(") => '((comment . "comment\x000D;\x000A;") openp))
  (check (get-all ";comment\x0085;(") => '((comment . "comment\x0085;") openp))
  (check (get-all ";comment\x000D;\x0085;(") => '((comment . "comment\x000D;\x0085;") openp))
  (check (get-all ";comment\x2028;(") => '((comment . "comment\x2028;") openp))
  (check (get-all ";comment\x2029;(") => '((comment . "comment\x2029;") openp))

  ;; Inline comments.
  (check (get-all "#;1") => '((inline-comment () . 1)))
  (check (get-all "#; 1") => '((inline-comment ((whitespace . " ")) . 1)))
  (check (get-all "#;;\n1") => '((inline-comment ((comment . "\n")) . 1)))
  (check (get-all "#;#||#1") => '((inline-comment ((nested-comment . "")) . 1)))
  (check (get-all "#;#;1 2") => '((inline-comment ((inline-comment () . 1) (whitespace . " ")) . 2)))

  ;; Nested comments.
  (check (get-all "#| |#") => '((nested-comment . " ")))
  (check (get-all "#| x |# y") => '((nested-comment . " x ") (whitespace . " ") (identifier . y)))
  (check (get-all "#| x #| y |# z |# w") => '((nested-comment . " x #| y |# z ") (whitespace . " ") (identifier . w)))
  (check (get-all "#|##||#|#") => '((nested-comment . "##||#")))
  (check (get-all "#|#|#||#|#|#") => '((nested-comment . "#|#||#|#")))
  (check (get-all "#|#|#| ## |#|#|#") => '((nested-comment . "#|#| ## |#|#")))

  ;; Examples from R6RS.
  (check (get-all "#\\a") => '(#\a))
  (check (get-all "#\\A") => '(#\A))
  (check (get-all "#\\(") => '(#\())
  (check (get-all "#\\ ") => '(#\x0020))
  (check (get-all "#\\nul") => '(#\x0000))
  (check (get-all "#\\alarm") => '(#\x0007))
  (check (get-all "#\\backspace") => '(#\x0008))
  (check (get-all "#\\tab") => '(#\x0009))
  (check (get-all "#\\linefeed") => '(#\x000A))
  (check (get-all "#\\newline") => '(#\x000A))
  (check (get-all "#\\vtab") => '(#\x000B))
  (check (get-all "#\\page") => '(#\x000C))
  (check (get-all "#\\return") => '(#\x000D))
  (check (get-all "#\\esc") => '(#\x001B))
  (check (get-all "#\\space") => '(#\x0020))
  (check (get-all "#\\delete") => '(#\x007F))
  (check (get-all "#\\xFF") => '(#\x00FF))
  (check (get-all "#\\x03BB") => '(#\x03BB))
  (check (get-all "#\\x00006587") => '(#\x6587))
  (check (get-all "#\\λ") => '(#\x03BB))
  (check (get-all "#\\x0001z") => '&lexical)
  (check (get-all "#\\λx") => '&lexical)
  (check (get-all "#\\alarmx") => '&lexical)
  (check (get-all "#\\alarm x") => '(#\x0007 (whitespace . " ") (identifier . x)))
  (check (get-all "#\\Alarm") => '&lexical)
  (check (get-all "#\\alert") => '&lexical)
  (check (get-all "#\\xA") => '(#\x000A))
  (check (get-all "#\\xFF") => '(#\x00FF))
  (check (get-all "#\\xff") => '(#\x00FF))
  (check (get-all "#\\x ff") => '(#\x0078 (whitespace . " ") (identifier . ff)))
  (check (get-all "#\\x(ff)") => '(#\x0078 openp (identifier . ff) closep))
  (check (get-all "#\\(x)") => '&lexical)
  (check (get-all "#\\(x") => '&lexical)
  (check (get-all "#\\((x)") => '(#\x0028 openp (identifier . x) closep))
  (check (get-all "#\\x00110000") => '&lexical)
  (check (get-all "#\\x000000001") => '(#\x0001))
  (check (get-all "#\\xD800") => '&lexical)

  ;; String examples from R6RS.
  (check (get-all "\"abc\"") => '("\x0061;\x0062;\x0063;"))
  (check (get-all "\"\\x41;bc\"") => '("\x0041;\x0062;\x0063;"))
  (check (get-all "\"\\x41; bc\"") => '("\x0041;\x0020;\x0062;\x0063;"))
  (check (get-all "\"\\x41bc;\"") => '("\x41BC;"))
  (check (get-all "\"\\x41\"") => '&lexical)
  (check (get-all "\"\\x;\"") => '&lexical)
  (check (get-all "\"\\x41bx;\"") => '&lexical)
  (check (get-all "\"\\x00000041;\"") => '("\x0041;"))
  (check (get-all "\"\\x0010FFFF;\"") => '("\x10FFFF;"))
  (check (get-all "\"\\x00110000;\"") => '&lexical)
  (check (get-all "\"\\x000000001;\"") => '("\x0001;"))
  (check (get-all "\"\\xD800;\"") => '&lexical)
  (check (get-all "\"A\nbc\"") => '("\x0041;\x000A;\x0062;\x0063;")))

;; Detect file type
(letrec ((detect (lambda (input)
                   (call-with-port (open-string-input-port input)
                     detect-scheme-file-type))))
  (check (detect "") => 'empty)
  (check (detect "#!r6rs") => 'empty)
  (check (detect "#!/usr/bin/env scheme-script\n#f") => 'r6rs-program)
  (check (detect "#! /usr/bin/env scheme-script\n#f") => 'r6rs-program)
  (check (detect "(import (rnrs))") => 'r6rs-program)
  (check (detect "#!r6rs (import ") => 'r6rs-program)
  (check (detect "#!r6rs (library ") => 'r6rs-library)
  ;; Looks weird but it's allowed.
  (check (detect "#!r6rs [library ") => 'r6rs-library)
  (check (detect "[#!r6rs library ") => 'r6rs-library)
  (check (detect "#!r6rs [import ") => 'r6rs-program)
  (check (detect "[#!r6rs import ") => 'r6rs-program))

;; Reading
(letrec ((stripped-read
          (lambda (input)
            (let ((reader (make-reader (open-string-input-port input) "<test>")))
              (guard (_
                      (else 'error))
                (annotation-stripped (read-annotated reader)))))))
  (check (stripped-read "#!/usr/bin/env scheme-script\n#f") => '#f)
  (check (stripped-read " #!/usr/bin/env scheme-script\n#f") => 'error)
  (check (stripped-read "#f") => #f)
  (check (stripped-read "()") => '())
  (check (stripped-read "#!r6rs ()") => '())
  (check (stripped-read "(#!r6rs)") => '())
  (check (stripped-read "#!\tr6rs ()") => 'error)
  (check (stripped-read "#\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case #\\NEWLINE") => #\newline)
  (check (stripped-read "#!no-fold-case #\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case X") => 'x)
  ;; Disabled for GNU Guile 2.2.0.
  ;; (check (stripped-read "#!fold-case STRAßE") => 'strasse)
  (check (stripped-read "\"\\xf6;\"") => "\xf6;")

  ;; There is no obfuscated Scheme contest.
  (check (stripped-read "(let((* .(`'((unquote .(+ .[])).()))).())((caadr .(* .())).(#o-7 .[#E#x93/3])).[])") =>
         '(let ([* `'(,+)]) ((caadr *) -7 49)))

  ;; Comment to line ending or paragraph separator.
  (check (stripped-read "; comment\x000A;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;\x000A;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x0085;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;\x0085;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x2028;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x2029;(display 'hello)") => '(display 'hello))

  ;; Inline comments
  (check (stripped-read "#; ;foo\n(x) (y)") => '(y))
  (check (stripped-read "#; #| |# (x) (y)") => '(y))
  (check (stripped-read "#; #; (x) (y) (z)") => '(z))

  ;; Nested comments
  (check (stripped-read "#| x |# y") => 'y)
  (check (stripped-read "#| x #| y |# z |# w") => 'w)
  (check (stripped-read "#|##||#|# x") => 'x)
  (check (stripped-read "#|#|#||#|#|# x") => 'x)
  (check (stripped-read "#|#|#| ## |#|#|# x") => 'x))

(check-report)
(exit (if (check-passed? 112) 0 1))

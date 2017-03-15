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
                    (let ((reader (make-reader (open-string-input-port input) "<test>")))
                      (let lp ((lexeme* '()))
                        (let ((lexeme (get-lexeme reader)))
                          (if (eof-object? lexeme)
                              (reverse lexeme*)
                              (lp (cons lexeme lexeme*)))))))))
  (check (get-all "#!/usr/bin/env scheme-script\n#f") => '((shebang 1 0 . "/usr/bin/env scheme-script") #f))
  (check (get-all " #!/usr/bin/env scheme-script\n#f") => '((whitespace . " ") (shebang 1 1 . "/usr/bin/env scheme-script") #f))
  (check (get-all " #f ") => '((whitespace . " ") #f (whitespace . " ")))
  (check (get-all "#!r6rs #f") => '((directive . r6rs) (whitespace . " ") #f)))

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
              (guard (exn
                      (else 'error))
                (annotation-stripped (read-annotated reader)))))))
  (check (stripped-read "#!/usr/bin/env scheme-script\n#f") => '#f)
  (check (stripped-read " #!/usr/bin/env scheme-script\n#f") => 'error)
  (check (stripped-read "#f") => #f)
  (check (stripped-read "()") => '())
  (check (stripped-read "#!r6rs ()") => '())
  (check (stripped-read "(#!r6rs)") => '())
  (check (stripped-read "#\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case #\\NEWLINE") => #\newline)
  (check (stripped-read "#!no-fold-case #\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case X") => 'x)
  (check (stripped-read "#!fold-case STRAßE") => 'strasse)
  (check (stripped-read "\"\\xf6;\"") => "\xf6;"))

(check-report)
(exit (if (check-passed? 27) 0 1))

;; TODO: nested comments   #|##||#|#
;; TODO: (check (stripped-read "#!\tr6rs ()") => ?)

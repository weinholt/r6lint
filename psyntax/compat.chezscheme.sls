;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (r6lint psyntax compat)
  (export make-parameter parameterize define-record pretty-print
          gensym void eval-core symbol-value set-symbol-value!
          file-options-spec read-library-source-file
          annotation? annotation-expression annotation-stripped
          read-annotated annotation-source annotation-source->condition
          library-version-mismatch-warning
          file-locator-resolution-error)
  (import
    (only (r6lint lib reader)
          make-reader
          read-annotated annotation? annotation-expression
          annotation-stripped annotation-source
          annotation-source->condition)
    (rnrs)
    (prefix (rnrs eval) rnrs:)
    ;; (only (chezscheme) record-writer type-descriptor)
    (rename (only (chezscheme) define-record
                  gensym getprop putprop
                  make-parameter parameterize
                  pretty-print void)
            (define-record define-record*)
            (gensym gensym*)))

  ;; This is the environment which will be available to eval-core,
  ;; which is used to run code during expansion.
  (define env (rnrs:environment
               '(except (rnrs)
                        identifier? generate-temporaries free-identifier=?
                        bound-identifier=? datum->syntax syntax-violation
                        syntax->datum make-variable-transformer
                        ;; Not safe -- if you need to access files
                        ;; from your macros, please figure something
                        ;; out. Maybe make filtering versions of these
                        ;; procedures.
                        open-output-file with-output-to-file
                        call-with-output-file open-file-input/output-port
                        open-file-output-port file-exists?)
               '(only (r6lint psyntax expander)
                      identifier? generate-temporaries free-identifier=?
                      bound-identifier=? datum->syntax syntax-violation
                      syntax->datum make-variable-transformer
                      syntax-dispatch syntax-error ellipsis-map)
               '(rnrs r5rs)
               ;; Not safe and it's not very common to call eval from macros.
               ;; '(rnrs eval)
               '(rnrs mutable-pairs)
               '(rnrs mutable-strings)
               '(only (r6lint psyntax compat) gensym)))

  (define (eval-core expr)
    (rnrs:eval expr env))

  (define gensym
    (case-lambda
      (()
       (gensym*))
      ((pretty-name)
       (if (symbol? pretty-name)
           (gensym* (symbol->string pretty-name))
           (gensym* pretty-name)))))

  (define (symbol-value s)
    (getprop s 'value))

  (define (set-symbol-value! s v)
    (putprop s 'value v))

  (define (read-library-source-file file-name)
    (call-with-port (open-input-file file-name)
      (lambda (p)
        (let ((reader (make-reader p file-name)))
          (read-annotated reader)))))

  (define (library-version-mismatch-warning name depname filename)
    ;;; please override this in your production implementation
    (display "Warning: inconsistent dependencies: ")
    (display name)
    (display depname)
    (display filename))


  (define (file-locator-resolution-error libname failed-list)
    ;;; please override this in your production implementation
    (error 'file-location "cannot find library" libname))

  (define-syntax define-record
    (syntax-rules ()
      [(_ name (field* ...) printer)
       (begin
         (define-record* name (field* ...))
         #;
         (record-writer (type-descriptor name)
                        (lambda (r p wr)
                          (printer r p))))]
      [(_ name (field* ...))
       (define-record* name (field* ...))]))

  (define (file-options-spec x)
    (error 'file-options-spec "not implemented")))

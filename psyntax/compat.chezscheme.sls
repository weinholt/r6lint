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
          read-annotated annotation-source
          library-version-mismatch-warning
          file-locator-resolution-error)
  (import
    (only (r6lint lib reader)
          read-annotated annotation? annotation-expression
          annotation-stripped annotation-source)
    (rnrs)
    ;; (for (only (scheme) record-writer type-descriptor) expand)
    (rename (only (scheme)
                  define-record
                  eval
                  gensym
                  top-level-value set-top-level-value!
                  make-parameter parameterize
                  pretty-print
                  void)
            (define-record define-record*)
            (gensym gensym*)
            (eval eval-core)
            (top-level-value symbol-value)
            (set-top-level-value! set-symbol-value!)))

  (define gensym
    (case-lambda
      (()
       (gensym*))
      ((pretty-name)
       (if (symbol? pretty-name)
           (gensym* (symbol->string pretty-name))
           (gensym* pretty-name)))))

  (define (read-library-source-file file-name)
    (with-input-from-file file-name
      (lambda ()
        (read-annotated (current-input-port)))))

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

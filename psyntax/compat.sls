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
  (export make-parameter parameterize define-record
          gensym eval-core symbol-value set-symbol-value!
          make-file-options read-library-source-file
          annotation? annotation-expression annotation-stripped
          make-reader read-annotated annotation-source annotation-source->condition
          library-version-mismatch-warning
          library-stale-warning
          file-locator-resolution-error
          label-binding set-label-binding! remove-location)
  (import
    (only (r6lint psyntax gensym) gensym)
    (only (r6lint lib reader)
          make-reader
          read-annotated annotation? annotation-expression
          annotation-stripped annotation-source
          annotation-source->condition)
    (rnrs)
    (prefix (rnrs eval) rnrs:))

  (define (map-in-order f x*)
    (if (null? x*)
        '()
        (cons (f (car x*))
              (map-in-order f (cdr x*)))))

  ;; XXX: Should probably possibly be parameters
  (define *GLOBALS* (make-eq-hashtable))
  (define *LABELS* (make-eq-hashtable))

  (define (label-binding x)
    (hashtable-ref *LABELS* x #f))

  (define (set-label-binding! x v)
    (hashtable-set! *LABELS* x v))

  (define (remove-location x)
    (hashtable-delete! *LABELS* x))

  (define (set-symbol-value! x v)
    (hashtable-set! *GLOBALS* x v))

  (define (symbol-value x)
    (unless (hashtable-contains? *GLOBALS* x)
      (error 'symbol-value "Undefined symbol" x))
    (hashtable-ref *GLOBALS* x #f))

  ;; This is used to run code during expansion.
  (define eval-core
    (let ((env #f))
      (lambda (expr)
        (unless env
          (set! env (rnrs:environment
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
                              open-file-output-port delete-file)
                     '(rnrs r5rs)
                     ;; Not safe and it's not very common to call eval from macros.
                     ;; '(rnrs eval)
                     '(rnrs mutable-pairs)
                     '(rnrs mutable-strings)
                     ;; We're importing ourselves here, which is why
                     ;; the call to environment is delayed. Without
                     ;; this GNU Guile will try to use a half-finished
                     ;; version of this library.
                     '(only (r6lint psyntax expander)
                            identifier? generate-temporaries free-identifier=?
                            bound-identifier=? datum->syntax syntax-violation
                            syntax->datum make-variable-transformer
                            syntax-dispatch syntax-error ellipsis-map)
                     '(only (r6lint psyntax compat)
                            make-file-options gensym
                            symbol-value set-symbol-value!))))
        (rnrs:eval expr env))))

  (define (read-library-source-file file-name)
    (call-with-port (open-input-file file-name)
      (lambda (p)
        (let ((reader (make-reader p file-name)))
          (read-annotated reader)))))

  (define make-parameter
    (case-lambda
      ((x) (make-parameter x (lambda (x) x)))
      ((x fender)
       (assert (procedure? fender))
       (let ((x (fender x)))
         (case-lambda
           (() x)
           ((v) (set! x (fender v))))))))

  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        ((_ () b b* ...) (syntax (let () b b* ...)))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (with-syntax (((lhs* ...) (generate-temporaries (syntax (olhs* ...))))
                       ((rhs* ...) (generate-temporaries (syntax (olhs* ...)))))
           (syntax (let ((lhs* olhs*) ...
                         (rhs* orhs*) ...)
                     (let ((swap
                            (lambda ()
                              (let ((t (lhs*)))
                                (lhs* rhs*)
                                (set! rhs* t))
                              ...)))
                       (dynamic-wind
                         swap
                         (lambda () b b* ...)
                         swap)))))))))


  (define (library-version-mismatch-warning name depname filename)
    ;;; please override this in your production implementation
    (define p (current-error-port))
    (display "Warning: inconsistent dependencies: " p)
    (display name p)
    (display depname p)
    (display filename p))

  (define (library-stale-warning name filename)
    (define p (current-error-port))
    (display "WARNING: library " p)
    (display name p)
    (display " is stale; file " p)
    (display filename p)
    (display "will be recompiled from source.\n" p))

  (define (file-locator-resolution-error libname failed-list pending-list)
    ;;; please override this in your production implementation
    (error 'file-location "cannot find library" libname failed-list pending-list))

  ;;; we represent records as vectors for portability but this is
  ;;; not nice.  If your system supports compile-time generative
  ;;; records, replace the definition of define-record with your
  ;;; system supplied definition (which you should support in the
  ;;; expander first of course).
  ;;; if your system allows associating printers with records,
  ;;; a printer procedure is provided (so you can use it in the
  ;;; output of the macro).  The printers provided take two
  ;;; arguments, a record instance and an output port.  They
  ;;; output something like #<stx (foo bar)> or #<library (rnrs)>
  ;;; to the port.
  ;;;
  ;;; The following should be good for full R6RS implementations.
  ;;;
  ;;;   (define-syntax define-record
  ;;;     (syntax-rules ()
  ;;;       ((_ name (field* ...) printer)
  ;;;        (define-record name (field* ...)))
  ;;;       ((_ name (field* ...))
  ;;;        (define-record-type name
  ;;;           (sealed #t)     ; for better performance
  ;;;           (opaque #t)     ; for security
  ;;;           (nongenerative) ; for sanity
  ;;;           (fields field* ...)))))

  ;; borrowed from mosh
  (define-syntax define-record
    (lambda (x)
      (define (syn->str s)
        (symbol->string
         (syntax->datum s)))
      (define (gen-getter id)
        (lambda (fld)
          (datum->syntax id
                         (string->symbol
                          (string-append (syn->str id) "-" (syn->str fld))))))
      (define (gen-setter id)
        (lambda (fld)
          (datum->syntax id
                         (string->symbol
                          (string-append "set-" (syn->str id) "-" (syn->str fld) "!")))))
      (syntax-case x ()
        [(_ name (field* ...) printer)
         #`(begin
             (define-record name (field* ...))
             #;
             (define rp (make-record-printer 'name printer)))]
        [(_ name (field* ...))
         (with-syntax ([(getter* ...)
                        (map (gen-getter #'name) #'(field* ...))]
                       [(setter* ...)
                        (map (gen-setter #'name) #'(field* ...))])
           #`(define-record-type name
               (sealed #t) ; for better performance
               (opaque #f)
               (nongenerative) ; for sanity
               (fields (mutable field* getter* setter*) ...)))])))

  (define file-options-set (make-enumeration '(no-create no-fail no-truncate)))
  (define make-file-options (enum-set-constructor file-options-set)))

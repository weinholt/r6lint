;;; flycheck-r6lint.el --- Flycheck extensio for r6lint.  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Göran Weinholt <goran@weinholt.se>
;;
;; Author: Göran Weinholt <goran@weinholt.se>
;; Keywords: languages r6rs scheme
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (flycheck "0.25"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Flycheck extension for the R6RS linter r6lint.

;;; Code:

(require 'flycheck)

(flycheck-def-executable-var r6lint "r6lint")

(flycheck-def-config-file-var flycheck-r6lintrc r6lint
                              ".r6lintrc"
  :safe #'stringp)

(flycheck-define-checker r6lint
  "An R6RS Scheme syntax and style checker.
See `https://github.com/weinholt/r6lint/'."
  :command ("r6lint"
            ;; (config-file "--rcfile" flycheck-r6lintrc)
            source-inplace)
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (or "E" "F") ":"
          (id (one-or-more (not (any ":")))) ":"
          (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (or "W" "R") ":"
            (id (one-or-more (not (any ":")))) ":"
            (message) line-end)
   (info line-start (file-name) ":" line ":" column ":"
         "C:" (id (one-or-more (not (any ":")))) ":"
         (message) line-end))
  :modes scheme-mode)

;;;###autoload
(defun flycheck-r6lint-setup ()
  "Setup Flycheck for r6lint."
  (interactive)
  (add-to-list 'flycheck-checkers 'r6lint))

(provide 'flycheck-r6lint)

;;; flycheck-r6lint.el ends here

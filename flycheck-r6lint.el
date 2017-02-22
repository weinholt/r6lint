;;; flycheck-r6lint.el --- Flycheck extension for r6lint.  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Göran Weinholt <goran@weinholt.se>
;;
;; Author: Göran Weinholt <goran@weinholt.se>
;; Keywords: languages r6rs scheme
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (flycheck "0.25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck extension for the R6RS Scheme linter r6lint.

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
            (config-file "--config" flycheck-r6lintrc)
            "--" source-inplace)
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

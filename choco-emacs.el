;;; choco-emacs --- chocolatey

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/choco-emacs
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 30 September 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; choco/src/chocolatey.resources/helpers/functions

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))
(require 'etags)

(defvar choco-source-repo "https://github.com/chocolatey/choco")
(defvar choco-directory (getenv "chocolateyInstall"))

(defvar powershell--xref-identifier-completion-table
  (apply-partially #'completion-table-with-predicate
                   powershell-eldoc-obarray
                   (lambda (sym)
                     (intern-soft sym powershell-eldoc-obarray))
                   'strict))

;; ------------------------------------------------------------
;;* Load available helpers, get help docs

(defun choco-helpers ()
  (let ((scripts (directory-files
                  (expand-file-name "helpers/functions" choco-directory) t "^[^.]")))
    ()))

(defvar choco-emacs-objects (make-hash-table :test 'equal))


(provide 'choco-emacs)

;;; choco-emacs.el ends here

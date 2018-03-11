;;; choco-mode --- some helpers for choco

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/choco
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  3 November 2016

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
;;; Code:
(eval-when-compile
  (defvar company-etags-modes))
(require 'choco)
(require 'tag-utils)
(require 'etags)

;; determine file of tag, relative to `chocolatey-dir'
;; (defun choco-file-of-tag (&optional _ignored)
  ;; (save-excursion
  ;;   (re-search-backward "\f\n\\([^\n]+\\),[0-9]*\n")
  ;;   (let ((str (convert-standard-filename
  ;;               (buffer-substring (match-beginning 1) (match-end 1)))))
  ;;     (expand-file-name str (file-truename chocolatey-dir)))))

;;; Tags

;;;###autoload
(defun choco-tag-ido ()
  "Find tag with ido-completion.  Tag `chocolatey-dir' if no TAGS file exists."
  (interactive)
  (unless (file-exists-p (expand-file-name "TAGS" choco--dir))
    (choco-tag))
  (condition-case nil
      (xref-find-definitions
       (ido-completing-read "Tag: " (tags-completion-table)))
    (error (tags-reset-tags-tables))))

;; tag chocolatey helper functions
;; use absolute paths in TAGS file since can't write to install directory
(defun choco-tag ()
  (interactive)
  (tag-utils-tag-dir chocolatey-dir
                     :program "ctags"
                     :absolute-path t
                     :ofile (expand-file-name "TAGS" choco--dir)))

;;; Chocolatey objects

(defvar choco-objects (make-hash-table :test 'equal))

;; load helpers, scripts from chocolatey directory
;; (defun choco-load ()
;;   (unless (file-exists-p powershell-helper-script)
;;     (user-error "Unable to find powershell helper script: %s" powershell-helper-script))
;;   (let* ((proc (inf-powershell-shell-process t)))))

(when (featurep 'powershell)
  (defvar powershell--xref-identifier-completion-table
    (apply-partially #'completion-table-with-predicate
                     posh-functions
                     (lambda (sym)
                       (intern-soft sym posh-functions))
                     'strict)))

;;; Helpers

(defun choco-dired ()
  (interactive)
  (dired-other-window chocolatey-dir))

;;; Minor mode

(defvar choco-minor-mode-menu
  '("Choco"
    ["Dired" choco-dired t]
    ("Tags"
     ["Make Tags" choco-tag t]
     ["Find Tag" choco-tag-ido t])))

(defvar choco-minor-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil choco-minor-mode-menu)
    (define-key km (kbd "<f2> m f") #'choco-tag-ido)
    (define-key km (kbd "<f2> m d") #'choco-dired)
    (define-key km (kbd "<f2> m t") #'choco-tag)
    km))

;;;###autoload
(define-minor-mode choco-minor-mode
  "Chocolatey minor mode. Tags chocolatey directory when enabled and sets
company backends so autocompletion works for helper functions."
  nil
  :lighter "Choco"
  (when (featurep 'tag-utils)
    (unless (file-exists-p (expand-file-name "TAGS" choco--dir))
      (choco-tag)))
  (when (featurep 'company)
    (add-to-list 'company-etags-modes 'powershell-mode)
    (setq-local company-backends '((company-capf
                                    company-etags
                                    company-dabbrev-code)
                                   company-files
                                   company-dabbrev))))

(provide 'choco-mode)
;;; choco-mode.el ends here

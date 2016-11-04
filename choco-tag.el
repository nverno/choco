;;; choco-tag --- some helpers to tag choco

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

(provide 'choco-tag)
;;; choco-tag.el ends here

;;; choco --- chocolatey -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/choco
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

;; [![Build Status](https://travis-ci.org/nverno/choco.svg?branch=master)](https://travis-ci.org/nverno/choco)

;; choco/src/chocolatey.resources/helpers/functions

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib)
  (defvar company-backends))
(require 'powershell nil t)

(defvar choco-completing-read 'ido-completing-read)

;; stop waiting for process ouput to finish after this many seconds
(defvar choco-process-max-tries 60)

;; write messages with attempt number
(defvar choco-process-verbose nil)

(defvar choco-source-repo "https://github.com/chocolatey/choco")
(defvar chocolatey-dir
  (expand-file-name "helpers" (getenv "chocolateyInstall")))

;; ------------------------------------------------------------
;;; Internal variables

(defvar choco--dir nil)
(when load-file-name
  (setq choco--dir (file-name-directory load-file-name)))

(put 'choco-process-interrupted 'error-conditions
     '(choco-process-interrupted error))
(put 'choco-process-interrupted 'error-message
     "Choco proces interrupted, increase `choco-process-max-tries'.")

;; timer used to periodically poll choco process to
;; see if it is finished
(defvar choco-process-timer)

;; internal variable to track number of process pollings
(defvar choco-process--attempt)

(defalias 'choco-completing-read choco-completing-read)

;; ------------------------------------------------------------
;;; Inferior process
;; simple interface to powershell process

;; delete choco process
(defun choco-process-delete (&optional proc)
  (delete-process (or proc (get-buffer-process (current-buffer)))))

;; set status to busy until prompty is found
(defun choco-process-set-status (proc string)
  (let ((busy (not (string-match-p "\\(PS>\\)\\'" string))))
    (process-put proc 'busy busy)
    (process-put proc 'last-eval (current-time))))

;; setup process filter
(defun choco-process-output-filter (proc string)
  (choco-process-set-status proc string)
  (with-current-buffer (process-buffer proc)
    (insert string)))

;; accept process output until process isnt busy
(defun choco-process-accept-output (proc &optional wait max-wait)
  (let ((start-time (float-time))
        (wait (or wait 0.002)))
    (save-excursion
      (while (and (or (accept-process-output proc wait)
                      (process-get proc 'busy))
                  (if max-wait
                      (< (- (float-time) start-time) max-wait)
                    t))
        (if (> (- (float-time) start-time) 0.5)
            (setq wait 0.5))))))

;; start or return choco inferior process
(defun choco-process (&optional buffer)
  (let* ((buff (get-buffer-create (or buffer "*choco-process*")))
         (proc (get-buffer-process buff)))
    (if (process-live-p proc)
        proc
      (setq proc (start-process "powershell" buff "powershell"))
      (set-process-filter proc 'choco-process-output-filter)
      (with-current-buffer buff
        ;; setup prompt
        (process-send-string proc "function prompt { 'PS>' }\n")
        ;; kill process when buffer is killed
        (add-hook 'kill-buffer-hook 'choco-process-delete nil 'local))
      proc)))

;; check choco process to see if it is busy.
;; when it's done, cancel the timer and process the output
(defun choco-process-output (proc callback)
  (when choco-process-verbose
    (message "polling choco process: %s" choco-process--attempt))
  (when (or (> (cl-incf choco-process--attempt) choco-process-max-tries)
            (not (process-get proc 'busy)))
    (cancel-timer choco-process-timer)
    (when (> choco-process--attempt choco-process-max-tries)
      ;; interrupt process if taking longer than desired
      (interrupt-process proc)
      (signal 'choco-process-interrupted nil))
    (with-current-buffer (process-buffer proc)
      ;; remove final prompt
      (goto-char (point-max))
      (delete-region (point-at-bol) (point-max))
      ;; process output
      (if callback
          (funcall callback)
        (pop-to-buffer (current-buffer))))))

;; query choco process and read response
(defun choco-process-query (query &optional buffer callback)
  (let ((proc (choco-process buffer)))
    (with-current-buffer (process-buffer proc)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; set process status to busy and send query
      (process-put proc 'busy t)
      (process-send-string proc (concat query "\n"))
      ;; poll the process every second for `choco-process-max-tries'
      ;; then give up. Process is considered finished when it is no
      ;; longer 'busy
      (setq choco-process--attempt 0)
      (setq choco-process-timer
            (run-with-timer 0.5 1 'choco-process-output proc callback))
      ;; `choco-process-accept-output' is too slow to wait for synchronously
      ;; (choco-process-accept-output proc)
      )))

;; ------------------------------------------------------------
;;; Formatting output

;; construct choco query
(defmacro with-choco-query (query &rest body)
  (declare (indent defun) (debug (symbolp &rest form)))
  `(cl-letf ((fn (lambda () ,@body)))
     (choco-process-query ,query nil fn)))

;; chocolatey package
;; (cl-defstruct (choco-package (:constructor choco-package--create))
;;   name version)

;;; Tabulated List

(defvar choco-list-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "e") 'choco-list-execute)
    (define-key km (kbd "l") 'choco-list)
    (define-key km (kbd "c") 'choco)
    km))

;; Mode to display results in tabulated list
(define-derived-mode choco-list-mode tabulated-list-mode "Choco"
  "Display choco results as tabulated list.\n
Commands:\n
\\{choco-list-mode-map}"
  ;; bind these dynamically depending on output
  ;; - tabulated-list-format
  ;; - tabulated-list-entries
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook
            'choco-list-revert-buffer nil 'local)
  (linum-mode))

;; recall last choco command to revert the *choco* buffer
(defun choco-list-revert-buffer ()
  (when (bound-and-true-p choco-last-command)
    (message "Reverting: %s" choco-last-command)
    (funcall (get-text-property 0 'last-action choco-last-command)
             choco-last-command)))

;; Output query to tabulated list
;; FORMAT is `tabulated-list-format'
;; BODY should return entries for `tabulated-list-entries'
;; The body is processed in the choco-process buffer
(defmacro with-output-to-choco-list (query format &rest body)
  (declare (indent defun) (debug (symbolp symbolp &rest form)))
  `(with-choco-query ,query
     ;; after process finishes, point will be at point-max
     ;; in the choco process-buffer
     (goto-char (point-min))
     (let ((entries (progn ,@body)))
       (when entries
         (with-current-buffer (get-buffer-create "*choco*")
           ;; Clear *choco* output buffer, reset buffer variables
           ;; setup tabulated list mode and show buffer
           (kill-all-local-variables)
           (setq buffer-read-only nil)
           (erase-buffer)
           ;; store last query for `revert-buffer'
           (setq choco-last-command ,query)
           ;; reset buffer variables
           (setq tabulated-list-format ,format)
           (setq tabulated-list-entries entries)
           (choco-list-mode)
           (tabulated-list-print)
           (pop-to-buffer (current-buffer)))))))

;; default action to list choco packages as tabulated list
(defun choco-list--default-action (query)
  (with-output-to-choco-list query [("name" 30 t) ("version" 20 nil)]
    (let (choco-pkgs)
      (while (re-search-forward
              ;; collect lines like "package.name" "version"
              "^\\([^ \t\n]+\\)\\s-*\\([a-zA-Z.0-9]+\\)\\s-*$" nil t)
        (let ((name (match-string-no-properties 1))
              (version (match-string-no-properties 2)))
          (push (list name
                      (vector
                       (propertize name 'face 'font-lock-function-name-face)
                       (propertize version 'face 'font-lock-string-face)))
                choco-pkgs)))
      choco-pkgs)))

;; format default output for 'choco outdated' (lists outdated packages)
(defun choco-outdated--default-action (query)
  (with-output-to-choco-list query [("name" 25 t)
                                    ("current" 15 nil)
                                    ("available" 15 nil)
                                    ("pinned" 10 t)]
    
    (let (choco-pkgs)
      (while (re-search-forward
              "^\\([^\n| ]+\\)|\\(.+\\)|\\(.+\\)|\\(.+\\)$" nil t)
        (let ((name (match-string-no-properties 1))
              (current (match-string-no-properties 2))
              (available (match-string-no-properties 3))
              (pinned (match-string-no-properties 4)))
          (push (list name (vector
                            (propertize name 'face 'font-lock-function-name-face)
                            (propertize current 'face 'font-lock-warning-face)
                            (propertize available 'face 'font-lock-string-face)
                            (propertize pinned 'face 'font-lock-comment-face)))
                choco-pkgs)))
      (nreverse choco-pkgs))))

;; ------------------------------------------------------------
;;; Interactive

;; alist with formatting functions associated with choco output
(defvar choco-list-callbacks
  '(("list"         . ((action . choco-list--default-action)
                       (default-args "--local-only")))
    ("outdated"     . ((action . choco-outdated--default-action)
                       (default-args . "")))))

;;;###autoload
(defun choco-list (command &optional default-args action callback limit interactive)
  "Call 'choco' with optional CALLBACK function to process the output.
With a prefix argument, set time limit for async timer."
  (interactive
   (let* ((command (choco-completing-read
                    "Choco command: " (mapcar 'car choco-list-callbacks)))
          (alist (cdr (assoc command choco-list-callbacks)))
          (action (cdr (assoc 'action alist)))
          (args (read-from-minibuffer "Additional flags: "
                                      (or (cdr (assoc 'default-args alist))
                                          "--local-only")))
          (limit (if current-prefix-arg
                    (read-number "Time limit: " choco-process-max-tries)
                  choco-process-max-tries)))
     (list (concat "choco " command " " args) nil action nil limit t)))
  (when limit
    (setq choco-process-max-tries limit))
  (if callback
      (choco-process-query command nil callback)
    ;; capture last action for reverting buffer
    (put-text-property 0 1 'last-action action command)
    (funcall action command)))

;;;###autoload
(defun choco (arg)
  "Call choco and output raw results to buffer. With prefix, setup time limit
to wait to process response."
  (interactive "P")
  (let ((choco-process-max-tries (if arg
                                     (read-number "Seconds: ")
                                   choco-process-max-tries)))
    (choco-process-query (concat "choco " (read-from-minibuffer "Choco: ")))))

;;; Tabulated List Functions

;; interactively choose field from items in row at point
(defun choco-list-choose ()
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (and entry
         (choco-completing-read "Entry: " (append entry ())))))

(defun choco-list-execute (arg &optional cmd index)
  "Call choco on the entry at point. Prompts for command and runs it 
as external elevated powershell.  With prefix, also specify with cell of 
the tabulated list to use as entry."
  (interactive "P")
  (let ((cmd (or cmd
                 (read-from-minibuffer "Choco command: " "upgrade -y ")))
        (name (if arg (choco-list-choose)
                (or (and index (aref (tabulated-list-get-entry) index))
                    (tabulated-list-get-id)))))
    (when name
      (w32-shell-execute
       "runas" "powershell"
       (concat 
        (format " -Command \"choco %s %s\";" cmd name)
        "Write-Host \"`n`nHit return to exit\";"
        "$x = $host.UI.RawUI.Readkey('NoEcho,IncludeKeyDown')")))))

;; ------------------------------------------------------------
;;; Dev

(defun choco-dired ()
  (interactive)
  (dired-other-window chocolatey-dir))

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

;; ------------------------------------------------------------

(provide 'choco)

;;; choco.el ends here

;;; scratch-mode.el --- An opinionated major mode for a multi-purpose scratch buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable, set `initial-major-mode' to `scratch-mode':
;;   (setq initial-major-mode 'scratch-mode)

;; It is also recommended to bind `scratch-reset' to a key, for example:
;;   (global-set-key (kbd "C-c s") #'scratch-reset)

;;; Code:

(require 'pcase)

(defgroup scratch-mode nil
  "A specialized mode for the scratch buffer.")

(defcustom scratch-mode-dashboard-on-first-run nil
  "Compute the `scratch-mode' during the Emacs init or only on the subsequent runs."
  :type 'boolean)

;;;###autoload
(defun scratch-reset (skip-dashboard)
  "Reset the *scratch* buffer to its initial `scratch-mode' state.

With the SKIP-DASHBOARD prefix argument, unconditionally skip the
dashboard generation."
  (interactive "P")
  (switch-to-buffer "*scratch*")
  (let ((scratch-mode-dashboard-on-first-run (not skip-dashboard)))
    (scratch-mode)))

(defun scratch-self-insert (&optional desc pre post)
  "Produce a function calling `self-insert-command' in `scratch-mode'.

First switch to `lisp-interaction-mode' or call PRE instead if
supplied.  Then call `self-insert-command' inserting whatever was
pressed.  Finally call POST if it was supplied.

Initially intended as a quick way to switch to
`lisp-interaction-mode' and start a new S-expression, then it
was generalized."
  (let ((cmd (lambda ()
               (interactive)
               (if pre
                   (funcall pre)
                 (lisp-interaction-mode))
               (self-insert-command 1)
               (when post
                 (funcall post)))))
    (if desc
        (cons desc cmd)
      cmd)))

(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'org-mode)
    (define-key map (kbd "e") #'lisp-interaction-mode)
    (when (fboundp 'markdown-mode)
      (define-key map (kbd "m") #'markdown-mode))
    (define-key map (kbd "t") #'text-mode)
    (when user-init-file
      (define-key map (kbd "i") (cons
                                 (abbreviate-file-name user-init-file)
                                 (lambda ()
                                   (interactive)
                                   (find-file user-init-file)))))
    (when early-init-file
      (define-key map (kbd "I") (cons
                                 (abbreviate-file-name early-init-file)
                                 (lambda ()
                                   (interactive)
                                   (find-file early-init-file)))))
    (define-key map (kbd "C") (lambda ()
                                (interactive)
                                (find-library "scratch-mode")))
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "P") #'package-list-packages)
    (when (fboundp 'notmuch)
      (define-key map (kbd "SPC") #'notmuch)
      (define-key map (kbd "s") #'notmuch-search)
      (define-key map (kbd "M") #'notmuch-mua-new-mail))
    (when (fboundp 'deft)
      (define-key map (kbd "z") #'deft))
    (define-key map (kbd "C-c C-x C-j") #'org-clock-goto)
    (define-key map (kbd "j") #'org-clock-goto)
    (when (fboundp 'org-mru-clock-select-recent-task)
      (define-key map (kbd "J") #'org-mru-clock-select-recent-task))
    (define-key map (kbd "c") #'calc)
    (define-key map (kbd "C-'") #'org-cycle-agenda-files)
    (define-key map (kbd "C-c C-w") #'org-refile)
    (define-key map (kbd "a") #'org-agenda)
    (define-key map (kbd "A") #'org-agenda)
    (define-key map (kbd "r") (lookup-key global-map (kbd "C-x r")))
    (define-key map (kbd "g") #'scratch-reset)
    (define-key map (kbd "(") (scratch-self-insert
                               "lisp-interaction-mode + ("))
    map))

(defcustom scratch-mode-key-hints
  `(?o
    ?e
    ?m
    ?t
    ,@(when user-init-file  '(?i))
    ,@(when early-init-file '(?I))
    ?p
    ,@(when (fboundp 'notmuch)
        '(?\s ?s ?M))
    ?z
    ?a
    ?j
    ?J
    ?c)
  "The keymap hints to show in `scratch-mode'.

Use the `(STRING . DEFN)' format of `define-key' to provide
a custom description for a key."
  :type '(repeat character))

(defcustom scratch-mode-dashboard-functions nil
  "A list of functions to show various info in `scratch-mode'."
  :type '(repeat (cons string function)))

(defcustom scratch-mode-dashboard-separator "\n----------------\n"
  "The separator between the `scratch-mode' dashboard and the key hints."
  :type 'string)

(defcustom scratch-mode-show-cursor nil
  "Whether and when to hide the cursor in `scratch-mode'.

Useful mostly if the dashboard contains clickable text or buttons."
  :type '(radio (const :tag "No" nil)
                (const :tag "Yes" t)
                (const :tag "On non-empty dashboard" dashboard)))

(defun scratch-mode-dashboard-generate ()
  "Generate the dashboard section using `scratch-mode-dashboard-functions'."
  (delete nil
          (let ((progress-reporter
                 (make-progress-reporter "Generating scratch-mode dashboard...")))
            (prog1
                (mapcar (lambda (f)
                          (progress-reporter-update progress-reporter (car f))
                          (let ((result (funcall (cdr f))))
                            (when result
                              (format "%s: %s" (car f) result))))
                        scratch-mode-dashboard-functions)
              (progress-reporter-done progress-reporter)))))

(defvar scratch-mode-font-lock-keywords
  '(("^\\(.*?\\):" 1 'bold)
    (": \\(.*\\)$" 1 'italic)))

;;;###autoload
(define-derived-mode scratch-mode special-mode "scratch"
  "A dedicated scratch buffer mode with commonly used commands bound."
  (setq font-lock-defaults '(scratch-mode-font-lock-keywords))

  (unless (eq scratch-mode-show-cursor t)
    (setq cursor-type nil))

  (emacs-lock-mode 'kill)

  ;; chdir to the original working directory.
  (let ((pwd (getenv "PWD")))
    (cd pwd))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (when (and scratch-mode-dashboard-on-first-run
               scratch-mode-dashboard-functions)
      (let ((dashboard (scratch-mode-dashboard-generate)))
        (when dashboard
          (insert (mapconcat #'identity dashboard "\n")
                  scratch-mode-dashboard-separator)
          (when (eq scratch-mode-show-cursor 'dashboard)
            (setq cursor-type t)))))

    (let ((keymap-alist (cdr scratch-mode-map)))
      (dolist (key scratch-mode-key-hints)
        (pcase (alist-get key scratch-mode-map)
          (`(,(and (pred stringp) desc)
             . ,(pred commandp))
           (insert (format "%s: %s\n" (key-description (list key)) desc)))
          ((and (pred commandp) command)
           (insert (format "%s: %s\n" (key-description (list key)) command)))))))

  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (add-hook 'change-major-mode-hook
            (lambda ()
              (read-only-mode 0)
              (erase-buffer))
            nil t))

(provide 'scratch-mode)

;;; scratch-mode.el ends here

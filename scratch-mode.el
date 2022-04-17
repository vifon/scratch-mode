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

;;;###autoload
(defun scratch-reset ()
  "Reset the *scratch* buffer to its initial `scratch-mode' state."
  (interactive)
  (switch-to-buffer "*scratch*")
  (scratch-mode))

(defun scratch-self-insert (&optional pre post)
  "Produce a function calling `self-insert-command' in `scratch-mode'.

First switch to `lisp-interaction-mode' or call PRE instead if
supplied.  Then call `self-insert-command' inserting whatever was
pressed.  Finally call POST if it was supplied.

Initially intended as a quick way to switch to
`lisp-interaction-mode' and start a new S-expression, then it
was generalized."
  (lambda ()
    (interactive)
    (if pre
        (funcall pre)
      (lisp-interaction-mode))
    (self-insert-command 1)
    (when post
      (funcall post))))

(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'org-mode)
    (define-key map (kbd "e") #'lisp-interaction-mode)
    (when (fboundp 'markdown-mode)
      (define-key map (kbd "m") #'markdown-mode))
    (define-key map (kbd "t") #'text-mode)
    (when user-init-file
      (define-key map (kbd "i") (lambda ()
                                  (interactive)
                                  (find-file user-init-file))))
    (when early-init-file
      (define-key map (kbd "I") (lambda ()
                                  (interactive)
                                  (find-file early-init-file))))
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
    (define-key map (kbd "C-'") #'org-cycle-agenda-files)
    (define-key map (kbd "C-c C-w") #'org-refile)
    (define-key map (kbd "a") #'org-agenda)
    (define-key map (kbd "A") #'org-agenda)
    (define-key map (kbd "r") (lookup-key global-map (kbd "C-x r")))
    (define-key map (kbd "g") #'scratch-reset)
    (define-key map (kbd "(") (scratch-self-insert))
    map))

(defcustom scratch-mode-key-hints
  `("o"
    "e"
    "m"
    "t"
    ,@(when user-init-file
        `(("i" . ,(abbreviate-file-name user-init-file))))
    ,@(when early-init-file
        `(("I" . ,(abbreviate-file-name early-init-file))))
    "p"
    ,@(when (fboundp 'notmuch)
        '("SPC" "s" "M"))
    "z"
    "a"
    "j"
    "J")
  "The keymap hints to show in `scratch-mode'.

Each list element should be either:

- a key (the description gets looked up from the keymap), e.g. `\"e\"'
- a cons of a key and a description, e.g. `(\"e\" . \"lisp-interaction-mode\")'
- a cons of a key and a function taking the key and returning the description, e.g.
  `(\"(\" . (lambda (k) (format \"%s + %s\" #'lisp-interaction-mode k)))'"
  :type '(repeat string))

(defcustom scratch-mode-dashboard-functions nil
  "A list of functions to show various info in `scratch-mode'."
  :type '(repeat (cons string function)))

(defcustom scratch-mode-dashboard-separator "\n----------------\n"
  "The separator between the `scratch-mode' dashboard and the key hints."
  :type 'string)

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

;;;###autoload
(define-derived-mode scratch-mode special-mode "scratch"
  "A dedicated scratch buffer mode with commonly used commands bound."
  (setq font-lock-defaults
        '((("^\\(.*?\\):" 1 'bold)
           (": \\(.*\\)$" 1 'italic))))

  (setq cursor-type nil)

  (emacs-lock-mode 'kill)
  (cd "~/")

  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((dashboard (scratch-mode-dashboard-generate)))
      (when dashboard
        (insert (mapconcat #'identity dashboard "\n")
                scratch-mode-dashboard-separator)))
    (dolist (elem scratch-mode-key-hints)
      (pcase elem
        ((and (pred stringp) key)
         ;; Just the key, fetch the description from the bound command.
         (let ((kbind (local-key-binding (kbd key))))
           ;; If the key isn't actually bound, ignore it.  It's most
           ;; likely one of the "opportunistic" keybinds used only
           ;; when some package is installed.
           (when kbind
             (insert (format "%s: %s\n" key kbind)))))
        (`(,(and (pred stringp) key) . ,(and (pred stringp) desc))
         ;; A pair of a key and its description.  Use it verbatim.
         (insert (format "%s: %s\n" key desc)))
        (`(,(and (pred stringp) key) . ,(and (pred functionp) descf))
         ;; A pair of a key and function used to compute the
         ;; description.  Call it with the key as an argument.
         (insert (format "%s: %s\n" key (funcall descf key))))
        (any (warn "Bad scratch-mode hint: %S" any)
             (insert (format "BAD HINT: %S\n" any))))))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (add-hook 'change-major-mode-hook
            (lambda ()
              (read-only-mode 0)
              (erase-buffer))
            nil t))

(provide 'scratch-mode)

;;; scratch-mode.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; fill-column: 78; -*-

;;; init.el --- Mike LaSpina's dot emacs
;;
;; Copyright (c) 2012-2013 Mike LaSpina
;;
;; Author: Mike LaSpina
;; URL: http://github.com/mikelaspina/emacs
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; hide these immediately
(setq inhibit-startup-screen t)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'cl)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specific files are placed."))

;; load paths
(defun msl/add-subdirs-to-load-path (basedir)
  "Add all top-level subdirectories of `basedir' to `load-path'.
This function tries to match the behavior of
`normal-top-level-add-subdirs-to-load-path' except it only considers the
first level of subdirectories of `basedir'."
  (let ((default-directory basedir))
    (normal-top-level-add-to-load-path
     (cl-remove-if-not (lambda (file)
			 ;; Find all subdirs beginning with an alphanumeric
			 ;; character, but exclude VCS directories.
			 (and (string-match-p "\\`[[:alnum:]]" file)
			      (not (member file '("RCS", "CVS", "rcs", "cvs")))
			      ;; Avoid doing a `stat' when it isn't necessary.
			      (not (string-match-p "\\.elc?\\'" file))
			      (file-directory-p file)))
		       (directory-files basedir)))))

(let ((basedir (expand-file-name user-emacs-directory)))
  (cl-labels ((user-dir (name) (file-name-as-directory (concat basedir name))))
    (defvar msl-backups-dir (user-dir "backups"))
    (defvar msl-personal-dir (user-dir "personal"))
    (defvar msl-snippets-dir (user-dir "snippets"))
    (defvar msl-themes-dir (user-dir "themes"))
    (defvar msl-vendor-dir (user-dir "vendor"))))

(add-to-list 'load-path msl-personal-dir)
(add-to-list 'load-path msl-vendor-dir)
(msl/add-subdirs-to-load-path msl-vendor-dir)


;; packages
(when (>= emacs-major-version 24)
  (require 'msl-packages))

(when (and (display-graphic-p)
	   (fboundp 'load-theme))
  (load-theme 'solarized-dark t))

;; minimize annoyances
(setq visible-bell t)

;; Fix environment under Emacs.app.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; editor customizations
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))


(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; global modes
(column-number-mode +1)
(global-hl-line-mode +1)
(ido-mode +1)
(line-number-mode +1)

(setq delete-by-moving-to-trash t)

;; always colorize
(global-font-lock-mode +1)
(setq font-lock-maximum-decoration t)


;; enable backups/autosaves, but isolate them in a separate directory
(setq backup-directory-alist
      `((".*" . ,msl-backups-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,msl-backups-dir t)))

(setq backup-by-copying t     ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)      ; use versioned backups


;; Smarter BOL movement
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1) (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point)) ad-do-it)))


;; Better window movement
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq custom-file (concat msl-personal-dir "custom.el"))
(load custom-file)


;; ui customizations

(defun msl/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame. DELTA
should be a multiple of 10 to match the units used by the :height
face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
	 (new-point-height (/ new-height 10)))
    (set-face-attribute 'default nil :height new-height)
    (message "Default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (msl/increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (msl/increment-default-font-height -10))


(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


;; key bindings
(global-set-key (kbd "C-c D")  'delete-file-and-buffer)
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)


(add-hook 'after-init-hook (lambda () (yas-global-mode))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;
;; prog mode customizations
;;

(add-hook 'prog-mode-hook
	  (lambda ()
	    (fci-mode)
	    (setq fci-rule-column 80)))


(after 'cc-mode
  (c-add-style "apache"
	       '((c-basic-offset . 4)
		 (c-offsets-alist . ((brace-list-intro . +)
				     (defun-block-intro . +)
				     (inclass . +)
				     (inextern-lang . 0)
				     (label . 0)
				     (statement-block-intro . +)
				     (statement-case-intro . +)
				     (substatement . +)))
		 (indent-tabs-mode . nil)))

  (c-add-style "newrelic"
	       '((c-basic-offset . 2)
		 (c-cleanup-list . (brace-else-brace
				    brace-elseif-brace
				    space-before-funcall))
		 (c-hanging-braces-alist . ((substatement-open after)))
		 (c-offsets-alist . ((arglist-cont-nonempty . +)
				     (case-label . +)
				     (inextern-lang . 0)
				     (label . 0)
				     (statement-case-open . +)
				     (substatement-open . 0)))
		 (indent-tabs-mode . nil)))

  (defun msl/c-mode-hook ()
    (c-set-style "apache"))

  (add-hook 'c-mode-hook 'msl/c-mode-hook))


(after 'comint
  (defun msl/comint-mode-hook ()
    (setq comint-process-echoes t))

  (add-hook 'comint-mode-hook 'msl/comint-mode-hook))


(after 'go-mode
  (exec-path-from-shell-copy-env "GOPATH")

  (defun msl-go-mode-hook ()
    (setq tab-width 2)
    (setq gofmt-command "goimports")

    ; use go build to compile
    (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command) "go build -v"))

    ; auto-completion
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)

    (local-set-key (kbd "C-c C-b") #'compile)

    (yas-minor-mode t)
    (add-hook 'before-save-hook 'gofmt-before-save))

  (add-hook 'go-mode-hook 'msl-go-mode-hook))


(after 'sh-script
  (defun msl/sh-mode-hook ()
    (setq sh-basic-offset 2))

  (add-hook 'sh-mode-hook 'msl/sh-mode-hook))


(after 'ag
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search 't))


(after 'js
  (defun msl-js-mode-hook ()
    (setq js-indent-level 2))

  (add-hook 'js-mode-hook 'msl-js-mode-hook))


(after 'php-mode
  (defun msl-php-mode-hook ()
    (php-enable-psr2-coding-style)
    (setq c-basic-offset 2))

  (add-hook 'php-mode-hook 'msl-php-mode-hook))


(autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
(push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)
(push '("Makefile.real$" . makefile-mode) auto-mode-alist)
(push '("[cC]apfile$" . ruby-mode) auto-mode-alist)
(push '("Gemfile$" . ruby-mode) auto-mode-alist)
(push '("Rakefile$" . ruby-mode) auto-mode-alist)
(push '("\\.rake$" . ruby-mode) auto-mode-alist)

;;
;; OS specific customizations
;;

(when (eq system-type 'darwin)
  (load-library "osx.el"))

(when (eq system-type 'windows-nt)
  (load-library "windows.el"))

;;; init.el ends here

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
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; ui customizations
(setq inhibit-startup-screen t
      visible-bell t)

;; autosave/backup customizations
(setq backup-directory-alist
      `((".*" . ,msl-backups-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,msl-backups-dir t)))
(setq backup-by-copying t     ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)      ; use versioned backups

;; editor customizations
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(prefer-coding-system 'utf-8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;
;; global modes
;;

(column-number-mode t)
(global-font-lock-mode t)
(global-hl-mode-hook t)
(ido-mode t)
(line-number-mode t)

(scroll-bar-mode -1)
(toolbar-bar-mode -1)

;;
;; editor customizations
;;

(setq delete-by-moving-to-trash t
      font-lock-maximum-decoration t)

(setq custom-file (concat msl-personal-dir "custom.el"))

;;
;; cc-mode customizations
;;

(eval-after-load "cc-mode"
  '(progn
     (c-add-style "apache"
		  '((brace-list-intro . ++)
		    (defun-block-intro . ++)
		    (inclass . ++)
		    (indent-tabs-mode . nil)
		    (inextern-lang . 0)
		    (label . 0)
		    (statement-block-intro . ++)
		    (statement-case-intro . ++)
		    (substatement . ++)))

     (defun msl/c-mode-hook ()
       (c-set-style "apache"))

     (add-hook 'c-mode-hook 'msl/c-mode-hook)))

;;
;; comint-mode customizations
;;

(eval-after-load "comint"
  '(progn
     (defun msl/comint-mode-hook () 
       (setq comint-process-echoes t))

     (add-hook 'comint-mode-hook 'msl/comint-mode-hook)))

;;
;; go-mode customizations
;;

(require 'go-mode-load)
(eval-after-load "go"
  '(progn
     (defun msl/go-mode-hook ()
       (add-hook 'before-save-hook #'gofmt-before-save)
       (setq tab-width 2))

     (add-hook 'go-mode-hook 'msl/go-mode-hook)))

;;
;; shell-mode customizations
;;

(eval-after-load "shell-mode"
  '(progn
     (defun msl/shell-mode-hook ()
       (setq sh-basic-offset 2))

     (add-hook 'sh-mode-hook 'msl/shell-mode-hook)))

;;
;; powershell-mode customizations
;;

(autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
(push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)

;;
;; OS specific customizations
;;

(when (eq system-type 'darwin)
  (load-library "osx.el"))

(when (eq system-type 'windows-nt)
  (load-library "windows.el"))

;;
;; themes
;;

(when (and (display-graphic-p)
	   (fboundp 'load-theme))
  (load-theme 'zenburn))

;;; init.el ends here

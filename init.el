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
(labels ((user-dir (name) (concat user-emacs-directory name)))
  (defvar msl-backups-dir (user-dir "backups"))
  (defvar msl-personal-dir (user-dir "personal"))
  (defvar msl-snippets-dir (user-dir "snippets"))
  (defvar msl-themes-dir (user-dir "themes"))
  (defvar msl-vendor-dir (user-dir "vendor")))

(add-to-list 'load-path msl-personal-dir)
(add-to-list 'load-path msl-vendor-dir)

;; add vendor subdirs to load-path
(let ((default-directory msl-vendor-dir))
  (normal-top-level-add-to-load-path))

;; packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; ui customizations
(setq inhibit-startup-screen t
      visible-bell t)

;(when (fboundp 'menu-bar-mode)
;  (menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-font-lock-mode t)

(setq column-number-mode t
      delete-by-moving-to-trash t
      font-lock-maximum-decoration t
      line-number-mode t)

(when (and (display-graphic-p)
	   (fboundp 'load-theme))
  (load-theme 'zenburn))

(setq custom-file (concat user-emacs-directory "custom.el"))

;;;
;;; cominit Mode
;;;

(defun my-comint-init () 
  (setq comint-process-echoes t)) 
(add-hook 'comint-mode-hook 'my-comint-init)

;;;
;;; Shell Mode
;;;

(defun my-shell-mode-hook ()
  (setq sh-basic-offset 2))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;;;
;;; Powershell Mode
;;;

(autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
(push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)

;;;
;;; Go Mode
;;;

(require 'go-mode-load)
(eval-after-load "go"
  '(progn
     (defun my-go-mode-hook ()
       (add-hook 'before-save-hook #'gofmt-before-save)
       (setq tab-width 2))
     (add-hook 'go-mode-hook 'my-go-mode-hook)))

;;;
;;; System specific customizations
;;;

(when (eq system-type 'darwin)
  (load-library "osx.el"))

(when (eq system-type 'windows-nt)
  (load-library "windows.el"))

;;; init.el ends here

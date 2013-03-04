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

(defvar msl-emacs-root (expand-file-name "~/.emacs.d/"))
(defvar msl-personal-dir (concat msl-emacs-root "personal"))
(defvar msl-snippets-dir (concat msl-emacs-root "snippets"))
(defvar msl-themes-dir (concat msl-emacs-root "themes"))
(defvar msl-vendor-dir (concat msl-emacs-root "vendor"))

(add-to-list 'load-path msl-personal-dir)
(add-to-list 'load-path msl-vendor-dir)

;;;
;;; Global customizations
;;;

;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq column-number-mode t)
(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((sql-set-product . sqlite)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

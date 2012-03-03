;;;
;;; Mike LaSpina's dot emacs
;;;

(require 'cl)

(defvar my-emacs-root "~/.emacs.d/")

;; Setup custom load paths
(add-to-list 'load-path (concat my-emacs-root "lisp"))
(add-to-list 'load-path (concat my-emacs-root "site-lisp"))

;;;
;;; Global customizations
;;;

;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-font-lock-mode t)

(setq column-number-mode t)
(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

;;;
;;; Powershell Mode
;;;

(autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
(push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)

;;;
;;; System specific customizations
;;;

(when (eq system-type 'darwin)
  (load-library "osx.el"))

(when (eq system-type 'windows-nt)
  (load-library "windows.el"))

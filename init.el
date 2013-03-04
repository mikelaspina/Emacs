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

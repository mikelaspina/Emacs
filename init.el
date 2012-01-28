;;;
;;; Mike LaSpina's dot emacs
;;;

(require 'cl)

(defvar my-emacs-root "~/.emacs.d/")

;;;
;;; Global customizations
;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-font-lock-mode t)

(setq column-number-mode t)
(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

;;;
;;; System specific customizations
;;;

(when (eq system-type 'darwin)
  (load-file (concat my-emacs-root "osx.el")))

(when (eq system-type 'windows-nt)
  (load-file (concat my-emacs-root "windows.el")))

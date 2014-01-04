;;; -*- mode: emacs-lisp; coding: utf-8; fill-column: 78; -*-

;;; msl-packages.el --- Mike LaSpina's dot emacs
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

(require 'package)

(defvar msl/packages
  '(ag
    clojure-mode
    csharp-mode
    exec-path-from-shell
    fill-column-indicator
    go-mode
    groovy-mode
    json-mode
    markdown-mode
    php-mode
    solarized-theme
    toml-mode
    yaml-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun msl/packages-installed-p ()
  (loop for p in msl/packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (msl/packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p msl/packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'msl-packages)

;;; msl-packages.el ends here

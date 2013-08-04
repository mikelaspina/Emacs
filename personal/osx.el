;;; osx.el --- Customizations for Mac OS X
;;
;; Copyright (c) 2012-2013 Mike LaSpina
;;
;; Author: Mike LaSpina
;; URL: http://github.com/mikelaspina/emacs
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

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

;; Improve keybindings
(setq mac-function-modifier 'control)
(setq mac-command-modifier 'meta)

;; If we're not running in a terminal, use Menlo 16.
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist
	       '(font . "-apple-Menlo-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")))

;;; osx.el ends here

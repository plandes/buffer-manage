;;; config-manage-core.el --- configuration management core

;; Copyright (C) 2017 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: configuration settings persistable

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This contains core functions for the configuration management system.

;;; Code:

(defvar config-manager-instance)

;;;###autoload
(defmacro config-manage-declare-functions (&rest fns)
  "Declare functions in list FNS for the purposes of silencing the compiler.

This is used in the compiler module libraries to silence the compiler in
`eval-when-compile' scopes."
  `(eval-when-compile
     (mapcar #'(lambda (sym)
		 (unless (fboundp sym)
		   (eval `(defun ,sym (&rest x)
			    (error "Bad declare order")))))
	     (quote ,fns))))

;;;###autoload
(defmacro config-manage-declare-variables (&rest vars)
  "Declare variables in list VARS for the purposes of silencing the compiler.

This is used in the compiler module libraries to silence the compiler in
`eval-when-compile' scopes."
  `(eval-when-compile
     (mapcar #'(lambda (sym)
		 (unless (fboundp sym)
		   (eval `(defvar ,sym nil))))
	     (quote ,vars))))

(defun config-manage-mode-assert (&optional no-error-p this)
  "Throw an error if not in `config-manage-mode' when NO-ERROR-P is nil.

Pattern match on THIS if it is given and this is a `config-manager-mode'."
  (let ((matchp (and (eq major-mode 'config-manage-mode)
		     (or (not this)
			 (equal this config-manager-instance)))))
    (if (and (not no-error-p) (not matchp))
	(error "Must be in `config-manage-mode' for this command or wrong manager")
      matchp)))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("config-manage-delcare-functions" . font-lock-keyword-face)
    ("config-manage-delcare-variables" . font-lock-keyword-face)))

(provide 'config-manage-core)

;;; config-manage-core.el ends here

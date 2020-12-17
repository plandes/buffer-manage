;;; config-manage-declare.el --- Configuration management core  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: internal maint
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "26.0"))
;; Package-Version: 0

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


(require 'eieio)
(require 'eieio-core)

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
(defmacro config-manage-declare-methods (&rest fns)
  "Declare methods in list FNS for the purposes of silencing the compiler.

This is used in the compiler module libraries to silence the compiler in
`eval-when-compile' scopes."
  `(eval-when-compile
     (mapcar #'(lambda (sym)
		 (unless (fboundp sym)
		   (eval `(cl-defgeneric ,sym (&rest x)
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

(defun config-manage-declare-mode-assert (&optional no-error-p this)
  "Throw an error if not in `config-manage-mode' when NO-ERROR-P is nil.

Pattern match on THIS if it is given and this is a `config-manager-mode'."
  (let ((matchp (and (eq major-mode 'config-manage-mode)
		     (or (not this)
			 (equal this config-manager-instance)))))
    (if (and (not no-error-p) (not matchp))
	(error "Must be in `config-manage-mode' for this command or wrong manager")
      matchp)))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("config-manage-declare-functions" . font-lock-keyword-face)
    ("config-manage-declare-methods" . font-lock-keyword-face)
    ("config-manage-declare-variables" . font-lock-keyword-face)))

(defun config-manage-declare-slots (class)
  "Return an alist of slots for EIEIO CLASS.

This is a helper function and probably shouldn't be trusted to work long term
since it uses code ripped off from EIEIO guts."
  (let ((slots (eieio--class-slots (cl--find-class class))))
    (mapcar #'(lambda (i)
		(let* ((sd (aref slots i))
		       (doc (alist-get :documentation
				       (cl--slot-descriptor-props sd))))
		  `(,(cl--slot-descriptor-name sd) .
		    ((init . ,(cl--slot-descriptor-initform sd))
		     (documentation . ,doc)
		     (type . ,(cl--slot-descriptor-type sd))))))
	    (number-sequence 0 (1- (length slots))))))

(define-error 'config-manage-un-implemented
  "Un-implemented method config-manage method"
  'cl-no-applicable-method)

(provide 'config-manage-declare)

;;; config-manage-declare.el ends here

;;; config-manage-mode.el --- Manage abstract configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: internal maint
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "26"))
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

;; This library provides functionality for the configuration entry listing.  It
;; allows the user to "graphically" add, delete, rename (and other operations)
;; configuration entries.

;;; Code:


(require 'dash)
(require 'config-manage-declare)
(require 'config-manage-base)
(require 'config-manage-prop)

(defvar config-entry-status)

(config-manage-declare-variables
 config-manager-instance
 config-manage-on-mouse-down
 org-window-config)

(config-manage-declare-methods buffer-manager-switch)

;;; mode
(defcustom config-manage-highlight t
  "Whether to hightlight buffer using `config-manager-list-entries-buffer'."
  :group 'config-manage
  :type 'boolean)

(defgroup config-manage-font-lock-faces nil
  "Buffer Manage Faces"
  :group 'config-manage
  :prefix "config-manage-font-lock-")

;; face definitions
(defface config-manage-font-lock-headers-face
  '((t (:foreground "red")))
  "Font Lock mode face used to highlight entry headers."
  :group 'config-manage-font-lock-faces)
(defface config-manage-font-lock-name-face
  '((t (:foreground "darkcyan")))
  "Font Lock mode face used to highlight entry names."
  :group 'config-manage-font-lock-faces)
(defface config-manage-font-lock-desc-face
  '((t (:foreground "blue")))
  "Font Lock mode face used to highlight description."
  :group 'config-manage-font-lock-faces)

;; font variables
(defvar config-manage-font-lock-headers-face
  'config-manage-font-lock-headers-face
  "Face headers to use for headerss.")
(defvar config-manage-font-lock-name-face
  'config-manage-font-lock-name-face
  "Face name to use for names.")
(defvar config-manage-font-lock-desc-face
  'config-manage-font-lock-desc-face
  "Face name to use for working directories.")

(defvar config-manage-font-lock-keywords
  `((,(format "^.\\{%d\\}\\(.*?\\)[ \t]+.*$"
	      (1+ config-manage-base-list-col-space))
     1 config-manage-font-lock-name-face t)
    (,(format "^.\\{%d\\}.*?[ \t]+\\(.*\\)$"
	      (1+ config-manage-base-list-col-space))
     1 config-manage-font-lock-desc-face t)
    ("^\\([- \t]+\\)$" 1 config-manage-font-lock-headers-face t))
  "Additional expressions to highlight in config manage mode.")

(defun config-manage-mode-quit ()
  "Quit from within the `config-manage-mode'."
  (interactive)
  (config-manage-mode-assert)
  (if t
      (bury-buffer)
    (let ((cfg org-window-config))
      (kill-buffer (current-buffer))
      (set-window-configuration cfg))))

(defun config-manage-mode-name-at-point ()
  "Return the name of the entry at the current point if there is one."
  (config-manage-mode-assert)
  (save-excursion
    (beginning-of-line)
    (forward-char (+ (length (cdar config-manager-status-defs))
		     config-manage-base-list-col-space))
    (if (looking-at "\\(.+?\\)[ \t]")
	(match-string-no-properties 1))))

(defun config-manage-mode-mouse-down (event)
  "Call back for mouse down events.
EVENT mouse event data."
  (interactive "e")
  (mouse-set-point event)
  (setq config-manage-on-mouse-down (config-manage-mode-name-at-point)))

(defun config-manage-mode-mouse-up (event)
  "Call back for mouse down events.
EVENT mouse event data."
  (interactive "e")
  (mouse-set-point event)
  (let ((name (config-manage-mode-name-at-point)))
    (if (string= name config-manage-on-mouse-down)
	(config-manage-mode-activate-entry name))))

(defun config-manage-mode-next ()
  "Called by pressing the `tab' key in `config-manage-mode'."
  (interactive)
  (config-manage-mode-assert)
  (beginning-of-line)
  (unless (save-excursion (end-of-line) (eobp))
    (forward-line)))

(defun config-manage-mode-previous ()
  "Called by pressing the `tab' key in `config-manage-mode'."
  (interactive)
  (config-manage-mode-assert)
  (beginning-of-line)
  (if (> (line-number-at-pos (point)) 3)
      (forward-line -1)))

(defun config-manage-mode-activate-entry (&optional name)
  "Activates the entry with name NAME."
  (interactive)
  (config-manage-mode-assert)
  (setq name (or name (config-manage-mode-name-at-point)))
  (let ((this config-manager-instance))
    (config-manage-mode-assert)
    (config-manager-activate this name)))

(defun config-manage-mode-view (&optional name)
  "Activates the entry with name NAME."
  (interactive)
  (config-manage-mode-assert)
  (setq name (or name (config-manage-mode-name-at-point)))
  (let ((this config-manager-instance))
    (when (child-of-class-p (eieio-object-class this)
			    'buffer-manager)
      (let ((win (selected-window)))
	(buffer-manager-switch this name nil 'split)
	(select-window win)))))

(defun config-manage-mode-info (&optional name)
  "Display information about the entry NAME in a separate buffer."
  (interactive)
  (config-manage-mode-assert)
  (setq name (or name (config-manage-mode-name-at-point)))
  (let* ((this config-manager-instance)
	 (entry (config-manager-entry this name)))
    (config-prop-entry-info entry)))

(defun config-manage-mode-edit (&optional name)
  "Edit the entry with NAME."
  (interactive)
  (config-manage-mode-assert)
  (setq name (or name (config-manage-mode-name-at-point)))
  (let* ((this config-manager-instance)
	 (entry (config-manager-entry this name)))
    (when (child-of-class-p (eieio-object-class entry)
			    'config-prop-entry)
      (config-prop-entry-configure entry nil))))

(defun config-manage-mode-set-status (status)
  "Set the mode status to STATUS for the mode."
  (config-manage-mode-assert)
  (let ((name (config-manage-mode-name-at-point)))
    (when name
      (puthash name status config-entry-status)
      (config-manage-mode-refresh)
      (config-manage-mode-next))))

(defun config-manage-mode-mark-delete ()
  "Delete a buffer (terminate)."
  (interactive)
  (config-manage-mode-set-status 'to-delete))

(defun config-manage-mode-mark-show ()
  "Display \(show) a buffer."
  (interactive)
  (config-manage-mode-set-status 'to-show))

(defun config-manage-mode-mark-undelete ()
  "Unmark a buffer for deletion."
  (interactive)
  (config-manage-mode-set-status 'alive))

(defun config-manage-mode-apply-selected (status replace func)
  "Apply STATUS to the selection.
Replace status for REPLACE and the selection uses the return
value of FUNC."
  (config-manage-mode-assert)
  (let ((this config-manager-instance))
    (maphash #'(lambda (key val)
		 (when (eq status val)
		   (let ((entry (config-manager-entry this key)))
		     (and entry (funcall func this entry)))
		   (if replace
		       (puthash key replace config-entry-status)
		     (remhash key config-entry-status))))
	     config-entry-status)
    (config-manage-mode-refresh)))

(defun config-manage-mode-delete-selected ()
  "Delete all entries that are selected for delete."
  (interactive)
  (config-manage-mode-assert)
  (config-manage-mode-apply-selected 'to-delete nil
				     'config-manager-remove-entry))

(defun config-manage-mode-rename (new-name)
  "Rename a buffer to NEW-NAME."
  (interactive
   (progn
     (config-manage-mode-assert)
     (let ((this config-manager-instance))
       (list (config-manager-read-new-name this "Rename")))))
  (config-manage-mode-assert)
  (let ((name (config-manage-mode-name-at-point))
	(this config-manager-instance))
    (config-entry-set-name (config-manager-entry this name) new-name)
    (config-manage-mode-refresh)))

(defun config-manage-mode-new ()
  "Create a new entry."
  (interactive)
  (config-manager-insert-entry config-manager-instance)
  (config-manage-mode-refresh))

(define-derived-mode config-manage-mode fundamental-mode "Configuration Manager"
  "Major mode for displaying and buffer entries.
Special commands:
\\{config-manage-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(config-manage-font-lock-keywords t))
  (font-lock-mode (if config-manage-highlight 1 0))
  (let ((cfg (current-window-configuration)))
   (set (make-local-variable 'org-window-config) cfg)))

(define-key config-manage-mode-map "q" 'config-manage-mode-quit)
(define-key config-manage-mode-map [down-mouse-2] 'config-manage-mode-mouse-down)
(define-key config-manage-mode-map [mouse-2] 'config-manage-mode-mouse-up)
(define-key config-manage-mode-map [return] 'config-manage-mode-activate-entry)
(define-key config-manage-mode-map "n" 'config-manage-mode-next)
(define-key config-manage-mode-map "p" 'config-manage-mode-previous)
(define-key config-manage-mode-map [(control down)] 'config-manage-mode-next)
(define-key config-manage-mode-map [(control up)] 'config-manage-mode-previous)
(define-key config-manage-mode-map "d" 'config-manage-mode-mark-delete)
(define-key config-manage-mode-map "s" 'config-manage-mode-mark-show)
(define-key config-manage-mode-map "u" 'config-manage-mode-mark-undelete)
(define-key config-manage-mode-map "i" 'config-manage-mode-new)
(define-key config-manage-mode-map "x" 'config-manage-mode-delete-selected)
(define-key config-manage-mode-map "g" 'config-manage-mode-refresh)
(define-key config-manage-mode-map "r" 'config-manage-mode-rename)
(define-key config-manage-mode-map "v" 'config-manage-mode-view)
(define-key config-manage-mode-map "?" 'config-manage-mode-info)
(define-key config-manage-mode-map "e" 'config-manage-mode-edit)

(defvar config-manage-mode-menu-definition
  (list "Config Manage"
	["Create New" config-manage-mode-new t]
	["Goto Entry" config-manage-mode-activate-entry t]
	"-"
	["Mark Delete" config-manage-mode-mark-delete t]
	["Unmark" config-manage-mode-mark-undelete t]
	["Rename" config-manage-mode-rename t]
	["Delete Selected" config-manage-mode-delete-selected t]
	"-"
	["Refresh" config-manage-mode-refresh t]
	["Quit" config-manage-mode-quit t]))


;;; base
(cl-defmethod config-manager-list-entries-buffer ((this config-manager)
						  &optional buffer-name)
  "Create a listing of buffers used for viewing, renameing, deleting, adding.
BUFFER-NAME is the name of the buffer holding the entries for the mode.
THIS is the instance."
  (let* ((buffer-name (or buffer-name (->> (config-manager-name this)
					   (format "*%s*"))))
	 (buf (get-buffer buffer-name))
	 (newp (not buf))
	 (buf (or buf (get-buffer-create buffer-name))))
    (save-excursion
      (eval-and-compile
	(let ((msg (concat "we need `save-excursion' since interactively "
			   "called `config-manage-mode-refresh' sets "
			   "the window point")))
	  (display-warning 'config-manage msg :debug)))
      ;; silence the compiler
      (apply #'set-buffer (list buf))
      (if (not newp)
	  (config-manage-mode-refresh)
	(setq buffer-read-only nil)
	(erase-buffer)
	(config-manage-mode)
	(config-manager-list-entries this)
	(set (make-local-variable 'config-manager-instance) this)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(easy-menu-define config-manage-mode-menu config-manage-mode-map
	  "Menu for Buffer Manage." config-manage-mode-menu-definition)))
    (switch-to-buffer buf)))

(provide 'config-manage-mode)

;;; config-manage-mode.el ends here

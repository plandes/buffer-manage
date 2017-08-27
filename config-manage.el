;;; config-manage.el --- manage abstract configurations

;; Copyright (C) 2017 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: configuration settings persistable
;; URL: https://github.com/plandes/config-manage
;; Package-Requires: ((emacs "25") (choice-program "0.1") (dash))

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

;; This package provides abstract behavior for EIEIO classes that are meant to
;; be extended.  This library provides classes that:

;; - Provide a way to manage configuration in a simplier file persistence than
;;   the EIEIO default
;; - GUI (mode buffer) that provides an easy interface to create, read, update
;;   and destroy configuration objects that then CRUD through to disk
;;   (optionally).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'time-stamp)
(require 'dash)
(require 'eieio)
(require 'choice-program)


;; quiet the compiler
(defvar config-entry-status)
(defvar config-manager-instance)
(defvar config-manage-mode-map)
(defvar config-manage-mode-menu-definition)
(defvar config-manage-on-mouse-down)
(defvar org-window-config)



(defclass config-persistent ()
  ((pslots :initarg :pslots
	  :initform nil
	  :type list))
  :documentation "\
Super class for objects that want to persist to the file system.

This class is necessary since EIEIO list types can't unpersist as they produce
this error:

  eieio-persistent-validate/fix-slot-value: In save file, list of object
  constructors found, but no :type specified for slot displays of type nil")

(cl-defmethod config-persistent-persist-value ((this config-persistent) val)
  (or (and (consp val)
	   (or (let ((fval (car val)))
		 (and fval
		      (eieio-object-p fval)
		      (object-of-class-p fval 'config-persistent)
		      (-map (lambda (val)
			      (config-persistent-persist val))
			    val)))))
      val))

(cl-defmethod config-persistent-persist-slots ((this config-persistent))
  "Persist the slots of the instance."
  (with-slots (pslots) this
    (->> pslots
	 (-map (lambda (slot)
		 (let ((val (->> (slot-value this slot)
				 (config-persistent-persist-value this))))
		   (cons slot val)))))))

(cl-defmethod config-persistent-persist ((this config-persistent))
  "Persist an object."
  (append `((class . ,(eieio-object-class this))
	    (slots . ,(config-persistent-persist-slots this)))
	  (condition-case nil
	      (cl-call-next-method this)
	    (cl-no-next-method))))

(cl-defmethod config-persistent-unpersist-value ((this config-persistent) val)
  "Unpersist VAL by determining its type and then recursively applying to
create unerpsist \(optionally) children classes and slots."
  (or (and (consp val)
	   (or (let ((fval (car val)))
		 (and (consp fval)
		      (consp (car fval))
		      (eq 'class (caar fval))
		      (-map (lambda (val)
			      (config-persistent-unpersist val))
			    val)))))
      val))

(cl-defmethod config-persistent-unpersist ((this config-persistent) vals)
  "Persist an object."
  (with-slots (pslots) this
    (->> pslots
	 (-map (lambda (slot)
		 (let ((val (->> (cdr (assq slot vals))
				 (config-persistent-unpersist-value this))))
		   (setf (slot-value this slot) val)))))))

(cl-defmethod config-persistent-unpersist ((vals list))
  "Restore the objects state from VALS, which has a symbols
`class' and `slots'."
  (let* ((class (cdr (assq 'class vals)))
	 (slots (cdr (assq 'slots vals)))
	 (obj (make-instance class)))
    (config-persistent-unpersist obj slots)
    obj))

(cl-defmethod object-print-fields ((this config-persistent)) nil)

(cl-defmethod object-print ((this config-persistent) &optional strings)
  "Return a string as a representation of the in memory instance of THIS."
  (cl-flet* ((format-obj
  	      (slot)
  	      (let ((obj (eieio-oref this slot)))
  		(format "%S %s"
  			slot
  			(cond ((eieio-object-p obj) (object-print obj))
  			      ((stringp obj) (format "'%s'" obj))
  			      (obj))))))
    (let ((fields (object-print-fields this)))
      (apply #'cl-call-next-method this
	     (cons (concat (if fields " ")
			   (mapconcat #'format-obj
				      fields
				      " "))
		   strings)))))



(defclass config-persistable (config-persistent)
  ((file :initarg :file
	 :initform nil
	 :type (or null string)
	 :documentation "The file to persist the state of the object."))
  :documentation "Subclasses that can persist to a file.")

(cl-defmethod config-persistable-save ((this config-persistable))
  "Persist manager and compiler configuration."
  (with-slots (file) this
    (when file
     (let ((save-class-name (->> this eieio-object-class eieio-class-name))
	   (state (config-persistent-persist this)))
       (with-temp-buffer
	 (insert (format "\
;; -*- emacs-lisp -*- <%s %s>
;; Object: %s.  Don't change this file.\n"
			 (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
			 file save-class-name))
	 (insert (with-output-to-string
		   (pp state)))
	 (write-region (point-min) (point-max) file))
       (message "Wrote %s" file)))))



;;; config objects
(defclass config-entry (config-persistent)
  ((name :initarg :name
	 :initform nil
	 :type (or null string)
	 :reader config-entry-name
	 :writer config-entry-set-name
 	 :protection :protected)
   (description :initarg :description
		:initform "<none>"
		:reader config-entry-description
		:type string
		:documentation "
The description of this entry, used in `config-manager-list-entries-buffer'.")
   (manager :initarg :manager
	    :initform nil
	    :protection :protected))
  :abstract true
  :documentation "Abstract class for all configurable entries.")

(cl-defmethod config-entry-save ((this config-entry))
  "Save the current entry configuration."
  nil)

(cl-defmethod config-entry-restore ((this config-entry))
  "Restore the current entry configuration."
  nil)

(cl-defmethod config-entry-rename ((this config-entry) name)
  "Rename the config entry to NAME and return the new entry."
  (with-slots (name) this
    (oset this :name name)))



(defclass config-manager (config-persistent)
  ((name :initarg :name
	 :initform "untitled"
	 :reader config-manager-name
	 :type string
	 :documentation "Name of this configuration manager.")
   (cycle-method :initarg :cycle-method
		 :initform last-visit
		 :reader config-manager-cycle-method
		 :writer config-manager-set-cycle-method
		 :type symbol
		 :custom (choice :tag "Cycle Method"
				 (const :tag "Last Visited" last-visit)
				 (const :tag "Next In Line" next))
		 :documentation "\
How entries are cycled by default when invoking `config-manager-activate'.
This parameter is used as the default for `criteria' \(see
`config-manager-activate'), which is `cycle'.")
   (entries :initarg :entries
	    :initform nil		; initialize with 0 entries
	    :type (or null list)
	    ;:reader config-manager--entries
	    :protection :private
	    :documentation
	    "Contains the data structure for the buffer entries.")
   (list-header-fields :initarg :list-header-fields
		       :initform '("C" "Name"  "Description")
		       :type list
		       :documentation "\
List of fields used in output of `buffer-list'.")

   ;; careful: this slot keeps stale entries after they've been removed/killed
   (last-switched-to :initform nil
		     :protection :private
		     :documentation "\
Keeps track of the last entry for last-visit cycle method."))
  :documentation "Manages configurations.")

(defconst config-manager-list-col-space 4
  "Space between columns.")

(defconst config-manager-status-defs
  '((alive . " ")
    (to-delete . "D")
    (to-show . "S"))
  "Enumeration of status for buffers in `config-manager-list-entries'.")

(defun config-manager-insert-at-position (seq elt pos)
  "Return SEQ with ELT inserted at position POS."
  (append (cl-subseq seq 0 pos)
	  (list elt)
	  (cl-subseq seq pos)))

(cl-defmethod config-manager-new-entry ((this config-manager) &optional slots)
  "Create a new nascent entry object.
SLOTS are passed as a property list on instantiating the object."
  (error "No implementation of `config-manager-new-entry' for class `%S'"
	 (eieio-object-class this)))

(cl-defmethod config-manager-entry-default-name ((this config-manager))
  "Return a name for a new entry created with `config-manager-new-entry'."
  (error "No implementation of `config-manager-entry-default-name' for class `%S'"
	 (eieio-object-class this)))

(cl-defmethod config-manager--update-entries ((this config-manager) entries)
  "Observer pattern to for observers to react to entries modifications.")

(cl-defmethod config-manager-cycle-entries ((this config-manager) entry
					    &optional mode)
  "Rearrange the entry order to place ENTRY in place after cycling."
  (with-slots (entries cycle-method) this
    (let ((first-entry (car entries)))
      (setq entries (cons entry (remove entry entries)))
      (if (and (eq 'next cycle-method)
	       (> (length entries) 1)
	       (not (eq first-entry entry)))
	  (setq entries (append (remove first-entry entries)
				(if first-entry (list first-entry)))))
      (config-manager--update-entries this entries)
      entries)))

(cl-defmethod config-manager-entry-exists-p ((this config-manager) entry)
  "If ENTRY is an instance of a class or subclass of `config-entry' return it."
  (with-slots (entries) this
    (and (eieio-object-p entry)
	 (object-of-class-p entry 'config-entry)
	 (member entry entries)
	 entry)))

(cl-defmethod config-manager-entry ((this config-manager)
				    criteria &optional assertp)
  "This returns an entry based on CRITERIA.
CRITERIA is:
  a string: the entry name to switch to the entry with that name
  an integer: get it by index
  a symbol: if `first', the highest priority entry is selected,
	    if `last' the last most priority entry is selected,
	    if `next' the next entry in the list or start back with the first
	    if `cycle' the next (after the current) most desirable
	    entry is selected based on the value of slot `cycle-method'"
  (let* ((entries (config-manager--entries this))
	 (len (length entries))
	 entry)
    (setq entry
	  (cond ((stringp criteria)
		 (cl-block entry-lookup
		   (dolist (entry (config-manager--entries this))
		     (if (equal criteria (config-entry-name entry))
			 (cl-return-from entry-lookup entry)))))
		((config-manager-entry-exists-p this criteria))
		((= len 0) nil)
		((= len 1) (car entries))
		((eq criteria 'first) (car entries))
		((eq criteria 'last) (last entries))
		;; TODO
		((eq criteria 'cycle) (config-manager-entry-cycle this))
		(t (error "Illegal argument for criteria: %S"
			  criteria))))
    (if (and assertp (null entry))
	(error "No entry exists that satisfies criteria `%S'" criteria))
    entry))

(cl-defmethod config-manager--entries ((this config-manager)
				       &optional include-fn exclude-fn sort-form)
  "Return entries that match INCLUDE-FN and don't match EXCLUDE-FN.
Entries returned are only entries contained in this instance of the
`config-manager'.

Sorting on the returned entries are done when SORT-FORM is non-`nil'.  Any
sorting is only done on the returned set of entries and doesn't change any
of the object's internal state.  Sorting is done based on SORT-FORM's value:
 - symbol 'lexical: sort lexically based on the config entry's name
 - function: sort using SORT-FORM as a predicate \(see `sort')."
  (with-slots (entries) this
    (setq include-fn (or include-fn (lambda (entry) t))
	  exclude-fn (or exclude-fn (lambda (entry) nil)))
    (let ((entries
	   (remove nil (mapcar (lambda (entry)
				 (if (and (funcall include-fn entry)
					  (not (funcall exclude-fn entry)))
				     entry))
			       entries))))
      (when sort-form
	(cl-flet ((lexical-fn
		   (a b)
		   (string< (config-entry-name a) (config-entry-name b))))
	  (let ((sort-fn (cond ((eq sort-form 'lexical) 'lexical-fn)
			       ((functionp sort-form) sort-form)
			       (t (error "Illegal sort form: %S" sort-form)))))
	    (setq entries (sort entries sort-fn)))))
      entries)))

(cl-defmethod config-manager-current-instance ((this config-manager)
					       &optional assertp)
  "The current entry we're in/at, if there is one.

ASSERTP, if non-nil, raise an error if there is no current entry."
  (with-slots (last-switched-to) this
    (or last-switched-to
	(cl-first (config-manager--entries this))
	(and assertp
	     (error "No current entry")))))

(cl-defmethod config-manager-entry-cycle ((this config-manager))
  "Return the next `cycled' entry based on slot `cycle-method'.
The default uses:
  last-visit: go to the last visited entry
	next: go to the next highest priority entry"
  (with-slots (last-switched-to cycle-method) this
    (let ((entries (config-manager--entries this))
	  ;; the current entry we're in (if there is one)
	  (cur-entry (config-manager-current-instance this))
	  (method cycle-method))
      (if (not (member method (config-manager-cycle-methods this)))
	  (error "Invalid cycle method: %S" method))
      (or
       ;; only can switch to one if there is one
       (if (= (length entries) 1) (car entries))
       ;; try to use the last entry that was switched into if we can
       (if (and last-switched-to
		;; don't pick the one we are in or we won't switch at all
		(not cur-entry))
	   last-switched-to)
       (let ((bak-entries (copy-tree entries)))
	 (or
	  (cl-case method
	    (last-visit (cl-second entries))
	    (next (cl-second entries))
	    (otherwise (error "Unimplemented (but value) cycle method: %S"
			      method)))
	  (car entries)))))))

(defun config-manager-iterate-name (name names)
  "Create a unique NAME from existing NAMES by iterating FORM<N> integer.

where N is an integer.

This is the typical unique name (buffers, files etc) creation."
  (let* ((regex "\\(?:<\\([0-9]+\\)>\\)?$")
	 (idxs (->> names
		    (-map (lambda (elt)
			    (if (string-match (concat "^" name regex) elt)
				(let ((expr (match-string 1 elt)))
				  (or (and expr (read expr)) 1)))))
		    (-filter #'identity)
		    (cons 0)))
	 (to-remove (->> (apply #'max idxs)
			 1+
			 (number-sequence 1)))
	 (idx (car (seq-difference to-remove idxs))))
    (if (= 1 idx)
	name
      (concat name "<" (-> idx prin1-to-string) ">"))))

(cl-defmethod config-manager-add-entry ((this config-manager) &optional slots)
  "Add and optionally create first a new entry if ENTRY is nil."
  (let* ((entry (config-manager-new-entry this slots))
  	 (name (or (config-entry-name entry)
		   (config-manager-entry-default-name this))))
    (with-slots (entries entry-index) this
      (->> entries
  	   (-map (lambda (elt)
  		   (config-entry-name elt)))
  	   (config-manager-iterate-name name)
  	   (config-entry-set-name entry))
      (config-manager-cycle-entries this entry 'after)
      entry)))

(cl-defmethod config-manager-insert-entry ((this config-manager)
					   &optional context)
  (config-manager-add-entry this))

(cl-defmethod config-manager-set-name ((this config-manager)
				       &optional new-name)
  "Set the name of this `config-manager' to NEW-NAME."
  (with-slots (name) this
    (let ((new-name (or new-name (config-manager-entry-default-name this))))
      (setq name new-name))))

(cl-defmethod config-manager-entry-restore ((this config-manager)
					    &optional entry)
  "Restore this `config-manager' and contained `config-entry' instances."
  (let ((entry (or entry (config-manager-entry this 0))))
    (config-entry-restore entry)))

(cl-defmethod config-manager-remove-entry ((this config-manager) entry)
  "Remove/kill ENTRY from this manager."
  (with-slots (entries) this
    (when (memq entry entries)
      (let ((name (config-entry-name entry)))
	(setq entries (remove entry entries))
	(config-manager--update-entries this entries)
	(destructor entry)
	entry))))

(cl-defmethod config-manager-activate ((this config-manager) criteria)
  "Switch to a config entry.

If the config CRITERIA is the name of the config to switch to, go to that
config, otherwise, create a new one with that name and switch to it.
Returns the config entry we switched to based on CRITERIA \(see
`config-manager-entry')."
  (let ((entry (or (config-manager-entry this criteria)
		   (apply #'config-manager-new-entry
			  this (and (stringp criteria)
				    `(:name ,criteria))))))
    (config-entry-restore entry)
    (config-manager-cycle-entries this entry)
    (config-manage-refresh-windows)
    entry))

(cl-defmethod config-manager-list-clear ((this config-manager))
  (with-slots (entries) this
    (setq entries nil)
    (config-manager--update-entries this entries)))

(cl-defmethod config-manager-cycle-methods ((this config-manager))
  "All valid cycle methods (see `config-manager-entry-cycle')."
  '(last-visit next))

(cl-defmethod config-manager-toggle-cycle-method ((this config-manager))
  (let* ((methods (config-manager-cycle-methods this))
	 (method (config-manager-cycle-method this)))
    (setq method (or (cadr (member method methods)) (car methods)))
    (config-manager-set-cycle-method this method)
    method))

(cl-defmethod config-manager-list-entries ((this config-manager))
  "Return a multi-listing of the entries contained in this manager."
  (cl-flet* ((get-max
	      (getter-fn)
	      (let ((entries (config-manager--entries this)))
		(when entries
		  (apply #'max
			 (mapcar (lambda (entry)
				   (length (funcall getter-fn entry)))
				 entries)))))
	     (get-desc
	      (entry col-space name-len)
	      (let* ((name (config-entry-description entry))
		     (len (length name))
		     (width 79)
		     (max-len (- (- width col-space) name-len 0)))
		(if (> len max-len)
		    (concat (substring name 0 (- max-len 3)) "...")
		  name))))
    (let* ((entries (config-manager--entries this))
	   (entry-status (if (boundp 'config-entry-status)
			     config-entry-status
			   (make-hash-table :test 'equal)))
	   (name-len (get-max #'config-entry-name))
	   (col-space config-manager-list-col-space)
	   (name-len (or name-len col-space))
	   (headers (oref this list-header-fields))
	   format-meta)
      (dolist (entry entries)
	(let ((name (config-entry-name entry)))
	  (unless (gethash name entry-status)
	    (puthash name 'alive entry-status))))
      (setq format-meta (format "%%-%ds %%-%ds%%s"
				col-space (+ col-space name-len)))
      (insert (apply 'format format-meta headers)
	      "\n"
	      (apply 'format format-meta
		     (mapcar #'(lambda (arg)
				 (make-string (length arg) ?-))
			     headers))
	      "\n")
      (cl-do ((lst entries (setq lst (cdr lst)))
	      entry)
	  ((null lst))
	(setq entry (car lst))
	(let ((name (config-entry-name entry))
	      (status (cdr (assq (gethash (config-entry-name entry)
					  entry-status)
				 config-manager-status-defs))))
	  (put-text-property 0 (length name) 'mouse-face 'highlight name)
	  (insert (apply #'format format-meta
			 (append (list status name
				       (get-desc entry col-space name-len))))))
	(if (cdr lst) (insert "\n")))
      (set (make-local-variable 'config-entry-status) entry-status))))

(cl-defmethod config-manager-list-entries-buffer ((this config-manager)
						  &optional buffer-name)
  "Create a listing of buffers used for viewing, renameing, deleting, adding.
BUFFER-NAME is the name of the buffer holding the entries for the mode."
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
	(set-window-point (get-buffer-window (current-buffer)) (point))
	(set (make-local-variable 'config-manager-instance) this)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(easy-menu-define config-manage-mode-menu config-manage-mode-map
	  "Menu for Buffer Manage." config-manage-mode-menu-definition)))
    (switch-to-buffer buf)))

(cl-defmethod config-manager-read-new-name ((this config-manager)
					    &optional prompt auto-generate-p)
  "Read an entry name from user input."
  (let ((def (config-manager-entry-default-name this))
	name)
    (if auto-generate-p
	def
      (setq prompt (or prompt (capitalize (config-manager-name this))))
      (setq prompt (choice-program-default-prompt prompt def))
      (setq name (read-string prompt nil nil def))
      (if (= 0 (length name)) (setq name nil))
      name)))

(cl-defmethod initialize-instance ((this config-manager) &optional slots)
  (with-slots (pslots) this
    (setq pslots
	  (append pslots '(name entries))))
  (cl-call-next-method this slots)
  (config-manager-set-name this))

(defun config-manage-refresh-windows ()
  "Refresh config entries list buffer."
  (->> (window-list)
       (-map (lambda (win)
	       (let ((buf (window-buffer win)))
		 (with-current-buffer buf
		   (if (config-manage-mode-assert t)
		       (config-manage-mode-refresh))))))))
(add-hook 'buffer-list-update-hook #'config-manage-refresh-windows)



;;; modal

(defcustom config-manage-highlight t
  "Whether or not to hightlight buffer using `config-manager-list-entries-buffer'."
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
  `((,(format "^.\\{%d\\}\\(.*?\\)[ \t]+.*$" (1+ config-manager-list-col-space))
     1 config-manage-font-lock-name-face t)
    (,(format "^.\\{%d\\}.*?[ \t]+\\(.*\\)$" (1+ config-manager-list-col-space))
     1 config-manage-font-lock-desc-face t)
    ("^\\([- \t]+\\)$" 1 config-manage-font-lock-headers-face t))
  "Additional expressions to highlight in config manage mode.")

(defun config-manage-mode-assert (&optional no-error-p this)
  "Throw an error if not in `config-manage-mode' when NO-ERROR-P is nil.

Pattern match on THIS if it is given and this is a `config-manager-mode'."
  (let ((matchp (and (eq major-mode 'config-manage-mode)
		     (or (not this)
			 (equal this config-manager-instance)))))
   (if (and (not no-error-p) (not matchp))
       (error "Must be in `config-manage-mode' for this command or wrong manager")
     matchp)))

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
		     config-manager-list-col-space))
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
    (config-manage-mode-assert)
    (config-manager-activate this name nil 'split)))

(defun config-manage-mode-set-status (status)
  "Set the mode status to STATUS for the mode."
  (config-manage-mode-assert)
  (let ((name (config-manage-mode-name-at-point)))
    (when name
      (puthash name status config-entry-status)
      (config-manage-mode-refresh)
      (config-manage-mode-next))))

(defun config-manage-mode-refresh ()
  "Refresh the entry listing buffer."
  (interactive)
  (config-manage-mode-assert)
  (let ((line (count-lines (point-min) (point))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (config-manager-list-entries config-manager-instance)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line line)
    (beginning-of-line)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

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
    (config-entry-rename (config-manager-entry this name) new-name)
    (config-manage-mode-refresh)))

(defun config-manage-mode-new ()
  "Create a new entry."
  (interactive)
  (let* ((this config-manager-instance)
	 (name (config-manager-read-new-name this)))
    (config-manager-insert-entry this)
    (config-manage-mode-refresh)))

(define-derived-mode config-manage-mode fundamental-mode "Configuration Manager"
  "Major mode for displaying and buffer entries.
Special commands:
\\{config-manage-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(config-manage-font-lock-keywords t))
  (font-lock-mode (if config-manage-highlight 1 0))
  (set (make-local-variable 'org-window-config)
       (current-window-configuration)))

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

(provide 'config-manage)

;;; config-manage.el ends here

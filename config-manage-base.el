;;; config-manage-base.el --- configuraiton management base classes

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

;; This library contains the base classes for the configuration management
;; system.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'time-stamp)
(require 'dash)
(require 'eieio)
(require 'eieio-base)
(require 'choice-program)
(require 'config-manage-core)

(config-manage-declare-variables config-manager-instance)
(defvar config-entry-status)

;; This class is necessary since EIEIO list types can't unpersist as they produce
;; this error:
;;   eieio-persistent-validate/fix-slot-value: In save file, list of object
;;   constructors found, but no :type specified for slot displays of type nil
(defclass config-persistent (eieio-named)
  ((pslots :initarg :pslots
	   :initform nil
	   :type list))
  :method-invocation-order :c3
  :documentation "\
Super class for objects that want to persist to the file system.")

(cl-defmethod initialize-instance ((this config-persistent) &optional slots)
  
  (setq slots (plist-put slots :pslots
			 (append (plist-get slots :pslots)
				 '(object-name))))
  (cl-call-next-method this slots))

(cl-defmethod config-persistent--unimplemented ((this config-persistent) method)
  "Signal error CONFIG-MANAGE-UN-IMPLEMENTED for EIEIO METHOD."
  (with-temp-buffer
    (set-buffer (get-buffer-create "*config-manage-backtrace*"))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (backtrace)))
  (signal 'config-manage-un-implemented
	  (list method (with-temp-buffer
			 (cl-print-object this (current-buffer))))))

(cl-defmethod config-persistent-destruct ((this config-persistent))
  "Deallocate any resources when the instance falls out of use.
The EIEIO `destructor' is deprecated in 26.  However is still
used and needed in this framework.")

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

(cl-defmethod config-persistent-unpersist ((vals list) &optional obj)
  "Restore the objects state from VALS, which has a symbols
`class' and `slots'."
  (let* ((class (cdr (assq 'class vals)))
	 (slots (cdr (assq 'slots vals)))
	 (obj (if obj
		  (if (not (eq (eieio-object-class obj) class))
		      (error "Class mismatch during unpersist: %S != %S"
			     (eieio-object-class obj) class)
		    obj)
		(make-instance class))))
    (config-persistent-unpersist obj slots)
    obj))

(cl-defmethod config-persistent-reset ((this config-persistent))
  "Reset all persistable slots to initial state.
This implementation sets all slots to nil."
  (with-slots (pslots) this
    (dolist (slotsym pslots)
      (setf (slot-value this slotsym) nil))))

(cl-defmethod object-print-fields ((this config-persistent)) nil)

(cl-defmethod object-format ((this config-persistent))
  (slot-value this 'object-name))

(cl-defmethod object-print ((this config-persistent) &rest strings)
  "Return a string as a representation of the in memory instance of THIS."
  (cl-flet* ((format-obj
	      (slot)
	      (let* ((obj (slot-value this slot))
		     (objs (with-temp-buffer
			     (cl-print-object obj (current-buffer)))))
		(format "%S %s"
			slot
			(cond ((eieio-object-p obj) objs)
			      ((stringp obj) (format "'%s'" obj))
			      (obj))))))
    (let ((fields (object-print-fields this)))
      (apply #'cl-call-next-method this
      	     (concat (if fields " ")
      		     (mapconcat #'format-obj
      				fields
      				" "))
      	     strings))))


(defclass config-persistable (config-persistent)
  ((file :initarg :file
	 :initform nil
	 :type (or null string)
	 :documentation "The file to persist the state of the object."))
  :method-invocation-order :c3
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

(cl-defmethod config-persistable-load ((this config-persistable))
  "Restore the state of the persistable object from FILE."
  (with-slots (file) this
    (let ((file (expand-file-name file)))
      (if (file-exists-p file)
	  (with-temp-buffer
	    (insert-file-contents file)
	    (let ((config (read (buffer-string))))
	      (config-persistent-unpersist config this)))))))



;;; config objects
(defclass config-entry (config-persistent)
  ((description :initarg :description
		:initform "<none>"
		:reader config-entry-description
		:type string
		:documentation "\
The description of this entry, used in `config-manager-list-entries-buffer'.")
   (manager :initarg :manager
	    :initform nil
	    :protection :protected))
  :abstract true
  :method-invocation-order :c3
  :documentation "Abstract class for all configurable entries.")

(cl-defmethod config-entry-name ((this config-entry))
  (slot-value this 'object-name))

(cl-defmethod config-entry-set-name ((this config-entry) name)
  "Set the name of the entry to NAME.

NAME's is stripped of properties since it might be fontified when
generated the buffer in `config-manage-mode'."
  (let ((name (substring-no-properties name)))
    (setf (slot-value this 'object-name) name)))

(cl-defmethod config-entry-save ((this config-entry))
  "Save the current entry configuration."
  nil)

(cl-defmethod config-entry-restore ((this config-entry))
  "Restore the current entry configuration."
  nil)

(cl-defmethod config-persistent-doc ((this config-entry) level)
  "Write compiler documentation to the current buffer."
  (with-slots (description) this
    (let ((doc (-> (eieio-object-class this)
		   cl--find-class
		   cl--class-docstring)))
      (setq doc
	    (if (not doc)
		""
	      (setq doc
		    (with-temp-buffer
		      (insert doc)
		      (goto-char (point-min))
		      (while (search-forward-regexp "`\\(.+?\\)'" nil t)
			(replace-match "`\\1`"))
		      (buffer-string)))))
      (if (> level 1)
	  (insert "\n\n"))
      (if (> level 0)
	  (insert (format "%s " (make-string level ?#))))
      (insert (format "%s\n\n%s\n" description doc)))))


;; config manager

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

(defclass config-manager (config-persistent)
  ((cycle-method :initarg :cycle-method
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
  :method-invocation-order :c3
  :documentation "Manages configurations.")

(cl-defmethod initialize-instance ((this config-manager) &optional slots)
  (setq slots (plist-put slots :pslots
			 (append (plist-get slots :pslots)
				 '(object-name entries))))
  (cl-call-next-method this slots))

(cl-defmethod config-manager-name ((this config-manager))
  "Return the name of the configuration manager."
  (slot-value this 'object-name))

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
	(let ((lexical-fn
	       #'(lambda (a b)
		   (string< (config-entry-name a) (config-entry-name b)))))
	  (let ((sort-fn (cond ((eq sort-form 'lexical) lexical-fn)
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
	(config-persistent-destruct entry)
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
				    `(:object-name ,criteria))))))
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
	   (headers (slot-value this 'list-header-fields))
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

(cl-defmethod config-persistent-doc ((this config-manager) &optional level)
  "Create markdown documentation on this manager and its entries.
The buffer is set to `markdown-mode' if library is available."
  (setq level (or level 2))
  (let* ((name (capitalize (config-manager-name this)))
	 (buf (->> (format "*%s Documentation*" name)
		   get-buffer-create))
	 (doc (-> (eieio-object-class this)
		  cl--find-class
		  cl--class-docstring)))
    (with-current-buffer buf
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "%s %s\n\n%s\n" (make-string level ?#) name doc))
      (dolist (entry (config-manager--entries this nil nil 'lexical))
	(config-persistent-doc entry (1+ level)))
      (goto-char (point-min))
      (and (fboundp 'markdown-mode) (markdown-mode))
      (read-only-mode 1))
    (display-buffer buf)
    buf))



;;; mode
(defun config-manage-mode-refresh ()
  "Refresh the entry listing buffer."
  (interactive)
  (config-manage-mode-assert)
  (let ((line (count-lines (point-min) (point)))
	(pos (point)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (config-manager-list-entries config-manager-instance)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line line)))

(defun config-manage-refresh-windows ()
  "Refresh config entries list buffer."
  (->> (window-list)
       (-map (lambda (win)
	       (let ((buf (window-buffer win)))
		 (with-current-buffer buf
		   (if (config-manage-mode-assert t)
		       (config-manage-mode-refresh))))))))

(provide 'config-manage-base)

;;; config-manage-base.el ends here

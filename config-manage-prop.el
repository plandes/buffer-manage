;;; config-manage-prop.el --- Property based configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: internal maint
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "26.1"))
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

;; This adds additional first class meta data to each slot, which allows a
;; minibuffer way of configuring slots.

;;; Code:

(require 'dash)
(require 'eieio)
(require 'choice-program-complete)
(require 'config-manage-base)

(defclass config-prop (config-persistent)
  ((prop-entry :initarg :prop-entry
	       :type config-prop-entry
	       :documentation "The persistent that `owns' this property.")
   (prompt :initarg :prompt
	   :type string
	   :documentation "Used to prompt the user for input.")
   (history :initarg :history
	    :initform (gensym "config-prop-history")
	    :type symbol
	    :documentation "The history variable for the input.")
   (required :initarg :required
	     :initform nil
	     :type boolean
	     :documentation "\
Whether or not the property is needed for compilation, run, or clean")
   (input-type :initarg :input-type
	       :initform 'last
	       :type symbol
	       :documentation "One of last toggle.")
   (order :initarg :order
	  :initform 100
	  :type integer
	  :documentation "The order of importance of setting the property.")
   (transient :initarg :transient
	      :initform nil
	      :type boolean
	      :documentation "Whether or not to persist the property."))
  :method-invocation-order :c3
  :documentation "\
The meta data property of a `config-prop-entry', which persists as a slot.")

(cl-defmethod initialize-instance ((this config-prop) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (dolist (elt (list :object-name :prop-entry :prompt))
    (unless (plist-get slots elt)
      (error "Missing initarg: %S in %s" elt this)))
  (cl-call-next-method this slots)
  (set (slot-value this 'history) nil))

(cl-defmethod config-prop-name ((this config-prop))
  "Return the name of the property.
THIS is the instance."
  (slot-value this 'object-name))

(cl-defmethod config-prop-default-input ((this config-prop))
  "Return the default string value for the default when prompting user input.
THIS is the instance."
  (with-slots (history input-type) this
    (if (boundp history)
	(let ((val (symbol-value history)))
	  (cl-case input-type
	    (toggle (or (cl-second val) (cl-first val)))
	    (last (cl-first val)))))))

(cl-defmethod config-prop-prompt ((this config-prop))
  "Return the prompt to use for user input.
THIS is the instance."
  (with-slots (prompt) this
    (let ((default (config-prop-default-input this)))
      (format "%s%s: " prompt (if default (format " (%s)" default) "")))))

(cl-defmethod config-prop-read ((this config-prop))
  "Read the user input for the property.
The default reads a string using `config-prop-default' and
`config-prop-prompt' with the history slot.
THIS is the instance."
  (with-slots (history) this
    (let* ((default (config-prop-default-input this))
	   (prompt (config-prop-prompt this)))
      (read-string prompt nil history default))))

(cl-defmethod config-prop-validate ((this config-prop) val)
  "Raise an error if user input VAL is not not valid data.
THIS is the instance."
  (ignore this val))

(cl-defmethod config-persistent-reset ((this config-prop))
  "Clear any state \(ie history) from the property.
THIS is the instance"
  (with-slots (object-name history) this
    (let ((hval (symbol-value history)))
      (if hval
	  (setf hval nil)
	(message "Warning: null history value for property: %s"
		 object-name)))))

(cl-defmethod config-prop-description ((this config-prop))
  "The human readable description of this property.
THIS is the instance."
  (with-slots (object-name) this
    (with-temp-buffer
      (insert (symbol-name object-name))
      (goto-char (point-min))
      (while (search-forward "-" nil t)
	(replace-match " " t t))
      (capitalize-region (point-min) (point-max))
      (buffer-string))))

(cl-defmethod config-persistent-doc ((this config-prop) _)
  "Write the property \(meta data) documentation.
THIS is the instance."
  (let ((desc (config-prop-description this))
	(doc (->> (slot-value this 'prop-entry)
		  eieio-object-class
		  config-manage-slots
		  (assq (slot-value this 'object-name))
		  (assq 'documentation)
		  cdr)))
    (setq doc
	  (if (null doc)
	      ""
	    (with-temp-buffer
	      (insert " ")
	      (insert doc)
	      (goto-char (point-min))
	      (if (search-forward "." nil t)
		  (delete-region (point) (point-max)))
	      (goto-char (point-min))
	      (while (search-forward-regexp "'" nil t)
		(replace-match "`"))
	      (buffer-string))))
    (insert (format "  * %s:%s\n" desc doc))))


;; properties
(defclass config-boolean-prop (config-prop)
  ()
  :method-invocation-order :c3
  :documentation "A boolean property that offers quick selection.")

(cl-defmethod config-prop-read ((this config-boolean-prop))
  "Read a config property input from the user as a boolean.
THIS is the instance."
  (let ((prompt (format (slot-value this 'prompt) "? ")))
    (y-or-n-p prompt)))


(defclass config-number-prop (config-prop)
  ((number-type :initarg :number-type
		:initform 'integer
		:type symbol
		:documentation "\
The Lisp data type to expect, which is either symbols 'float or 'integer."))
  :method-invocation-order :c3
  :documentation "A numeric \(int or float) property.")

(cl-defmethod config-prop-read ((this config-number-prop))
  "Read a config property input from the user as a number.
THIS is the instance."
  (with-slots (history) this
    (let* ((default (config-prop-default-input this))
	   (prompt (config-prop-prompt this)))
      (read-number prompt default))))

(cl-defmethod config-prop-validate ((this config-number-prop) val)
  "Validate the property value VAL using THIS instance."
  (with-slots (number-type) this
    (let ((given-type (type-of val)))
      (if (not (eq number-type given-type))
	  (error "Expecting a %S type but got %S" number-type given-type)))))


(defclass config-buffer-prop (config-prop)
  ()
  :method-invocation-order :c3
  :documentation "An Emacs buffer property.")

(cl-defmethod config-prop-read ((this config-buffer-prop))
  "Read a config property input from the user as a buffer name.
THIS is the instance."
  (with-slots (history) this
    ;; rid killed buffers from history
    (setf (symbol-value history)
	  (-filter #'(lambda (buf)
		       (buffer-live-p buf))
		   (symbol-value history)))
    (let* ((default (config-prop-default-input this))
	   (prompt (config-prop-prompt this))
	   (val (read-buffer prompt (list (current-buffer) default) t)))
      (setq val (get-buffer val))
      (add-to-list history val)
      val)))

(cl-defmethod config-prop-validate ((this config-buffer-prop) val)
  "Validate the property value VAL using THIS instance."
  (ignore this)
  (if (not (get-buffer-process val))
      (error "Buffer %S has no process" val)))


(defclass config-choice-prop (config-prop)
  ((choices :initarg :choices
	    :initform nil
	    :type (or null list)
	    :documentation "\
A list of symbols or strings for the choices to prompt the user.
Either this is non-nil or :choices-fn is, but not both.")
   (choices-fn :initarg :choices-fn
	       :initform nil
	       :type (or null function)
	       :documentation "\
A function used to generate choices in places of :choices.
The function takes this class instance as the single parameter.
Either this is non-nil or :choices is, but not both.")
   (ignore-case :initarg :ignore-case
		:initform t
		:type boolean
		:documentation "\
This is always used for `completion-ignore-case'."))
  :method-invocation-order :c3
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod config-choice-prop-choices ((this config-choice-prop))
  "Read from a list of choices for THIS property instance.
This list of choices is given by the `choices' slot or generated using the
function given in slot `choices-fn'."
  (with-slots (choices choices-fn) this
    (or choices (funcall choices-fn this))))

(cl-defmethod config-prop-read ((this config-choice-prop))
  "Read a config property input from the user as a list of choices.
The list of choices is generated with `config-choice-prop-choices', then read
using `choice-program-complete'.
THIS is the instance."
  (with-slots (history ignore-case) this
    (let ((choices (config-choice-prop-choices this)))
      (if (= 1 (length choices))
	  (car choices)
	(let ((default (config-prop-default-input this))
	      (prompt (config-prop-prompt this))
	      (completion-ignore-case ignore-case))
	  (choice-program-complete prompt choices
				   nil t nil history default))))))


(defclass config-choice-description-prop (config-choice-prop)
  ()
  :method-invocation-order :c3
  :documentation "Property that prompts for a selection of a list of choices.
This is just like `config-choice-prop' but returns the input as a string.")

(cl-defmethod config-prop-read ((this config-choice-description-prop))
  "Read a config property input from the user as a list of choices.
THIS is the instance."
  (with-slots (history ignore-case) this
    (let ((choices (config-choice-prop-choices this)))
      (if (= 1 (length choices))
	  (cadr choices)
	(let ((default (config-prop-default-input this))
	      (prompt (config-prop-prompt this))
	      (completion-ignore-case ignore-case))
	  (choice-program-complete prompt choices t t nil history default))))))


(defclass config-file-prop (config-prop)
  ((validate-modes :initarg :validate-modes
		   :initform nil
		   :type list
		   :documentation "\
The major mode to use to validate/select `config-file` buffers."))
  :method-invocation-order :c3
  :documentation "Property that prompts for a file.")

(cl-defmethod config-prop-default-input ((this config-file-prop))
  "Return nil as the default input for THIS instance."
  (ignore this))

(cl-defmethod config-prop-read ((this config-file-prop))
  "Read a config property input from the user as a file.
THIS is the instance."
  (with-slots (history description) this
    (let* ((prompt (config-prop-prompt this))
	   (fname (buffer-file-name))
	   (initial (and fname (file-name-nondirectory fname))))
      (let* ((file-name-history (symbol-value history)))
	(prog1
	    (read-file-name prompt nil nil t initial)
	  (set history file-name-history))))))

(cl-defmethod config-prop-validate ((this config-file-prop) val)
  "Validate VAL as a file for THIS instance."
  (with-slots (validate-modes prop-entry) this
    (let ((description (slot-value prop-entry 'description)))
      (with-current-buffer (find-file-noselect val)
	(if (and validate-modes (not (memq major-mode validate-modes)))
	    (error (format "This doesn't look like a %s file, got mode: %S"
			   description major-mode)))))))


(defclass config-directory-prop (config-prop)
  ()
  :documentation "Directory property")

(cl-defmethod config-prop-read ((this config-directory-prop))
  "Read a directory from the user for THIS instance."
  (with-slots (history choices) this
    (let ((default (config-prop-default-input this))
	  (prompt (config-prop-prompt this)))
      (read-directory-name prompt default nil t))))


(defclass config-eval-prop (config-prop)
  ((func :initarg :func
	 :initform '(lambda (&rest) "Unimplemented")
	 :type function
	 :documentation "The function to invoke when reading the config.")))

(cl-defmethod config-prop-read ((this config-eval-prop))
  "Read a value using a function in the `func' slot from the user.
THIS is the instance."
  (with-slots (func history prop-entry) this
    (let ((default (config-prop-default-input this))
	  (prompt (config-prop-prompt this)))
      (funcall func this prop-entry default prompt history))))



(defclass config-prop-entry (config-entry)
  ((props :initarg :props
	  :initform nil
	  :documentation "The list of metadata configurations")
   (last-selection :initarg :last-selection))
  :abstract true
  :method-invocation-order :c3
  :documentation "A property based configurable `config-entry'.
All properties are added in each sub class's `initialize-instance' method as
the :props plist argument in SLOTS.

Important: Extend from this class _last_ so that it captures all proprties
since this class sets :pslots in the `config-persistent' subclass.")

(cl-defmethod initialize-instance ((this config-prop-entry) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (let* ((props (plist-get slots :props))
	 (pslots (-map #'(lambda (prop)
			   (config-prop-name prop))
		       props)))
    (setq slots (plist-put slots :last-selection
			   (config-choice-prop :object-name 'last-selection
					       :prompt "Property"
					       :prop-entry this
					       :choices pslots
					       :input-type 'last))
	  slots (plist-put slots
			   :pslots (append (plist-get slots :pslots) pslots))
	  slots (plist-put slots :props props)))
  (cl-call-next-method this slots))

(cl-defmethod config-persistent-persist-slots ((this config-prop-entry))
  "Return all non-transient slots of THIS that should be persisted."
  (with-slots (pslots) this
    (->> pslots
	 (cl-remove-if #'(lambda (slot-name)
			   (let ((prop (config-prop-by-name this slot-name)))
			     (and prop (slot-value prop 'transient)))))
	 (-map (lambda (slot)
		 (let ((val (->> (slot-value this slot)
				 (config-persistent-persist-value this))))
		   (cons slot val)))))))

(cl-defmethod config-prop-save-config ((this config-prop-entry))
  "Tell the compiler manager to persist the configuration of all compilers.
THIS is the instance."
  (with-slots (manager) this
    (unless manager
      (error "No manager set in compiler: %S"
	     (with-temp-buffer
	       (cl-print-object this (current-buffer)))))
    (config-persistable-save manager)))

(cl-defmethod config-prop-set ((this config-prop-entry) prop val)
  "Set a property with name \(symbol) PROP to VAL.
THIS is the instance."
  (config-prop-validate prop val)
  (setf (slot-value this (config-prop-name prop)) val)
  (config-prop-save-config this)
  (message "Set %S to %s" (config-prop-name prop)
	   (if (stringp val)
	       val
	     (prin1-to-string val))))

(cl-defmethod config-prop-by-order ((this config-prop-entry))
  "Get all properties sorted by their order values.
THIS is the instance."
  (with-slots (props) this
    (setq props (sort props #'(lambda (a b)
				(< (slot-value a 'order)
				   (slot-value b 'order)))))
    props))

(cl-defmethod config-prop-by-name ((this config-prop-entry) name)
  "Get a property by \(symbol) NAME.
THIS is the instance."
  (with-slots (props) this
    (let ((prop-map (mapcar #'(lambda (prop)
				`(,(config-prop-name prop) . ,prop))
			    props)))
      (cdr (assq name prop-map)))))

(cl-defmethod config-prop-entry-configure ((this config-prop-entry)
					   config-options)
  "Configure the prop-entry.

CONFIG-OPTIONS informs how to configure the prop-entry.  It is one of:
  - numeric argument (if any) passed in the iteractive mode with
    \\[universal-argument].
  - Either nil or the symbol 'immediate, which prompts for the property to set
    and then prompts and sets the property itself.
  - Form (prop-name <property to set>) prompts the specific property and value.
  - Form (prop-name <property to set> <value>) sets the specified
    property to the value.
THIS is the instance."
  (let (prop val)
    (cond ((or (null config-options) (eq config-options 'immediate))
	   (with-slots (props last-selection) this
	     (let ((prop-name (config-prop-read last-selection)))
	       (setq prop (config-prop-by-name this prop-name)))))
	  ((consp config-options)
	   (cl-case (car config-options)
	     (prop-name
	      (setq prop (config-prop-by-name
			  this (cl-second config-options))))
	     (t (error "Unknown type: %S" (car config-options))))
	   (if (> (length config-options) 2)
	       (setq val (nth 2 config-options))))
	  (t (let ((props (config-prop-by-order this)))
	       (setq prop (nth (min config-options (length props)) props)))))
    (setq val (or val (config-prop-read prop)))
    (config-prop-set this prop val)))

(cl-defmethod config-prop-entry-set-required ((this config-prop-entry))
  "Set all required properties for the prop-entry.
THIS is the instance."
  (dolist (prop (config-prop-by-order this))
    (let* ((name (config-prop-name prop))
	   (val (slot-value this name))
	   ;; minibuffer reading has odd behavior when this isn't nil
	   (display-buffer-alist nil))
      (when (and (null val) (slot-value prop 'required))
	(setq val (config-prop-read prop))
	(config-prop-set this prop val)))))

(cl-defmethod config-persistent-reset ((this config-prop-entry))
  "Wipe all values for the prop-entry.
THIS is the instance."
  (dolist (prop (config-prop-by-order this))
    (config-prop-set this prop nil))
  (dolist (prop (config-prop-by-order this))
    (config-persistent-reset prop))
  (message "Cleared %s configuration" (config-entry-name this)))

(cl-defmethod config-persistent-doc ((this config-prop-entry) level)
  "Return the prop-entry level documentation.
See the :prop-entry-doc slot.
LEVEL is the depth of this recursive call.
THIS is the instance."
  (cl-call-next-method this level)
  (let ((props (config-prop-by-order this)))
    (when props
      (insert (format "\nProperties:\n"))
      (dolist (prop props)
	(config-persistent-doc prop (1+ level))))))

(cl-defmethod config-prop-entry-info ((this config-prop-entry))
  "Create and display a buffer with the `prop-entry' documentation and config.
THIS is the instance."
  (with-current-buffer (->> (config-entry-name this)
			    capitalize
			    (format "%s Info")
			    get-buffer-create)
    (read-only-mode 0)
    (erase-buffer)
    (config-persistent-doc this 1)
    (when (child-of-class-p (eieio-object-class this)
			    'config-prop-entry)
      (insert "\n")
      (config-prop-entry-write-configuration this 1 "Configuration:")
      (and (fboundp 'markdown-mode) (markdown-mode)))
    (read-only-mode 1)
    (display-buffer (current-buffer))))

(cl-defmethod config-prop-entry-write-configuration ((this config-prop-entry)
						     &optional level header)
  "Write a human readable documentation string of THIS property.
This is written in to the current buffer.
LEVEL is the indentation level.
HEADER is a string written to describe the property, otherise the description
is used."
  (setq level (or level 0))
  (with-slots (description) this
    (insert (or header (format "%s configuration:" description)))
    (newline)
    (dolist (prop (config-prop-by-order this))
      (let* ((name (config-prop-name prop))
	     (val (or (slot-value this name) "*not set*"))
	     (space (make-string (* 2 level) ? )))
	(if (> level 0)
	    (setq space (concat space "* ")))
	(insert (format "%s%S: %s\n" space name val))))))

(cl-defmethod config-prop-entry-show-configuration ((this config-prop-entry))
  "Create a buffer with the configuration of the prop-entry of THIS."
  (with-slots (description) this
    (with-current-buffer
	(-> (format "*%s Configuration*" description)
	    get-buffer-create)
      (read-only-mode 0)
      (erase-buffer)
      (config-prop-entry-write-configuration this)
      (read-only-mode 1)
      (display-buffer (current-buffer))
      (current-buffer))))

(provide 'config-manage-prop)

;;; config-manage-prop.el ends here

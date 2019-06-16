;;; config-manage-prop.el --- property based configuration

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
	  :documentation "The order of importance of setting the property."))
  :documentation "\
The meta data property of a `config-prop-entry', which persists as a slot.")

(cl-defmethod initialize-instance ((this config-prop) &optional args)
  (dolist (elt (list :object-name :prop-entry :prompt))
    (unless (plist-get args elt)
      (error "Missing initarg: %S in %s" elt this)))
  (cl-call-next-method this args)
  (set (slot-value this 'history) nil))

(cl-defmethod object-print ((this config-prop) &rest strings)
  (with-slots (object-name order) this
    (apply #'cl-call-next-method this
	   (format ": %s (%d)" object-name order)
	   strings)))

(cl-defmethod config-prop-default-input ((this config-prop))
  "Return the default string value for the default when prompting user input."
  (with-slots (history input-type) this
    (if (boundp history)
	(let ((val (symbol-value history)))
	  (cl-case input-type
	    (toggle (or (cl-second val) (cl-first val)))
	    (last (cl-first val)))))))

(cl-defmethod config-prop-prompt ((this config-prop))
  "Return the prompt to use for user input."
  (with-slots (prompt) this
    (let ((default (config-prop-default-input this)))
      (format "%s%s: " prompt (if default (format " (%s)" default) "")))))

(cl-defmethod config-prop-read ((this config-prop))
  "Read the user input for the property.
The default reads a string using `config-prop-default' and
`config-prop-prompt' with the history slot."
  (with-slots (history) this
    (let* ((default (config-prop-default-input this))
	   (prompt (config-prop-prompt this)))
      (read-string prompt nil history default))))

(cl-defmethod config-prop-validate ((this config-prop) val)
  "Raise an error if user input VAL is not not valid data."
  nil)

(cl-defmethod config-persistent-reset ((this config-prop))
  "Clear any state \(i.e. history) from the property."
  (with-slots (history) this
    (setq (symbol-value history) nil)))

(cl-defmethod config-prop-description ((this config-prop))
  "The human readable description of this property."
  (with-slots (object-name) this
    (with-temp-buffer
      (insert (symbol-name object-name))
      (goto-char (point-min))
      (while (search-forward "-" nil t)
	(replace-match " " t t))
      (capitalize-region (point-min) (point-max))
      (buffer-string))))

(cl-defmethod config-persistent-doc ((this config-prop) level)
  "Write the property \(meta data) documentation."
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
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod config-choice-prop-choices ((this config-choice-prop))
  (with-slots (choices choices-fn) this
    (or choices (funcall choices-fn this))))

(cl-defmethod config-prop-read ((this config-choice-prop))
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
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod config-prop-read ((this config-choice-description-prop))
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
The major mode to use to validate/select `config-file` buffers.")))

(cl-defmethod initialize-instance ((this config-file-prop) &optional args)
  (dolist (elt (list :validate-modes))
    (unless (plist-get args elt)
      (error "Missing initarg: %S" elt)))
  (cl-call-next-method this args))

(cl-defmethod config-prop-default-input ((this config-file-prop))
  nil)

(cl-defmethod config-prop-read ((this config-file-prop))
  (with-slots (history description) this
    (let* ((prompt (config-prop-prompt this))
	   (fname (buffer-file-name))
	   (initial (and fname (file-name-nondirectory fname))))
      (let* ((file-name-history (symbol-value history)))
	(prog1
	    (read-file-name prompt nil nil t initial)
	  (set history file-name-history))))))

(cl-defmethod config-prop-validate ((this config-file-prop) val)
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
  "Read a directory from the user."
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
the :props plist argument in ARGS.

Important: Extend from this class _last_ so that it captures all proprties
since this class sets :pslots in the `config-persistent' subclass.")

(cl-defmethod initialize-instance ((this config-prop-entry) &optional args)
  (let* ((props (plist-get args :props))
	 (choices (mapcar #'(lambda (prop)
			      (slot-value prop 'object-name))
			  props)))
    (setq args (plist-put args :last-selection
			  (config-choice-prop :object-name 'last-selection
					      :prompt "Property"
					      :prop-entry this
					      :choices choices
					      :input-type 'last))
	  args (plist-put args :pslots
			  (append (plist-get args :pslots) choices))
	  args (plist-put args :props props)))
  (cl-call-next-method this args))

(cl-defmethod object-print ((this config-prop-entry) &rest strings)
  (apply #'cl-call-next-method this
	 (concat ", props: ("
		 (mapconcat #'(lambda (obj)
				(with-temp-buffer
				  (cl-print-object obj (current-buffer))))
			    (slot-value this 'props)
			    ", ")
		 ")")
	 strings))

(cl-defmethod config-prop-save-config ((this config-prop-entry))
  "Tell the compiler manager to persist the configuration of all compilers."
  (with-slots (manager) this
    (unless manager
      (error "No manager set in compiler: %S"
	     (with-temp-buffer
	       (cl-print-object this (current-buffer)))))
    (config-persistable-save manager)))

(cl-defmethod config-prop-set-prop ((this config-prop-entry) prop val)
  "Set a property with name \(symbol) PROP to VAL."
  (config-prop-validate prop val)
  (setf (slot-value this (slot-value prop 'object-name)) val)
  (config-prop-save-config this)
  (message "Set %S to %s" (slot-value prop 'object-name)
	   (if (stringp val)
	       val
	     (prin1-to-string val))))

(cl-defmethod config-prop-by-order ((this config-prop-entry))
  "Get all properties sorted by their order values."
  (with-slots (props) this
    (setq props (sort props #'(lambda (a b)
				(< (slot-value a 'order)
				   (slot-value b 'order)))))
    props))

(cl-defmethod config-prop-by-name ((this config-prop-entry) name)
  "Get a property by \(symbol) NAME."
  (with-slots (props) this
    (let ((prop-map (mapcar #'(lambda (prop)
				`(,(slot-value prop 'object-name) . ,prop))
			    props)))
      (cdr (assq name prop-map)))))

(cl-defmethod config-prop-entry-configure ((this config-prop-entry)
					   config-options)
  "Configure the prop-entry.

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument]."
  (let (prop val)
    (cond ((null config-options)
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
    (config-prop-set-prop this prop val)))

(cl-defmethod config-prop-entry-set-required ((this config-prop-entry))
  "Set all required properties for the prop-entry."
  (dolist (prop (config-prop-by-order this))
    (let* ((name (slot-value prop 'object-name))
	   (val (slot-value this name))
	   ;; minibuffer reading has odd behavior when this isn't nil
	   (display-buffer-alist nil))
      (when (and (null val) (slot-value prop 'required))
	(setq val (config-prop-read prop))
	(config-prop-set-prop this prop val)))))

(cl-defmethod config-persistent-reset ((this config-prop-entry))
  "Wipe all values for the prop-entry."
  (dolist (prop (config-prop-by-order this))
    (config-prop-set-prop this prop nil))
  (dolist (prop (config-prop-by-order this))
    (config-persistent-reset prop))
  (message "Cleared %s configuration" (config-entry-name this)))

(cl-defmethod config-persistent-doc ((this config-prop-entry) level)
  "Return the prop-entry level documentation.
See the :prop-entry-doc slot."
  (cl-call-next-method this level)
  (let ((props (config-prop-by-order this)))
    (when props
      (insert (format "\nProperties:\n"))
      (dolist (prop props)
	(config-persistent-doc prop (1+ level))))))

(cl-defmethod config-prop-entry-show-configuration ((this config-prop-entry))
  "Create a buffer with the configuration of the prop-entry."
  (with-slots (description) this
    (with-current-buffer
	(-> (format "*%s Configuration*" description)
	    get-buffer-create)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "%s configuration:\n" description))
      (dolist (prop (config-prop-by-order this))
	(let* ((name (slot-value prop 'object-name))
	       (val (or (slot-value this name) "<not set>")))
	  (insert (format "%S: %s\n" name val))))
      (read-only-mode 1)
      (display-buffer (current-buffer))
      (current-buffer))))

(provide 'config-manage-prop)

;;; config-manage-prop.el ends here

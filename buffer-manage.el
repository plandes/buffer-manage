;;; buffer-manage.el --- Manage buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2020 Paul Landes

;; Version: 0.12
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: internal maint
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "26.1") (choice-program "0.13") (dash "2.17.0"))

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

;; Provides support to manage buffers of any kind.  This is helpful for using
;; multiple inferior shells, multiple SQL session buffers or any other piped
;; process that requires multiple buffers.

;; The library includes support for:

;; * A major mode and buffer for listing, switching, and organizing multiple
;;   Emacs buffers.
;; * Fast switching with customized key bindings through the customize framework.
;; * Switch between managers providing the same key bindings for buffer entries
;;   with the same key bindings for creation, switching and managing.
;; * Create your own trivial implementations in minimal set of Emacs Lisp code.
;; * Interact with buffer entries and manager as objects with a straight forward
;;   API.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'derived)
(require 'choice-program-complete)
(require 'config-manage)

;; buffer local vars to silence compiler
(defvar buffer-manager-instance)
(defvar buffer-entry-instance)

(defgroup buffer-manage nil
  "Buffer management and tracking"
  :group 'buffer-manage
  :prefix "buffer-manage-")

(defvar buffer-manage-current-buffer (current-buffer)
  "The current buffer used by `buffer-manage-post-command-hook'.
This variable is used to keep track of switching/entering the next buffer
entry.")



;;; key bindings
(defcustom buffer-manage-key-bindings
  '((nil (("switch" nil "C-x C-h")
	  ("list" nil "C-<tab>")
	  ("new" nil "M-C-<tab>")
	  ("toggle-cycle-method" nil "C-x C-'"))))
  "Specifies key bindings for buffer manager instance\(s) functions.
See `buffer-manager-bind-functions'."
  :group 'buffer-manage
  :type '(repeat
	  (list :tag "Key Bindings"
		(choice :tag "Applies To"
			(const :tag "All Managers" nil)
			(string :tag "Manager Name"))
		(repeat :tag "Manager Key Binding"
			(list :tag "Binding Entry"
			      (choice :tag "Action"
				      (const :tag "New Entry" "new")
				      (const :tag "List Entries" "list")
				      (const :tag "Switch To Entry" "switch")
				      (const :tag "Toggle Cycle Method" "toggle-cycle-method")
				      (string :tag "Other"))
			      (choice :tag "Keymap"
				      (const :tag "Global Keymap" nil)
				      (symbol :tag "Specific"))
			      (string :tag "Binding"))))))

(defvar buffer-manage-remap-instances nil
  "List of all instances that are eligible for remapping of keybindings.
This enables keybinding switches for various instances of managers to their
respect generated functions \(see `buffer-manager-interactive-functions').")

(defvar buffer-manage-on-mouse-down nil
  "Name of buffer entry for `buffer-manage-mode-mouse-*' functions.")



;;; buffer-entry class
(defclass buffer-entry (config-entry)
  ((buffer :initarg :buffer
	   :reader buffer-entry-buffer
	   :protection :private
	   :documentation
	   "Contains the emacs buffer object.")
   (kill-frame-p :initarg :kill-frame-p
		 :initform nil
		 :writer buffer-entry-set-kill-frame-p
		 :type boolean
		 :protection :protected
		 :documentation
		 "Whether or not to kill the containing frame when the
process dies.")
   (sentinel :initarg :sentinel
	     :initform nil
	     :type (or null function)
	     :protection :protected))
  :abstract true
  :method-invocation-order :c3
  :documentation "Abstract class for all buffer entry like objects.")

(cl-defmethod initialize-instance ((this buffer-entry) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (cl-call-next-method this slots)
  (let ((win-cfg (current-window-configuration)))
    (with-slots (sentinel manager) this
      (unwind-protect
	  (let ((new-buf (buffer-entry-create-buffer this)))
	    (setf (slot-value this 'buffer) new-buf)
	    (with-current-buffer new-buf
	      (set (make-local-variable 'buffer-entry-instance) this)
	      (add-hook 'post-command-hook
			'buffer-manage-post-command-hook nil t)
	      (when sentinel
		(set-process-sentinel
		 (get-buffer-process new-buf)
		 `(lambda (process msg)
		    (funcall (quote ,sentinel) process
			     msg ,this ,manager))))))
	(set-window-configuration win-cfg)))))

(cl-defmethod config-persistent-destruct ((this buffer-entry))
  "Dispose a `buffer-entry' instance by killing THIS's process.
Don't use this instance after this method is called.  Instead, don't reference
it and let the garbage collector get it."
  (when (buffer-entry-live-p this)
    (with-slots (buffer) this
     (when (get-buffer-process buffer)
       (buffer-entry-insert this "exit" t)
       (sit-for 0.5)))
    (if (slot-value this 'kill-frame-p)
	(delete-frame (window-frame (selected-window))))
    (kill-buffer (buffer-entry-buffer this))))

(cl-defmethod config-entry-description ((this buffer-entry))
  "Get the description of THIS configuration entry."
  (with-slots (buffer) this
    (with-current-buffer buffer
      (if (string-match abbreviated-home-dir default-directory)
	  (->> (match-string 0 default-directory)
	       length
	       (substring default-directory)
	       (concat "~/"))
	default-directory))))

(cl-defmethod config-entry-set-name ((this buffer-entry) name)
  "Rename the buffer entry to NAME for THIS and return the new entry.
Note that NAME is not buffer name syntax, it is the name of the
entry."
  (buffer-entry-live-p this t)
  (cl-call-next-method this name)
  (with-current-buffer (buffer-entry-buffer this)
    (rename-buffer name t)))

(cl-defmethod buffer-entry-create-buffer ((this buffer-entry))
  "Create the buffer for THIS entry.
This is a factory method that is called once by the constructor of the object."
  (error "Must override `buffer-entry-create-buffer' for class `%S'"
	 (eieio-object-class this)))

(cl-defmethod buffer-entry-live-p ((this buffer-entry) &optional error-p)
  "Return non-nil if THIS entry is still active.
Otherwise it is useless and the garbage collector should clean
this instance up.

If ERROR-P is non-nil, raise an error stating that the buffer is no longer
valid.  This is useful for doing a check and getting the buffer at the same
time."
  (let ((live-p (buffer-live-p (buffer-entry-buffer this))))
    (if (and error-p (not (buffer-entry-live-p this)))
	(error "Buffer isn't valid"))
    live-p))


(defclass buffer-manager (config-manager)
  ((start-dir :initarg :start-dir
	      :initform "~/"
	      ;; customized vars need public access
	      ;;:protection :protected
	      :reader buffer-manager-start-dir
	      :type (or null string)
	      :label "Start directory of the entry process"
	      :custom (choice
		       (const :tag "Default directory" nil)
		       (directory :tag "Specific Directory"))
	      :documentation
	      "Used for `default-directory' for new entries.
If `nil', the current value of `default directory' is used.")
   (include-frame-wins :initarg :include-frame-wins
		       :initform t
		       :reader buffer-manager-include-frame-wins
		       :writer buffer-manager-set-include-frame-wins
		       :type boolean
		       :custom boolean
		       :documentation "\
When non-nil, don't necessarily got to another entry in the frame just because
it's visible.  One way this happens is to chose the next desired entry based on
`cycle-method' regardeless of the last visited entry or what entries are in the
current frame.")
   (read-history :initform (gensym "buffer-manage-read-history")
		 :type symbol
		 :protection :private
		 :documentation "\
Used for history when reading user input when switching to other buffers."))
  :abstract true
  :method-invocation-order :c3
  :documentation "Manages buffer entries.")

(cl-defmethod initialize-instance ((this buffer-manager) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :list-header-fields
			 '("C" "Name" "Working Directory")))
  (cl-call-next-method this slots)
  (set (slot-value this 'read-history) nil))

(cl-defmethod config-persistent-destruct ((this buffer-manager))
  "Dispose by disping all buffer entries of THIS buffer manager.
Don't use this instance after this method is called.  Instead, don't reference
it and let the garbage collector get it."
  (with-slots (entries) this
    (dolist (entry entries)
      (config-persistent-destruct entry))
    (let (new-entries)
      (dolist (entry entries)
	(if (buffer-entry-live-p entry)
	    (setq new-entries (append new-entries (cons entry nil)))))
      (setq entries new-entries))))

(cl-defmethod config-manager-insert-entry ((this buffer-manager) &optional
					   name start-dir new-frame-p switchp)
  "Create a new entry instance and return its name.
If NAME is non-nil, use it as the name of the buffer entry,
otherwise, create a use a auto generated name.

START-DIR is the initial directory to read from the user input that will be
  used for the `default-directory' of the new `buffer-entry' instance.

NEW-FRAME-P, if non-nil, creates a new frame for the entry, and kills that
  frame when it is destroyed.

SWITCHP, if non-nil, switch to the new entry after created with
  `buffer-manager-switch'.

THIS is the object instance."
  (let* ((default-directory (file-name-as-directory
			     (or start-dir
				 (buffer-manager-start-dir this)
				 default-directory)))
	 (entry (config-manager-add-entry
		 this
		 `(:sentinel buffer-manager-process-sentinel
			     :manager ,this
			     :kill-frame-p ,new-frame-p
			     :object-name ,name))))
    (with-current-buffer (buffer-entry-buffer entry)
      (rename-buffer (config-entry-name entry) nil)
      (set (make-local-variable 'buffer-manager-instance) this))
    (message "Created %s" (config-manager-name this))
    (if switchp
	(buffer-manager-switch this entry new-frame-p))
    entry))

(cl-defmethod buffer-manager-display-entries ((this buffer-manager)
					      &optional include-fn exclude-fn
					      other-buffers sort-form)
  "Display buffers from an entries for THIS buffer manager.

The buffers selected from entries are those that matches INCLUDE-FN and don't
match EXCLUDE-FN.  These buffers are tiled across the current frame.

Both INCLUDE-FN and EXCLUDE-FN take an instance of `config-entry' as a
singleton parameter.

OTHER-BUFFERS is a list of other buffers to consider for displaying.

Sorting on the returned entries are done when SORT-FORM is non-nil.  Any
sorting is only done on the returned set of entries and doesn't change any
of the object's internal state.  Sorting is done based on SORT-FORM's value:
 - symbol 'lexical: sort lexically based on the config entry's name
 - function: sort using SORT-FORM as a predicate \(see `sort').

See `config-manager--entries'."
  (let* ((entries (config-manager--entries this include-fn exclude-fn sort-form))
	 (bufs (append other-buffers (mapcar #'buffer-entry-buffer entries))))
    (when bufs
      (let* ((wins (length bufs))
	     (win-height (/ (window-body-height) wins)))
	(delete-other-windows)
	(switch-to-buffer (car bufs))
	(dolist (buf (cdr bufs))
	  (split-window-vertically win-height)
	  (other-window 1)
	  (switch-to-buffer buf))
	(other-window 1)))))

(cl-defmethod config-manager-current-instance ((this buffer-manager)
					       &optional assertp)
  "Get the current buffer entry instance in the current buffer.
Return nil if this isn't an entry buffer that belongs to this
`buffer-manager' instance.

ASSERTP, if non-nil, raise an error if there is no current entry.
THIS is the object instance."
  (with-slots (entries) this
    (if (and (boundp 'buffer-entry-instance)
	     (member buffer-entry-instance entries))
	buffer-entry-instance
      (when assertp
	(error "Missing buffer entry or wrong buffer")))))

(cl-defmethod config-manager-entry-cycle ((this buffer-manager))
  "Override to provide better support for buffers across multi-window frames.
THIS is the object instance."
  (cl-flet* ((current-fn
	      (entry)
	      (eq (config-manager-current-instance this) entry)))
    (with-slots (last-switched-to include-frame-wins cycle-method) this
      (let ((entries (config-manager--entries this))
	    ;; get all entries displayed in windows on this frame except the
	    ;; current entry we're in
	    (win-entries (buffer-manager-window-entries this #'current-fn))
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
		  (not cur-entry)
		  ;; make sure this isn't a stale entry that's been killed
		  (buffer-live-p (buffer-entry-buffer last-switched-to))
		  ;; when other windows in the frame aren't candidates, the
		  ;; last visited entry isn't a candidate either
		  (or include-frame-wins
		      (not (memq last-switched-to win-entries))))
	     last-switched-to)
	 (let ((bak-entries (copy-tree entries)))
	   (when (not include-frame-wins)
	     ;; get rid of current frame window entries when the user doesn't
	     ;; want them; this shrinks the candidate pool to all but what's on
	     ;; the frame
	     (setq entries
		   (remove nil
			   (mapcar (lambda (arg)
				     (unless (memq arg win-entries) arg))
				   entries)))
	     ;; get rid of `win-entries' now that we've used it to set diff
	     (setq win-entries nil)
	     ;; revert to the set of entries if narrowed to tight
	     (if (or (null entries)
		     (and (= 1 (length entries))
			  (eq (car entries) cur-entry)))
		 (setq entries bak-entries)))
	   (or
	    (cl-case method
	      (last-visit (or
			   ;; win-entries is nil if no entries
			   (car win-entries)
			   ;; last visited will be second in the queu
			   (cl-second entries)))
	      (next (or
		     ;; next is also the second, when we cycle, it's put at the
		     ;; back after the switch
		     (cl-second entries)))
	      (otherwise (error "Unimplemented (but value) cycle method: %S"
				method)))
	    (and (not include-frame-wins) (car entries)))))))))

(cl-defmethod buffer-manager-cleanup ((this buffer-manager))
  "Remove all dead buffer entries.
THIS is the object instance."
  (condition-case err
      (with-slots (entries) this
	(setq entries
	      (remove nil
		      (mapcar (lambda (entry)
				(if (buffer-entry-live-p entry) entry))
			      entries))))
    (error (with-current-buffer
	       (get-buffer-create "*Manage Buffer Errors*")
	     (goto-char (point-max))
	     (insert (format "In `buffer-manager-cleanup': %S\n" err))
	     (display-buffer (current-buffer))))))

(defun buffer-manager-process-sentinel (process msg entry manager)
  "Sentinal call back.
PROCESS the that finished.
MSG the message that resulted from the process.
ENTRY is the `buffer-entry' instance.
MANAGER the `buffer-manager' singleton instance."
  (ignore process)
  (if (or (string= msg "finished\n")
	  (save-match-data (string-match "^exited" msg)))
      (config-manager-remove-entry manager entry)))

(defun buffer-manager-kill-buffer-callback ()
  "Called by `kill-buffer-hook'."
  (when (and (boundp 'buffer-manager-instance)
	     ;; some buffers use managers and might not be entries
	     (boundp 'buffer-entry-instance))
    (let ((this buffer-manager-instance))
      (config-manager-remove-entry this buffer-entry-instance)
      (buffer-manager-cleanup this))))
(add-hook 'kill-buffer-hook 'buffer-manager-kill-buffer-callback)

(cl-defmethod buffer-manager-window-entries ((this buffer-manager)
					     &optional exclude-fn)
  "Return `buffer-entry' instances contained in windows for this frame.

The entries selected are those that don't match EXCLUDE-FN.  These buffers are
tiled across the current frame.  The EXCLUDE-FN function takes an instance of
`config-entry' as a singleton parameter.

THIS is the object instance.

See `config-manager--entries'."
  (config-manager--entries this
			  (lambda (entry)
			    (cl-block inc-fn
			     (dolist (win (window-list))
			       (with-current-buffer (window-buffer win)
				 (if (and (boundp 'buffer-entry-instance)
					  (eq buffer-entry-instance entry))
				     (cl-return-from inc-fn t))))))
			  exclude-fn))

(defun buffer-manage-post-command-hook ()
  "Switch/enter next buffer entry."
  (let ((curr-buff (current-buffer))
	(old-buff buffer-manage-current-buffer))
    (setq buffer-manage-current-buffer curr-buff)
    (unless (equal curr-buff old-buff)
      (if (and (boundp 'buffer-manager-instance)
	       (boundp 'buffer-entry-instance))
	  (buffer-manager-enter-buffer buffer-manager-instance
				       buffer-entry-instance)))))

(cl-defmethod buffer-manager-enter-buffer ((this buffer-manager) entry)
  "Book keeping to to reorder entries to palce ENTRY after cycling.
THIS is the object instance."
  (with-slots (last-switched-to) this
    (setq last-switched-to entry)
    (config-manager-cycle-entries this entry)))

(cl-defmethod buffer-entry-insert ((this buffer-entry) command
				   &optional send-command-p
				   no-window-set-point-p)
  "Add COMMAND to the buffer prompt.
If the buffer doesn't have the point at the prompt, then create an error.

SEND-COMMAND-P, if non-nil, actually execute the command inserted as if the
  user hit ENTER.

NO-WINDOW-SET-POINT-P, if non-nil don't reset the window point.

THIS is the object instance."
  (let ((buf (buffer-entry-buffer this)))
    (save-match-data
      (with-current-buffer buf
	(goto-char (point-max))
	(insert command)
	(if (and (fboundp 'comint-send-input) send-command-p)
	    (comint-send-input))
	(goto-char (point-max))
	(unless no-window-set-point-p
	  (set-window-point (get-buffer-window buf) (point-max)))))))

(cl-defmethod config-manager-activate ((this buffer-manager) criteria)
  "Switch to a `buffer-entry' in THIS manager.

CRITERIA, see the `config-manager' method `config-manager-activate'."
  (cl-call-next-method this criteria)
  (buffer-manager-switch this 'first))

(cl-defmethod buffer-manager-switch ((this buffer-manager) criteria
				     &optional new-frame-p window-cfg)
  "Switch to a buffer entry.

If the buffer CRITERIA is the name of the buffer to switch to, go to that
buffer, otherwise, create a new one with that name and switch to it.
Returns the buffer entry we switched to based on CRITERIA \(see
`buffer-manager-entry').

NEW-FRAME-P, if non-nil, create a new frame and switch to the new buffer in
  it.

WINDOW-CFG, if non-nil, split the window based on the value, which is
  currently just the symbol `split'.

THIS is the object instance."
  (let ((entry (or (config-manager-entry this criteria)
		   (config-manager-insert-entry this))))
    (if new-frame-p
	(save-window-excursion
	  (select-window (frame-first-window (make-frame-command)))
	  (buffer-manager-switch this entry))
     (let ((win-entries (buffer-manager-window-entries this))
	   (buf (buffer-entry-buffer entry)))
       ;; if entry we're going to is already in the frame, switch over to that
       ;; window in the current frame instead of duplicating windows
       (if (member entry win-entries)
	   (select-window (get-buffer-window buf))
	 (if (eq 'split window-cfg)
	     (save-excursion (display-buffer buf))
	   (switch-to-buffer buf)))))
    entry))

(cl-defmethod buffer-manager-read-name ((this buffer-manager)
					&optional prompt require-match
					default name-fn)
  "Read a buffer entry name from the user.

PROMPT is given to the user, or defaults to the `config-manager-name'.  Don't
  add any trainling `: ' type characters as `default' syntax is added.

REQUIRE-MATCH, if non-nil don't allow the user to produce a name that doesn't
  already exist as a entry name for this manager.

DEFAULT, if non-nil use as the default instead of next buffer entry name.
  Otherwise the default is selected an entry that isn't currently in any window
  in the current frame.

NAME-FN, if non-nil use to create a name when prompting the user for each
  buffer.  The default is `config-entry-name'.

THIS is the object instance."
  (setq name-fn (or name-fn 'config-entry-name))
  (let ((entries (config-manager--entries this))
	def-entry def name-map)
    (with-slots (include-frame-wins) this
      (let ((old-include-frame-wins include-frame-wins))
	;; it isn't useful to use a default based on the entry chosen by the
	;; normal cycle method; instead toggle to change what entries are
	;; visible based on `include-frame-wins'
	;;
	;; `let' forms no longer (starting in 27) have an effect on slot values
	;; even using `with-slots', so we must rebind the value, then set it
	;; back
	(unwind-protect
	    (progn
	      (setq include-frame-wins (not include-frame-wins))
	      (setq def-entry (cond ((= 1 (length entries)) (car entries))
				    (t (config-manager-entry this 'cycle)))
		    def (or default (if def-entry (funcall name-fn def-entry)))))
	  (setq include-frame-wins old-include-frame-wins))))
    (setq prompt (or prompt (capitalize (config-manager-name this))))
    (setq prompt (choice-program-complete-default-prompt prompt def))
    (setq name-map (mapcar (lambda (entry)
			     (cons (funcall name-fn entry)
				   (config-entry-name entry)))
			   entries))
    (with-slots (read-history) this
      (let ((input (completing-read prompt (mapcar name-fn entries)
				    nil require-match nil read-history def)))
	(if (= 0 (length input))
	    (setq input nil)
	  ;; keep non-matching name for new shell names, rename shell etc.
	  (setq input (or (cdr (assoc input name-map))
			  (unless require-match input))))
	input))))

(cl-defmethod buffer-manager-interactive-functions ((this buffer-manager)
						    singleton-variable-sym)
  "Create, evaluate and compile new Emacs Lisp user functions.
This is called by `buffer-manager-create-interactive-functions' to creates user
interactive functions as an entry point and access to the THIS instance's
functionality.

SINGLETON-VARIABLE-SYM is an interned symbol that is bound to the singleton
  class instance that points to THIS.

See `buffer-manager-create-interactive-functions'."
  (let ((cname (config-manager-entry-default-name this)))
    `(("new" (defun ,(intern (format "%s-new" cname))
		 (&optional name start-dir new-frame-p)
	       ,(format "Create a new %s entry.
When invoked with \\[universal-argument] a new NAME for the %s is prompted
to the user.  If the name of the %s doesn't yet exist, it will be created.

START-DIR, if non-nil, use this directory for the buffer
  process to start in instead of the `buffer-manager' instance's
  `start-dir' slot value.

NEW-FRAME-P, if non-nil, create a new frame and switch to the
  new buffer in it."
			(config-manager-name this)
			(config-manager-name this)
			(config-manager-name this))
	       (interactive
		(list (config-manager-read-new-name ,singleton-variable-sym nil
						    (not current-prefix-arg))))
	       (let* ((this ,singleton-variable-sym))
		 (config-manager-insert-entry this name start-dir new-frame-p t))))

      ("switch" (defun ,(intern (format "%s-switch" cname)) (&optional name)
		  ,(format "\
Switch to %s NAME, which is prompted from the user.

When invoked with \\[universal-argument] a new NAME for the %s is
prompted to the user.  If the name of the %s doesn't yet exist,
it will be created.  Otherwise, the default is used which is a
selected entry that isn't currently in any window in the current
frame."
			   (config-manager-name this)
			   (config-manager-name this)
			   (config-manager-name this))
		  (interactive
		   (list
		    (let ((this ,singleton-variable-sym))
		      (if current-prefix-arg
			  (buffer-manager-read-name
			   this (format "Switch to %s"
					(config-manager-name this)))
			(if (= 0 (length (config-manager--entries this)))
			    (config-manager-read-new-name ,singleton-variable-sym
							  nil t))))))
		  (let* ((this ,singleton-variable-sym)
			 (entry (buffer-manager-switch this (or name 'cycle))))
		    (message "Switched to `%s'" (config-entry-name entry))
		    entry)))
      ("toggle-cycle-method"
       (defun ,(intern (format "%s-toggle-cycle-method" cname)) ()
	 "Toggle cycle methods \(i.e. last visited vs. next buffer)."
	 (interactive)
	 (let ((this ,singleton-variable-sym)
	       method)
	   (setq method (config-manager-toggle-cycle-method this))
	   (message "Set cycle method to `%S'" method))))

      ("toggle-include-frame-windows"
       (defun ,(intern (format "%s-toggle-include-frame-windows" cname)) ()
	 "Toggle cycle methods \(i.e. last visited vs. next buffer)."
	 (interactive)
	 (let* ((this ,singleton-variable-sym)
		(incp (buffer-manager-include-frame-wins this)))
	   (setq incp (not incp))
	   (buffer-manager-set-include-frame-wins this incp)
	   (message "Now %sincluding frame windows" (if incp "" "not ")))))

      ("list" (defun ,(intern (format "%s-list" cname)) ()
		,(format "\
List the information for all %ss.
In this buffer, you can rename and go to %ss"
			 (config-manager-name this)
			 (config-manager-name this))
		(interactive)
		(let ((this ,singleton-variable-sym))
		  (config-manager-list-entries-buffer
		   ,singleton-variable-sym
		   (format "*%s Entries*"
			   (capitalize (config-manager-name this)))))))

      ("rename" (defun ,(intern (format "%s-rename" cname)) ()
		  ,(format "Rename the buffer entry %ss."
			   (config-manager-name this))
		  (interactive)
		  (let* ((this ,singleton-variable-sym)
			 (entry (config-manager-current-instance this t))
			 (default (file-name-nondirectory
				   (directory-file-name default-directory)))
			 (name (buffer-manager-read-name this "New name")))
		    (config-entry-set-name entry name))))

      ("display-all" (defun ,(intern (format "%s-display-all" cname)) ()
		       "Show all entries in the current frame."
		       (interactive)
		       (let* ((this ,singleton-variable-sym))
			 (buffer-manager-display-entries this)))))))

(cl-defmethod buffer-manager-bind-interactive-functions ((this buffer-manager))
  "This binds the keys to the user functions for THIS buffer manager.

See `buffer-manager-create-interactive-functions'."
  (let ((funcs (buffer-manager-interactive-functions this 'none))
	(bindings (car (delq nil (mapcar (lambda (entry)
					   (if (null (car entry))
					       (cadr entry)))
					 buffer-manage-key-bindings)))))
    (setq bindings
	  (append bindings
		  (cadr (assoc (config-manager-name this)
			       buffer-manage-key-bindings))))
    (dolist (binding bindings)
      (let ((def (cadr (assoc (cl-first binding) funcs)))
	    (keymap (cl-second binding))
	    (key (read-kbd-macro (cl-third binding))))
	(setq keymap (if keymap (symbol-value keymap) (current-global-map)))
	(define-key keymap key (cl-second def))
	(message "Bound function `%S' key `%s' on %S"
		 (cl-second def)
		 (cl-third binding)
		 (or (cl-second binding) 'global))))))

(cl-defmethod buffer-manager-key-bindings ((this buffer-manager))
  "Return the key bindings for THIS buffer manager.

This returns a list of tuples having the form:
  (<STRING FUNCTION NAME> <MODE MAP> <KEYBOARD MACRO>
where:
  STRING FUNCTION NAME is the name prefixed by the manager name
  MODE MAP is the mode map (i.e. `shell-mode-map')
  KEYBOARD MACRO is a string read by `read-kbd-macro'

See `buffer-manager-create-interactive-functions'.
See overriden method: https://github.com/plandes/bshell/blob/master/bshell.el"
  (ignore this))

(cl-defmethod buffer-manager-create-interactive-functions
  ((this buffer-manager) singleton-variable-sym)
  "Create, evaluate and compile Emacs Lisp user functions and bindings.

This does the following:

  1. Calls `buffer-manager-interactive-functions' to create the user functions.
  2. Calls `buffer-manager-key-bindings' to get the key bindings.
  3. Calls `buffer-manager-bind-interactive-functions' to bind the keys to the
     functions.

SINGLETON-VARIABLE-SYM is an interned symbol that is bound to the singleton
  class instance that points to THIS.

THIS the singleton class instance."
  (add-to-list 'buffer-manage-remap-instances singleton-variable-sym)
  (let ((defs (mapcar 'cl-second
		      (buffer-manager-interactive-functions
		       this singleton-variable-sym)))
	(more-bindings (buffer-manager-key-bindings this))
	(name (config-manager-name this))
	fns)
    (dolist (def defs)
      (let ((fn (eval def)))
	(setq fns (append fns (list fn)))
	(message "Created function `%S'" fn)))
    (when (and more-bindings
	       (not (assq name buffer-manage-key-bindings)))
      (setq buffer-manage-key-bindings
	    (append buffer-manage-key-bindings `((,name ,more-bindings)))))
    fns))



;;; key bindings interactive
(defvar buffer-manager-read-bind-choices-history nil)

(defun buffer-manager-read-bind-choices (&optional use-last-default-p)
  "Read user input that indicates how to switch between buffer entries.
USE-LAST-DEFAULT-P, switch to the previous setting if non-nil."
  (let ((choices (mapcar (lambda (inst)
			   (cons (config-manager-name (symbol-value inst))
				 inst))
			 buffer-manage-remap-instances))
	(second (cl-second buffer-manager-read-bind-choices-history)))
    (cdr (assoc
	  (if (and use-last-default-p second)
	      (progn
		(setq buffer-manager-read-bind-choices-history
		      (cons second buffer-manager-read-bind-choices-history))
		second)
	    (let* ((def (or second
			    (car buffer-manager-read-bind-choices-history)))
		   (prompt (choice-program-complete-default-prompt
			    "Buffer manager bind" def)))
	      (completing-read
	       prompt (mapcar 'car choices) nil t
	       nil 'buffer-manager-read-bind-choices-history def)))
	  choices))))

(defun buffer-manager-bind-functions (buffer-manager-instance-var)
  "Rebind the mapping of the keys defined in `buffer-manage-key-bindings'.
These keys are then mapped to the instance specified from user
input.  Switch to the previous setting \(if there is one) when
invoked with \\[universal-argument].

For example, for a shell buffer managment instance, there could be one key to
create a new shell, another key to switch and a third to go to the listing of
current shells.

BUFFER-MANAGER-INSTANCE-VAR is symbol variable that has the
binding (see `buffer-manager-read-bind-choices')."
  (interactive (list (buffer-manager-read-bind-choices current-prefix-arg)))
  (let ((this (symbol-value buffer-manager-instance-var)))
    (buffer-manager-bind-interactive-functions this)
    (message "Bound keys to manager `%s'" (config-manager-name this))))

(provide 'buffer-manage)

;;; buffer-manage.el ends here

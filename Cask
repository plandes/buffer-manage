;; -*- emacs-lisp -*-
(source gnu)
(source melpa)

(depends-on "noflet")
(depends-on "choice-program"
	    :git "https://github.com/plandes/choice-program"
	    :files ("lisp/*.el"))

(package-file "lisp/buffer-manage.el")

(files "lisp/*.el" "doc/*.texi" (:exclude ".dir-locals.el"))

(development
 (depends-on "dash")
 (depends-on "ert-runner"))

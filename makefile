## makefile automates the build and deployment for Emacs Lisp projects

# type of project
PROJ_TYPE=	elisp

# turn off package linting for 'doesn't start with package's prefix' errors
EL_SKIP_LINT =	1

include ./zenbuild/main.mk

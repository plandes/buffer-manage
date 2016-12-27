EMACS ?=	emacs
EMACSFLAGS =	-L .
CASK ?=		cask
LISP_DIR=	lisp
ELS=		$(wildcard $(LISP_DIR)/*.el)
OBJECTS=	$(ELS:.el=.elc)


.PHONY:		all
all:		package

.PHONY:		info
info:
		@echo "version: $(VERSION)"

# patterns
%.elc:		%.el
		$(CASK) build

# lifecycle
elpa:
		$(CASK) install || true
		$(CASK) update
		touch $@

.PHONY:		build
build:		elpa $(OBJECTS)

.PHONY:		test
test:		elpa cleantest
		$(CASK) exec ert-runner -L $(LISP_DIR) -L .

.PHONY:		package
package:	test
		$(CASK) package

# clean
.PHONY:		cleantest
cleantest:
		rm -f $(OBJECTS)

.PHONY:		clean
clean:		cleantest
		rm -rf elpa dist

.PHONY:		cleanall
cleanall:	clean
		rm -rf .cask $(GTAGUTIL)

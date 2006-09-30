EMACS=emacs

LISP_FILES = 	css-mode.el \
		gnuserv.el \
		htmlize.el \
		javascript-mode.el \
		pc-bufsw.el \
		php-mode.el \
		python-mode.el \
		tabbar.el \
		xcscope.el \


SUBDIRS = 	mmm-mode \


ELC_FILES = $(LISP_FILES:.el=.elc)
GEN-SUBDIRS = $(addprefix gen-,$(SUBDIRS))

all	:	$(ELC_FILES) $(GEN-SUBDIRS) compile-psgml
	@echo Done

%.elc	:	%.el Makefile
	$(EMACS) -q -batch -f batch-byte-compile "$<"

$(GEN-SUBDIRS): gen-%:
	$(MAKE) -f subdir.mk -C $*

compile-psgml	:	psgml
	cd psgml; \
	$(EMACS) -q -batch -l psgml-maint.el -f psgml-compile-files

clean	:	
	find . -name '*.elc' -exec rm {} \;

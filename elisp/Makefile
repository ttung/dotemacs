LISP_FILES = 	css-mode.el \
		gnuserv.el \
		htmlize.el \
		ispell4.el \
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
	emacs -batch --eval '(byte-compile-file "$<")'

$(GEN-SUBDIRS): gen-%:
	make -f subdir.mk -C $*

compile-psgml	:	psgml
	cd psgml; \
	emacs -batch -l psgml-maint.el -f psgml-compile-files

clean	:	
	find . -name '*.elc' -exec rm {} \;

LISP_FILES = andersl-java-font-lock.el \
		gnuserv.el \
		htmlize.el \
		ispell4.el \
		pc-bufsw.el \
		python-mode.el \
		tabbar.el \
		xcscope.el \

ELC_FILES = ${LISP_FILES:.el=.elc}

all	:	${ELC_FILES} compile-psgml
	@echo Done

compile-psgml	:	psgml
	cd psgml; \
	emacs -batch -l psgml-maint.el -f psgml-compile-files

%.elc	:	%.el Makefile
	emacs -batch --eval '(byte-compile-file "$<")'

clean	:	
	find . -name '*.elc' -exec rm {} \;

LISP_FILES = andersl-java-font-lock.el \
		generic-extras.el \
		generic-mode.el \
		gnuserv.el \
		htmlize.el \
		ispell4.el \
		pc-bufsw.el \
		verilog-mode.el \
		vhdl-mode.el \
		xcscope.el \

ELC_FILES = ${LISP_FILES:.el=.elc}

all	:	${ELC_FILES} compile-psgml
	@echo Done

compile-psgml	:	psgml
	cd psgml; \
	emacs -batch -l psgml-maint.el -f psgml-compile-files

generic-extras.elc	:	generic-extras.el generic-mode.elc
	emacs -batch -l generic-mode.elc --eval '(byte-compile-file "generic-extras.el")'

%.elc	:	%.el Makefile
	emacs -batch --eval '(byte-compile-file "$<")'

clean	:	
	find . -name '*.elc' -exec rm {} \;

LISP_FILES = andersl-java-font-lock.el \
		cscope.el \
		generic-extras.el \
		generic-mode.el \
		gnuserv.el \
		htmlize.el \
		ispell4.el \
		pc-bufsw.el \
		verilog-mode.el \
		vhdl-mode.el \

ELC_FILES = ${LISP_FILES:.el=.elc}

all	:	${ELC_FILES} compile-psgml
	@echo Done

compile-psgml	:	psgml
	cd psgml; \
	emacs -batch -l psgml-maint.el -f psgml-compile-files

%.elc	:	%.el Makefile
	emacs -batch --eval '(byte-compile-file "$<")'

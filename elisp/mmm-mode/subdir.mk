EMACS=emacs

LISP_FILES = 	mmm-auto.el \
		mmm-class.el \
		mmm-cmds.el \
		mmm-compat.el \
		mmm-cweb.el \
		mmm-mason.el \
		mmm-mode.el \
		mmm-noweb.el \
		mmm-region.el \
		mmm-rpm.el \
		mmm-sample.el \
		mmm-univ.el \
		mmm-utils.el \
		mmm-vars.el \


ELC_FILES = $(LISP_FILES:.el=.elc)

all	:	$(ELC_FILES)


%.elc	:	%.el subdir.mk
	$(EMACS) -q -batch --eval '(setq load-path (cons nil load-path))' -f batch-byte-compile "$<"

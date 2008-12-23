EMACS=emacs

LISP_FILES = 	byte-code-cache.el

SUBDIRS = 	dvc


ELC_FILES = $(LISP_FILES:.el=.elc)
GEN-SUBDIRS = $(addprefix gen-,$(SUBDIRS))

all	:	$(ELC_FILES) $(GEN-SUBDIRS)
	@echo Done

%.elc	:	%.el Makefile
	$(EMACS) -q -batch -f batch-byte-compile "$<"

$(GEN-SUBDIRS): gen-%:
	$(MAKE) -f subdir.mk -C $*

clean	:
	find . -name '*.elc' -exec rm {} \;

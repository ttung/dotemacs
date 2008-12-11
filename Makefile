EMACS=emacs

all	:	${HOME}/.emacs settings.elc elisp-compile
	chmod -R a+r *
	@echo Done

${HOME}/.emacs	:	emacs
	ln -fs ${CURDIR}/emacs ${HOME}/.emacs

%.elc	:	%.el
	$(EMACS) -batch -q --no-site-file -f batch-byte-compile $<

elisp-compile	:
	$(MAKE) -C elisp all

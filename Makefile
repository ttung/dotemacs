all	:	${HOME}/.emacs elisp-compile
	@echo Done

${HOME}/.emacs	:	emacs
	ln -fs ${CURDIR}/emacs ${HOME}/.emacs

elisp-compile	:
	${MAKE} -C elisp all

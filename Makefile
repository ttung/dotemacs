all	:	${HOME}/.emacs elisp-compile
	chmod -R a+r *
	@echo Done

${HOME}/.emacs	:	emacs
	ln -fs ${CURDIR}/emacs ${HOME}/.emacs

elisp-compile	:
	${MAKE} -C elisp all

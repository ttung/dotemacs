EMACS=emacs

all	:	${HOME}/.emacs
	chmod -R a+r *
	@echo Done

${HOME}/.emacs	:	emacs
	ln -fs ${CURDIR}/emacs ${HOME}/.emacs

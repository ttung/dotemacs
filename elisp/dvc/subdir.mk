EMACS=emacs

all	:	dvc-autoloads.el

dvc-autoloads.el	:
	srcdir="." $(EMACS) -batch -q --no-site-file -l dvc-build.el -f dvc-build-all

clean	:
	rm dvc-autoloads.el

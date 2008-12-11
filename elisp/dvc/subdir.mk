EMACS=emacs

all	:	dvc-autoloads.elc contrib/ewoc.elc

dvc-autoloads.elc	:
	$(EMACS) -batch -q --no-site-file -l dvc-build.el -f dvc-build-all

contrib/ewoc.elc	:	contrib/ewoc.el
	$(EMACS) -batch -q --no-site-file -f batch-byte-compile $<

clean	:
	rm *.elc dvc-autoloads.el contrib/*.elc

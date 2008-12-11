; -*- mode: emacs-lisp -*-
;;
;; Load DVC easily ...
;;
;; Manually, you can run
;;
;;   M-x load-file RET /path/to/dvc-load.el RET
;;
;; (usefull when you want to load DVC after starting "emacs -q"!), or
;; add
;;
;;   (load-file "/path/to/this/file/in/builddir/dvc-load.el")
;;
;; to your ~/.emacs.el

(add-to-list 'load-path 
             (reduce 'path-join '("emacs" "elisp" "dvc") :initial-value home-directory))
(unless (locate-library "ewoc")
  (add-to-list 'load-path 
               (reduce 'path-join '("emacs" "elisp" "dvc" "contrib") :initial-value home-directory)))

(if (featurep 'dvc-core)
    (dvc-reload)
  (require 'dvc-autoloads))

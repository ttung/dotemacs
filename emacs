(require 'cl)

(defvar home-directory (file-name-directory user-init-file))
(defun path-join (head tail)
  (expand-file-name tail head))

(setq debug-on-error 't)

(load (reduce 'path-join '("emacs" "elisp" "byte-code-cache") :initial-value home-directory))
(load (reduce 'path-join '("emacs" "settings.el") :initial-value home-directory))

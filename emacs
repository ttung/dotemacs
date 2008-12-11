(require 'cl)

(defvar home-directory (file-name-directory user-init-file))
(defun path-join (head tail)
  (concat (file-name-as-directory head) tail))

(load-file (reduce 'path-join '("emacs" "settings.el") :initial-value home-directory))

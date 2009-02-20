(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-version-num
  (+
   emacs-major-version
   (/ emacs-minor-version 100.0))
  "The major and minor version number converted into a floating-point value.")


(when (boundp 'user-init-file)
  (defvar home-directory (file-name-directory user-init-file))
  (defun path-join (head tail)
    (expand-file-name tail head))

  (setq debug-on-error 't)

  (when (> emacs-version-num 22.0)
    (load (reduce 'path-join '("emacs" "elisp" "byte-code-cache") :initial-value home-directory))
    (setq bcc-cache-directory (reduce 'path-join (list ".emacs.d" (format "%s" emacs-version-num) "byte-cache") :initial-value home-directory)))

  (load (reduce 'path-join '("emacs" "settings.el") :initial-value home-directory)))

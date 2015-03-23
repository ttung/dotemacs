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
  (defvar nep-base (file-name-directory load-file-name))
  (defvar home-directory (expand-file-name "~/"))
  (defun path-join (head tail)
    (expand-file-name tail head))

  (setq debug-on-error 't)

  (add-to-list 'load-path
    (reduce 'path-join '("emacs" "elisp") :initial-value nep-base))

  (setq load-prefer-newer t)
  (require 'auto-compile)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  (load (reduce 'path-join '("emacs" "settings.el") :initial-value nep-base)))

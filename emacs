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

  (let* ((temp-byte-code-cache-directory
          (reduce 'path-join (list ".emacs.d" (format "%s" emacs-version-num) "byte-cache") :initial-value home-directory))
         (fail-code
          (cond
           ((<= emacs-version-num 22.0)
            "Emacs version must be > 22.0 to use byte-code-cache.  Falling back on elisp sources.")
           ((not (and (file-directory-p temp-byte-code-cache-directory)
                      (file-writable-p temp-byte-code-cache-directory)))
            (format "byte-code-cache directory (%s) does not exist or cannot be written to.  Falling back on elisp sources." temp-byte-code-cache-directory))
           nil)))

    (unless fail-code
      (setq bcc-cache-directory temp-byte-code-cache-directory)
      (load (reduce 'path-join '("emacs" "elisp" "byte-code-cache") :initial-value nep-base)))

    (load (reduce 'path-join '("emacs" "settings.el") :initial-value nep-base))

    (when fail-code
      (display-warning 'byte-code-cache fail-code :warning))))


(defun get-command-output (executable &rest args)
  (unless args
    (setq args '()))

  (let ((full-path (executable-find executable)))
    (unless full-path
      (error "Cannot find %s" executable))

    ;; execute it.
    (let ((apply-args (append (list full-path nil t t) args)))
      (with-temp-buffer
        (let ((errcode (apply 'call-process apply-args)))
          (unless (= errcode 0)
            (error "Error running %s (code=%d)" full-path errcode))

          (end-of-buffer)

          ;; chop off the last CR/LF.
          (buffer-substring 1 (- (point) 1)))))))


;; the modules library expects these to be set.
(unless (getenv "MACHTYPE")
  (setenv "MACHTYPE" (get-command-output "uname" "-m")))
(unless (getenv "OSTYPE")
  (setenv "OSTYPE" (get-command-output "uname" "-s")))


;; given a command, return either a buffer with the commands to
;; execute or nil in the case of an error.  in the case of an error,
;; we will also retain a buffer called *module stderr* explaining the
;; errors.
(defun modulecmd (cmd &optional pre-cmd-args post-cmd-args)
  (unless pre-cmd-args
    (setq pre-cmd-args '()))
  (unless post-cmd-args
    (setq post-cmd-args '()))
  (let* ((args (append (list "-s" "elisp") pre-cmd-args (list cmd) post-cmd-args))
         (stdout-buffer (generate-new-buffer "*module stdout*"))
         (stderr-file (make-temp-file "module-stderr"))
         (apply-args (append (list "/Users/tonytung/software/pyenv/modulecmd"
                                   nil (list stdout-buffer stderr-file)
                                   't) args)))
    (unwind-protect
        ;; execute the function
        (let ((errcode (apply 'call-process apply-args)))

          (if (= errcode 0)
              (let ((commands-buffer (generate-new-buffer "*module commands*"))
                    (stdout-content nil))

                ;; yay, we successfully executed.  stdout contains the
                ;; filename that we should evaluate.
                (with-current-buffer stdout-buffer
                  (end-of-line)
                  (setq stdout-content (buffer-substring 1 (- (point) 1))))

                ;; read the elisp from that file.
                (with-current-buffer commands-buffer
                  (insert-file-contents-literally stdout-content))

                ;; clean up
                (kill-buffer stdout-buffer)
                (delete-file stdout-content)

                ;; return stdout content
                commands-buffer)

            ;; failed...  return stderr contents.

            (let ((stderr-buffer (generate-new-buffer "*module stderr*")))
              ;; read the contents of stderr-file into stderr-buffer
              (with-current-buffer stderr-buffer
                (insert-file-contents-literally stderr-file))

              ;; show the buffer
              (split-window-vertically)
              (switch-to-buffer stderr-buffer)

              ;; clean up
              (kill-buffer stdout-buffer)

              nil)))

      (progn
        (when stderr-file
          (delete-file stderr-file))))))


(defun module-avail-list ()
  (let ((cmd-buffer (modulecmd "avail" (list "--raw-msg-dump")))
        (output nil))
    (when cmd-buffer
      (with-current-buffer cmd-buffer
        (setq output (delq nil
                           (mapcar
                            (lambda (x) (if (= 0 (length x)) nil x))
                            (split-string (buffer-string) "\n"))))))

    (kill-buffer cmd-buffer)

    output))


(defun module-loaded-list ()
  (let ((cmd-buffer (modulecmd "loaded" (list "--raw-msg-dump")))
        (output nil))
    (when cmd-buffer
      (with-current-buffer cmd-buffer
        (setq output (delq nil
                           (mapcar
                            (lambda (x) (if (= 0 (length x)) nil x))
                            (split-string (buffer-string) "\n"))))))

    (kill-buffer cmd-buffer)

    output))


(defvar module-completer-cache nil
  "cache for module-completer")


;; (defun module-completer (str predicate mode)
;;   ;; pre-populate the ccahe
;;   (unless module-completer-cache
;;     (setq module-completer-cache (module-avail-list)))

;;   (message mode)

;;   (let ((results '()))
;;     (dolist (module-name module-completer-cache)
;;       (when (compare-strings str 1 nil
;;                              module-name 1 (length module-name))
;;         (setq results (append results module-name))))

;;     (cond ((= mode nil)
;;            ;; all of the strings have the same prefix
;;            )))
;;         ((= mode nil)
;;          (memq 

(defun module-avail ()
  (interactive)

  (let ((cmd-buffer (modulecmd "avail")))
    (when cmd-buffer
      (eval-buffer cmd-buffer)
      (kill-buffer cmd-buffer))))


(defun module-loaded ()
  (interactive)

  (let ((cmd-buffer (modulecmd "loaded")))
    (when cmd-buffer
      (eval-buffer cmd-buffer)
      (kill-buffer cmd-buffer))))


(defun module-load ()
  (interactive)

  (setq module-completer-cache nil)
  (setq module-completer-cache (module-avail-list))
  (let* ((module-name (completing-read
                       "Module to load: "
                       module-completer-cache))
         (cmd-buffer (modulecmd "load" '() (list module-name))))
    (when cmd-buffer
      (eval-buffer cmd-buffer)
      (kill-buffer cmd-buffer))))


(defun module-unload ()
  (interactive)

  (setq module-completer-cache nil)
  (setq module-completer-cache (module-loaded-list))
  (let* ((module-name (completing-read
                       "Module to unload: "
                       module-completer-cache))
         (cmd-buffer (modulecmd "unload" '() (list module-name))))
    (when cmd-buffer
      (eval-buffer cmd-buffer)
      (kill-buffer cmd-buffer))))

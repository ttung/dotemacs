;; Nice Emacs Package
;; (Yen-Ting) Tony Tung
;; version 6.14
;; 2000 July 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start debugging messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq debug-on-error t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq make-backup-files nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-version-num 
  (+
   emacs-major-version
   (/ emacs-minor-version 100.0))
  "The major and minor version number converted into a floating-point value.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global extras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-load (file)
  "Calls load with FILE as an argument.  Inhibits any fatal messages but quietly indicates failure and sets my-load-errors if errors occur."
  (let ((resp (load file t)))
    (if (eq resp nil)
        (message "Unable to load %s" file))
    resp))

(defun want (feature &optional filename)
  "Attempt to load FEATURE if available, but don't complain if it isn't.
See require. Return non-nil if FEATURE is or was loaded."
  (if (not (featurep feature))
      (if (not filename)
          (my-load (symbol-name feature))
        (my-load filename))
    t))

;; set up the paths for custom files
(if (eq system-type 'windows-nt)
    (setq exec-path 
          (cons 
           (expand-file-name "~/emacs/bin") 
           exec-path)))
(setq load-path 
      (cons 
       (expand-file-name "~/emacs/elisp") 
       load-path))

(setq auto-mode-alist (cons 
                       '("\\.emt\\'" . text-mode) 
                       auto-mode-alist))

(setq auto-mode-alist (cons 
                       '("\\.c\\'" . c++-mode) 
                       auto-mode-alist))

(setq auto-mode-alist (cons 
                       '("\\.h\\'" . c++-mode) 
                       auto-mode-alist))

(setq auto-mode-alist (cons 
                       '("\\.y\\'" . c++-mode) 
                       auto-mode-alist))

(setq auto-mode-alist (cons 
                       '("\\.lex\\'" . c++-mode) 
                       auto-mode-alist))

(setq completion-ignored-extensions
      (append completion-ignored-extensions '(".ps" ".pdf")))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up the paths for psgml
(setq load-path
      (cons 
       (expand-file-name "~/emacs/elisp/psgml") 
       load-path))

(if (want 'psgml)
    (progn
      (defvar sgml-data-directory (expand-file-name "~/emacs/etc/sgml"))
      (setq sgml-catalog-files '("~/emacs/etc/sgml/CATALOG" "CATALOG"))
      (setq sgml-ecat-files '("~/emacs/etc/sgml/ECAT" "ECAT"))
      ;;(defvar sgml-trace-entity-lookup t)

      ;; initialize psgml
      (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
      (autoload 'html-mode "psgml-html" "Major mode to edit HTML files." t)
      (setq auto-mode-alist (cons 
                             '("\\.html" . html-mode) 
                             auto-mode-alist))))


;; set up the java system
(if (want 'andersl-java-font-lock)
    (progn
      (setq auto-mode-alist (cons 
                             '("\\.java" . java-mode) 
                             auto-mode-alist))

      (add-hook 'java-mode-hook 'my-java-mode-hook)
      (defun my-java-mode-hook ()
        (cond (window-system
               (want 'andersl-java-font-lock)
               (turn-on-font-lock))))))

;; set up the verilog system
(if (want 'verilog-mode)
    (progn
      (autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
      (setq auto-mode-alist (cons
                             '("\\.v\\'" . verilog-mode) auto-mode-alist))
      (add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))))

;; set up the VHDL system
(if (> emacs-version-num 20.03)
    (if (want 'vhdl-mode)
        (progn 
          (autoload 'vhdl-mode "vhdl-mode" "VHDL Editing Mode" t)
          (setq auto-mode-alist (append '(("\\.vhdl?$" . vhdl-mode)) auto-mode-alist)))))

;; set up generic modes
(if (> emacs-version-num 19.28)
    (progn
      (want 'generic-mode)
      (want 'generic-extras)))

(if (> emacs-version-num 19.28)
    (want 'htmlize))
(if (> emacs-version-num 19.28)
    (if (want 'pc-bufsw)
        (pc-bufsw::bind-keys [C-tab] [C-S-tab])))

(if window-system 
    (if (eq system-type 'windows-nt)
        (if (want 'gnuserv)
            (gnuserv-start))
      (server-start)))

(if (want 'iswitchb)
    (progn
      (iswitchb-default-keybindings)
      (setq iswitchb-default-method 'samewindow)) ;always go to the same window
  (global-unset-key "\C-xb")
  (global-set-key "\C-xb" 's-switch-to-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing existing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set the indent correctly
(defun my-c-mode-common-hook ()
  (progn
    (setq c-basic-offset 2)
    (setq comment-column 60)
    (auto-fill-mode)
    (setq fill-column 100)
    (local-set-key "\C-c\C-w" 'c-wrap-conditional)))

;; set hooks
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; set up the font-lock system

(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)))
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)

(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Win32 utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ispell
;;
(if (eq system-type 'windows-nt)
    (progn
      (autoload 'ispell-word "ispell4" 
	"Check spelling of word at or before point" t)
      (autoload 'ispell-complete-word "ispell4" 
	"Complete word at or before point" t)
      (autoload 'ispell-region "ispell4" 
	"Check spelling of every word in the region" t)
      (autoload 'ispell-buffer "ispell4" 
	"Check spelling of every word in the buffer" t)
      (setq ispell-command (expand-file-name "~/emacs/ispell/ispell.exe")
            ispell-look-dictionary (expand-file-name "~/emacs/ispell/ispell.words")
            ispell-look-command (expand-file-name "~/emacs/ispell/look.exe")
            ispell-command-options (list "-d" (expand-file-name "~/emacs/ispell/ispell.dict"))
            )))

;; italic fonts
(if (eq system-type 'windows-nt)
    (progn
      (setq w32-enable-italics t)       ; This must be done before font settings!
                                        ; use interactive set-font-face followed by describe-face to determine this
      (set-face-font 'italic "-*-Courier New-normal-i-*-*-13-*-*-*-c-*-fontset-standard")
      (set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-13-*-*-*-c-*-fontset-standard")))

;; RCS for Win32
(if (eq system-type 'windows-nt)
    (progn
      (defadvice vc-do-command (before patch-vc-do-command-for-nt activate)
	"Patch vc-do-command to work under NT, so ediff-revision can work."
	;; First, let's check for the case we want to patch:
	(if (and (string= (ad-get-arg 2) "/bin/sh")
                 (string= (ad-get-arg 5) "-c")
                 (string-match (concat "^if \\[ x\"\\$1\" = x \\]; then shift; fi;[ \t]+"
                                       "umask [0-9]+; exec >\"\$1\" || exit;[ \t]+shift; "
                                       "umask 0; exec co \"\\$@\"") (ad-get-arg 6))
                 (string= (ad-get-arg 7) ""))
            (progn (ad-set-arg 2 "cmd");; Invoke "cmd" instead of "/bin/sh"
                   (ad-set-arg 5 "/c");; Convert "-c" to "/c"
                   (ad-set-arg 6 nil);; Zap the /bin/sh script
                   (ad-set-arg 7 "co");; Invoke the "co" command
                   (ad-set-arg 8 (concat ">" (ad-get-arg 8)));; Redirect to outfile
                   ))
	;; next, let's check for the CVS case
	(if (and (string= (ad-get-arg 2) "/bin/sh")
                 (string= (ad-get-arg 5) "-c")
                 (string-match "^exec" (ad-get-arg 6))
                 (string= (ad-get-arg 7) ""))
            (progn (ad-set-arg 2 "cmd");; Invoke "cmd" instead of "/bin/sh"
                   (ad-set-arg 5 "/c");; Convert "-c" to "/c"
                   (ad-set-arg 6 nil);; Zap the /bin/sh script
                   (ad-set-arg 7 "cvs update");; Invoke the "cvs" command
                   (ad-set-arg 8 (concat ">" (ad-get-arg 8)));; Redirect to outfile
                   )))))

;; set up the ange-ftp ftp program
(if (eq system-type 'windows-nt)
    (progn
      (setq ange-ftp-ftp-program-name "~/emacs/bin/ftp.exe")))

;; set up the ange-ftp temporary directory
(if (eq system-type 'windows-nt)
    (progn
      (setq ange-ftp-tmp-name-template 
            (concat 
             (expand-file-name (getenv "TEMP")) 
             "/ange-ftp"))
      (setq ange-ftp-gateway-tmp-name-template 
            (concat 
             (expand-file-name (getenv "TEMP")) 
             "/ange-ftp"))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIX tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; command-line shell - remove the ^M
(if (eq system-type 'usg-unix-v)
    (progn
      (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Win32 tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; binary mode
;;(if (eq system-type 'windows-nt)
;;    (setq file-name-buffer-file-type-alist '(("\\.bat$" . nil) ("\\.txt$" . nil) (".*" . t))))

;; command-line shell - remove the echo
;;(if (eq system-type 'windows-nt)
;;    (progn
;;      (setq comint-process-echoes t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Basic stuff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq standard-indent 2)

;; set up matching parentheses
(if (> emacs-version-num 19.30)
    (show-paren-mode 1))

;; set the scroll bar to the right side
(if (= emacs-version-num 20.02)
    (set-scroll-bar-mode 'right 'right))
(if (> emacs-version-num 20.02)
    (set-scroll-bar-mode 'right))

;; show marked area
(transient-mark-mode t)

;; scroll step
(setq scroll-step 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Fonts & Colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up some colors
(if (eq window-system nil)
    (menu-bar-mode 0)
  (progn
    (set-cursor-color "GREEN")
    (set-mouse-color "MAGENTA")
    (set-face-background 'default "BLACK")
    (set-face-foreground 'default "WHITE")
    (set-face-background 'highlight "DARKSEAGREEN2")
    (set-face-foreground 'highlight "WHITE")
    (set-face-background 'modeline "MIDNIGHTBLUE")
    (set-face-foreground 'modeline "CYAN")
    (set-face-background 'region "NAVY")
    (set-face-foreground 'region "CYAN")
    (set-foreground-color "WHITE")
    (set-background-color "BLACK")
    (if (> emacs-version-num 19.34)
	(progn
          ;; (set-face-background 'bold "BLACK")
          ;; (set-face-foreground 'bold "WHITE")
          ;; (set-face-background 'bold-italic "BLACK")
          ;; (set-face-foreground 'bold-italic "WHITE")
          ;; (set-face-background 'highlight "DARKSEAGREEN2")
          ;; (set-face-foreground 'highlight "WHITE")
          ;; (set-face-background 'italic "BLACK")
          ;; (set-face-foreground 'italic "WHITE")
          ;; (set-face-background 'secondary-selection "PALETURQUOISE")
          ;; (set-face-foreground 'secondary-selection "WHITE")
          ;; (set-face-background 'underline "BLACK")
          ;; (set-face-foreground 'underline "WHITE") 
          (set-face-background 'show-paren-match-face "NAVY")
          (set-face-foreground 'show-paren-match-face "CYAN")
          (set-face-background 'show-paren-mismatch-face "PURPLE")
          (set-face-foreground 'show-paren-mismatch-face "WHITE")
          (set-face-background 'font-lock-builtin-face "BLACK")
          (set-face-foreground 'font-lock-builtin-face "VIOLET")
          (set-face-background 'font-lock-comment-face "BLACK")
          (set-face-foreground 'font-lock-comment-face "ROSYBROWN2")
          (set-face-background 'font-lock-function-name-face "BLACK")
          (set-face-foreground 'font-lock-function-name-face "LIGHTSKYBLUE")
          (set-face-background 'font-lock-keyword-face "BLACK")
          (set-face-foreground 'font-lock-keyword-face "LIGHTSTEELBLUE")
          (set-face-background 'font-lock-string-face "BLACK")
          (set-face-foreground 'font-lock-string-face "LIGHTSALMON")
          (set-face-background 'font-lock-type-face "BLACK")
          (set-face-foreground 'font-lock-type-face "PALEGREEN")
          (set-face-background 'font-lock-variable-name-face "BLACK")
          (set-face-foreground 'font-lock-variable-name-face "LIGHTGOLDENROD")
          (set-face-background 'font-lock-warning-face "BLACK")
          (set-face-foreground 'font-lock-warning-face "RED") ))
    (if (> emacs-version-num 20.02)
        (progn
          (set-face-background 'font-lock-constant-face "BLACK")
          (set-face-foreground 'font-lock-constant-face "CADETBLUE"))
      (progn
        (set-face-background 'font-lock-reference-face "BLACK")
        (set-face-foreground 'font-lock-reference-face "CADETBLUE")))))

;; set up the font menu
(setq
 x-fixed-font-alist
 '("Font Menu"
   ("Misc"
    ("6x12" "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-*-1")
    ("6x13" "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*-1")
    ("lucida 13"
     "-b&h-lucidatypewriter-medium-r-normal-sans-0-0-0-0-m-0-*-1")
    ("7x13" "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-1")
    ("7x14" "-misc-fixed-medium-r-normal--14-130-75-75-c-70-*-1")
    ("9x15" "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
    ("")
    ("clean 8x8" "-schumacher-clean-medium-r-normal--*-80-*-*-c-*-*-1")
    ("clean 8x14" "-schumacher-clean-medium-r-normal--*-140-*-*-c-*-*-1")
    ("clean 8x10" "-schumacher-clean-medium-r-normal--*-100-*-*-c-*-*-1")
    ("clean 8x16" "-schumacher-clean-medium-r-normal--*-160-*-*-c-*-*-1")
    ("")
    ("sony 8x16" "-sony-fixed-medium-r-normal--16-120-100-100-c-80-*-1")
    ("")
    ("-- Courier --")
    ("Courier 10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-*-1")
    ("Courier 12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-*-1")
    ("Courier 14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-*-1")
    ("Courier 18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-*-1")
    ("Courier 18-b" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-*-1")
    )))

(setq default-frame-alist
      '((foreground-color  . "white")
	(background-color  . "black")
	(cursor-color      . "green")
	(menu-bar-lines    . 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up columns
(if (> emacs-version-num 19.28)
    (column-number-mode 1))

;; give us the ability to edit 8 MB files
(setq line-number-display-limit 8388608)

;; date related stuff
(setq display-time-24hr-format t)
(if (eq window-system nil)
    ()
  (setq display-time-day-and-date t))
(display-time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Titlebar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (> emacs-version-num 19.28)
    (progn
      (setq frame-title-format
            (concat invocation-name "@" system-name " - %f"))
      (setq icon-title-format
            (concat invocation-name "@" system-name " - %b"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My own functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Scroll buffer without moving point 
(defun scroll-down-line () 
  "Scroll one line down." 
  (interactive) 
  (scroll-down 1)) 

(defun scroll-up-line () 
  "Scroll one line up." 
  (interactive) 
  (scroll-up 1)) 

(defun my-reindent (FROM TO)
  "Refill the region as a paragraph and reindent."
  (interactive "r")
  (fill-region-as-paragraph FROM TO)
  (if indent-region-function
      (funcall indent-region-function FROM TO)
    (save-excursion
      (goto-char TO)
      (setq TO (point-marker))
      (goto-char FROM)
      (or (bolp) (forward-line 1))
      (while (< (point) TO)
	(or (and (bolp) (eolp))
            (funcall indent-line-function))
	(forward-line 1))
      (move-marker TO nil))))

(defun insert-time ()
  "Insert the current time."
  (interactive)
  (let ((time-string (format-time-string "%a %b %d %H:%M:%S %Z %Y")))
    (insert "\n")
    (insert 
     (make-string 
      (length time-string)
      ?-))
    (insert "\n")
    (insert time-string)
    (insert "\n")
    (insert 
     (make-string 
      (length time-string)
      ?-))
    (insert "\n\n")))

;  (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y%n")))

(defun my-list-buffers ()
  "Display a list of names of existing buffers and sets the active
buffer to the list.
The list is displayed in a buffer named `*Buffer List*'.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive)
  (let ((num-win (count-windows)))
    (list-buffers)
    (if (not (equal (buffer-name) "*Buffer List*"))
        (progn 
          (other-window 1)
          (goto-char (point-min))))
    (if (= num-win 1)
        (delete-other-windows))
    (if (= (point) (point-min))
        (next-line 2))))

(defun my-comment ()
  "Indents a region if the mark is active.  Otherwise starts a comment."
  (interactive)
  (if mark-active
      (comment-region (point) (mark) nil)
    (indent-for-comment)))

(defun region-remove-comment(from to)
  "Removes comments from the beginning of lines within a region."
  (interactive "r")
  (comment-region from to -1))

(defun c-wrap-conditional (from to string)
  "Wraps the region with a preprocessor conditional."
  (interactive "r\nMConditional (default #if): ")
  (if (> (mark) (point))
      (exchange-point-and-mark))
  (insert "#endif" "\n")
  (backward-char 7)
  (exchange-point-and-mark)
  (if (equal string "")
      (setq string "#if "))
  (insert string "\n")
  (backward-char))

(defun comment-line (&optional ARG)
  "Comments the current line.
With just C-u prefix arg, uncomment current line.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead."
  (interactive "P")
  (beginning-of-line)
  (let ((start (point)))
    (forward-line 1)
    (comment-region start (point) ARG)))

(defun my-delete (&optional arg)
  "Deletes the region if the mark is active.  Otherwise runs delete-char."
  (interactive "P")
  (if mark-active
      (delete-region (point) (mark))
    (if (eq arg nil)
        (delete-char 1 nil)             ;no prefix
      (if (consp arg)
          (delete-char 1 t)             ;null prefix
        (delete-char arg t)))))

(defun previous-error (&optional arg)
  "Similar to next-error, except it visits the previous compilation error."
  (interactive "P")
  (if (eq arg nil)
      (next-error -1)
    (next-error (- arg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Borrowed" functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUFFER SWITCHING FIX
;;
;; This changes the behaviour of the switch-to-buffer completion functions so
;; that the current buffer is NOT in the completion list.
;;
;; i.e. say you're working in "temp.c", and you want to visit "temp.h"; so you
;; type "C-xb", then "t<TAB>" which then presents you with a completion list of
;; temp.c and temp.h, so you then must type "h<RET>".  This is annoying since 
;; why would you want to switch back to the buffer you're in?!?
;; Using this fix would remove "temp.c" from the completion lits so that when 
;; you had typed "t<TAB>" the name would be completed as "temp.h" as desired.
;;
;; Steve Dodd 
;; March 1998

(defun s-minibuffer-complete ()
  "A shell around minibuffer-complete which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete))

(defun s-minibuffer-complete-word ()
  "A shell around minibuffer-complete-word which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete-word)
  )

(defun s-minibuffer-complete-and-exit ()
  "A shell around minibuffer-complete-and-exit which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete-and-exit))


(defun s-switch-to-buffer ()
  "A shell around switch-to-buffer which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (setq s-remove-first-completion 't)
  (switch-to-buffer (read-buffer "Switch to buffer: " (other-buffer))))

(setq s-remove-first-completion 'nil)

(define-key minibuffer-local-completion-map "\040" 's-minibuffer-complete-word)
(define-key minibuffer-local-completion-map "\t" 's-minibuffer-complete)
(define-key minibuffer-local-must-match-map [return] 's-minibuffer-complete-and-exit)

;; END OF BUFFER SWITCHING FIX

;; Go to matching parentheses
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((looking-at "\\s\{") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\}") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun my-shrink-window (arg &optional side)
  (interactive "p")
  (if (and (> (count-windows) 1)
   (> (window-height (selected-window)) 4))
      (shrink-window arg side)))

(defun my-enlarge-window (arg &optional side)
  (interactive "p")
  (if (and (> (count-windows) 1)
   (> (window-height (next-window (selected-window))) 4))
      (enlarge-window arg side)))

;; stolen from iswitchb.el
(defun iswitchb-make-buflist (&optional default)
  "Set `iswitchb-buflist' to the current list of buffers.
Currently visible buffers are put at the end of the list.
The hook `iswitchb-make-buflist-hook' is run after the list has been 
created to allow the user to further modify the order of the buffer names
in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
it is put to the start of the list."
  (setq iswitchb-buflist 
        (let* ((iswitchb-current-buffers (list (buffer-name)))
               (iswitchb-temp-buflist
                (delq nil 
                      (mapcar
                       (lambda (x)
                         (let ((b-name (buffer-name x)))
                           (if (not 
                                (or 
                                 (iswitchb-ignore-buffername-p b-name)
                                 (memq b-name iswitchb-current-buffers)))
                               b-name)))
                       (buffer-list)))))
          (nconc iswitchb-temp-buflist iswitchb-current-buffers)
          (run-hooks 'iswitchb-make-buflist-hook)
          ;; Should this be after the hooks, or should the hooks be the
          ;; final thing to be run?
          (if default
              (progn
                (setq iswitchb-temp-buflist 
                      (delete default iswitchb-temp-buflist))
                (setq iswitchb-temp-buflist 
                      (cons default iswitchb-temp-buflist))))
          iswitchb-temp-buflist)))

(defun iswitchb-complete ()
  "Try and complete the current pattern amongst the buffer names."
  (interactive)
  (let (res)
    (cond ((not  iswitchb-matches)
           (iswitchb-completion-help))
            
;            ((= 1 (length iswitchb-matches))
;                ;; only one choice, so select it.
;                (exit-minibuffer))
              
          (t
           ;; else there could be some completions
           (setq res iswitchb-common-match-string)
           (if (and (not (memq res '(t nil)))
                    (not (equal res iswitchb-text)))
               ;; found something to complete, so put it in the minibuffer.
               (progn
                 (setq iswitchb-rescan nil)
                 (delete-region (point-min) (point))
                 (insert  res))
             ;; else nothing to complete
             (iswitchb-completion-help)
             )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conveniences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable auto-save
(setq auto-save-default 1)
(setq auto-save-interval 250)

;; keybindings...
(if (eq window-system nil)
    (progn
      (keyboard-translate ?\C-h ?\C-?)
      (keyboard-translate ?\C-? ?\C-h)
      (global-unset-key [backspace])
      (global-unset-key [delete])
      (global-set-key [backspace] 'delete-backward-char)
      (global-set-key [delete] 'delete-char)
      (global-set-key [67108927] 'help)))

(global-unset-key "\362")               ;M-r
(global-unset-key "\361")               ;M-q
;; (global-unset-key "\C-xb")
(global-unset-key "\C-x\C-b")
(global-unset-key ";")
(global-unset-key "\C-d")

(global-unset-key "\C-f")
(global-unset-key "\C-b")
(global-unset-key "\346")               ;M-f
(global-unset-key "\342")               ;M-b

(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\346" 'forward-char)   ;M-f
(global-set-key "\342" 'backward-char)  ;M-b

(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'match-paren)
(global-set-key [f5] 'insert-time)
(global-set-key [f8] 'compile)
(global-set-key [f9] 'next-error)
(global-set-key [S-f9] 'previous-error)
(global-set-key [f11] 'my-shrink-window)
(global-set-key [f12] 'my-enlarge-window)
(global-set-key [M-f4] 'save-buffers-kill-emacs)
(global-set-key [C-f4] 'kill-buffer)
(global-set-key [M-f5] 'delete-frame)
(global-set-key [C-f5] 'make-frame-command)
(global-set-key "\356" 'goto-line)      ;M-n
(global-set-key "\362" 'revert-buffer)  ;M-r
(global-set-key "\221" 'fill-paragraph) ;C-M-q
(global-set-key "\361" 'my-reindent)    ;M-q
;; (global-set-key "\C-xb" 's-switch-to-buffer)
(global-set-key "\C-x\C-b" 'my-list-buffers)
(global-set-key ";" 'my-comment)
(global-set-key "\C-c\C-r" 'region-remove-comment)
(global-set-key "\C-c\C-l" 'comment-line)
(global-set-key "\C-d" 'my-delete)

(global-set-key [M-down] 'scroll-up-line) 
(global-set-key [M-up] 'scroll-down-line)

;; To get binding command, do this: First bind the key interactively, 
;; then immediately type "C-x ESC ESC C-a C-k C-g".

(setq compile-command "gmake ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stop debugging messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq debug-on-error nil)

;; Nice Emacs Package
;; (Yen-Ting) Tony Tung
;; $Id: emacs,v 8.34 2003/09/26 23:14:53 tonytung Exp $

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

(if (<= emacs-version-num 20)
    (progn
      (defmacro when (cond &rest body)
        "If COND yields non-nil, do BODY, else return nil."
        (list 'if cond (cons 'progn body)))

      (defmacro unless (cond &rest body)
        "If COND yields nil, do BODY, else return nil."
        (cons 'if (cons cond (cons nil body))))))

;; set up the paths for custom files
(when (eq system-type 'windows-nt)
    (setq exec-path 
          (cons 
           (expand-file-name "~/emacs/bin") 
           exec-path)))
(setq load-path 
      (cons 
       (expand-file-name "~/emacs/elisp") 
       load-path))

(when (eq (string-match "crhc.uiuc.edu" system-name) nil)
      (setq auto-mode-alist 
            (append '(("\\.c\\'" . c++-mode)
                      ("\\.h\\'" . c++-mode))
                    auto-mode-alist)))

(setq auto-mode-alist 
      (append '(("\\.emt\\'" . text-mode)
                ("\\.sched" . text-tab5)
                ("\\.crontab" . text-tab5)
                ("\\.procmailrc" . text-tab5)
                ("cshrc" . shell-script-mode))
              auto-mode-alist))

(setq completion-ignored-extensions
      (append completion-ignored-extensions '(".ps" ".pdf" ".gz")))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
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

(when (and (> emacs-version-num 19.34) (locate-library "psgml"))
  (defvar sgml-data-directory (expand-file-name "~/emacs/etc/sgml"))
  (setq sgml-catalog-files '("~/emacs/etc/sgml/CATALOG" "CATALOG"))
  (setq sgml-ecat-files '("~/emacs/etc/sgml/ECAT" "ECAT"))
  ;;(defvar sgml-trace-entity-lookup t)
  
  ;; initialize psgml
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'html-mode "psgml-html" "Major mode to edit HTML files." t)
  (setq auto-mode-alist (cons 
                         '("\\.html" . html-mode) 
                         auto-mode-alist)))

;; set up generic modes, html-ize, and pc-buffer switch
(when (> emacs-version-num 19.28)
;;   (want 'generic-mode)
;;   (want 'generic-extras)
  (when window-system
    (want 'htmlize))
  (when (want 'pc-bufsw)
    (pc-bufsw::bind-keys [C-tab] [C-S-tab])))

(when window-system
    (if (eq system-type 'windows-nt)
        (when (want 'gnuserv)
            (gnuserv-start))
      (server-start)))

(if (want 'iswitchb)
    (progn
      (iswitchb-default-keybindings)
      (setq iswitchb-default-method 'samewindow) ;always go to the same window
      (setq iswitchb-case t))
  (global-unset-key "\C-xb")
  (global-set-key "\C-xb" 's-switch-to-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing existing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-major-mode 'text-mode)
(setq truncate-partial-width-windows nil)

(defun my-text-mode-hook ()
  (auto-fill-mode)
  (setq fill-column 74)
  (setq indent-tabs-mode 't))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-sgml-mode-hook ()
  (auto-fill-mode))

(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

;;
;; cisco cc-mode settings 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar cisco-c-style
  '((tab-width				. 4)
    (c-basic-offset			. 4) 
    (c-comment-only-line-offset		. 0)
    (c-echo-syntactic-information-p	. nil)
    (c-block-comment-prefix		. "* ")
    (c-recognize-knr-p			. t) ; use nil if only have ANSI prototyp
    (c-tab-always-indent		. t)
    (comment-column			. 36)
    (comment-end			. " */")
    (comment-multi-line			. t)
    (comment-start			. "/* ")
    (c-offsets-alist			. ((knr-argdecl-intro   . +)
                                           (case-label          . 0)
                                           (knr-argdecl         . 0)
                                           (label               . 0)
                                           (statement-case-open . +)
                                           (statement-cont      . +)
                                           (substatement-open   . 0)))
    (c-cleanup-list			. (scope-operator brace-else-brace))
    (fill-column                        . 70))
  "cisco c-style for cc-mode")

(defvar my-c-style
  '("bsd"
    (c-basic-offset			. 2)
    (c-offsets-alist			. ((case-label   . +))))
  "my c style")

(defvar my-java-style
  '("java"
    (c-basic-offset			. 2)
    (c-offsets-alist			. ((substatement-open	.	0))))
  "my java style")

;; c-mode customizations that don't load c-mode nor font-lock mode
(defun my-c-mode-common-hook ()
  (setq comment-column 60)
  (auto-fill-mode)
  (setq fill-column 100)
  (abbrev-mode -1)
  (local-set-key "\C-c\C-w" 'c-wrap-conditional)
  (local-set-key "\C-d" 'my-delete)
  (when (not (fboundp 'my-c-mode-common-hook-done))
    (c-add-style "cisco-c-style" cisco-c-style)
    (c-add-style "my-java-style" my-java-style)
    (c-add-style "my-c-style" my-c-style)
    (font-lock-add-keywords
     'c-mode
     '(("\\<\\(NOTE:\\)" 1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)" 1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)" 1 font-lock-warning-face t)))
    (font-lock-add-keywords
     'c++-mode
     '(("\\<\\(NOTE:\\)" 1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)" 1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)" 1 font-lock-warning-face t)))
    (defvar my-c-mode-common-hook-done t 
      "Indicates that my-c-mode-common-hook has been called"))
  (if (string-match "cisco.com" system-name)
      (c-set-style "cisco-c-style")
    (c-set-style "my-c-style")))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-asm-mode-hook ()
  (setq comment-start "# ")
  (setq asm-comment-char 35))
(add-hook 'asm-mode-hook 'my-asm-mode-hook)

(defun my-tcl-mode-hook ()
  (when (not (fboundp 'my-tcl-mode-common-hook-done))
    (font-lock-add-keywords
     'tcl-mode
     '(("\\<\\(NOTE:\\)" 1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)" 1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)" 1 font-lock-warning-face t)))
    (defvar my-tcl-mode-common-hook-done t
      "Indicates that my-tcl-mode-common-hook has been called")))
(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)

;; set up the java system
(setq auto-mode-alist (cons 
                       '("\\.java" . java-mode) 
                       auto-mode-alist))

(defun my-java-mode-hook ()
  (when (not (fboundp 'my-java-mode-hook-done))
    (font-lock-add-keywords
     'java-mode
     '(("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
       ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
       ("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
    (defvar my-java-mode-hook-done t 
      "Indicates that my-java-mode-hook has been called"))
  (c-set-style "my-java-style"))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; set up the font-lock system
(when (fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       (setq font-lock-maximum-decoration t)
       (setq font-lock-maximum-size nil))

(setq-default indent-tabs-mode nil)
(setq compilation-scroll-output t)

;; shell mode stuff
(defun my-shell-mode-hook ()
  (setq comint-scroll-show-maximum-output t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(add-hook 'comint-output-filter-functions
         'comint-watch-for-password-prompt)
(add-hook 'comint-output-filter-functions
         'comint-postoutput-scroll-to-bottom)

(setq vc-follow-symlinks t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Win32 utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'windows-nt)
  ;; ispell
  ;;
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
        )

  ;; RCS for Win32
  (defadvice vc-do-command (before patch-vc-do-command-for-nt activate)
  "Patch vc-do-command to work under NT, so ediff-revision can work."
    ;; First, let's check for the case we want to patch:
    (if (and (string= (ad-get-arg 2) "/bin/sh")
             (string= (ad-get-arg 5) "-c")
             (string-match (concat "^if \\[ x\"\\$1\" = x \\]; then shift; fi;[ \t]+"
                                   "umask [0-9]+; exec >\"\$1\" || exit;[ \t]+shift; "
                                   "umask 0; exec co \"\\$@\"") (ad-get-arg 6))
             (string= (ad-get-arg 7) ""))
        (progn (ad-set-arg 2 "cmd") ;; Invoke "cmd" instead of "/bin/sh"
               (ad-set-arg 5 "/c") ;; Convert "-c" to "/c"
               (ad-set-arg 6 nil) ;; Zap the /bin/sh script
               (ad-set-arg 7 "co") ;; Invoke the "co" command
               (ad-set-arg 8 (concat ">" (ad-get-arg 8))) ;; Redirect to outfile
               ))
    ;; next, let's check for the CVS case
    (if (and (string= (ad-get-arg 2) "/bin/sh")
             (string= (ad-get-arg 5) "-c")
             (string-match "^exec" (ad-get-arg 6))
             (string= (ad-get-arg 7) ""))
        (progn (ad-set-arg 2 "cmd") ;; Invoke "cmd" instead of "/bin/sh"
               (ad-set-arg 5 "/c") ;; Convert "-c" to "/c"
               (ad-set-arg 6 nil) ;; Zap the /bin/sh script
               (ad-set-arg 7 "cvs update") ;; Invoke the "cvs" command
               (ad-set-arg 8 (concat ">" (ad-get-arg 8))) ;; Redirect to outfile
               )))

  ;; set up the ange-ftp ftp program
  (setq ange-ftp-ftp-program-name "~/emacs/bin/ftp.exe")

  ;; set up the ange-ftp temporary directory
  (setq ange-ftp-tmp-name-template 
        (concat 
         (expand-file-name (getenv "TEMP")) 
         "/ange-ftp"))
  (setq ange-ftp-gateway-tmp-name-template 
        (concat 
         (expand-file-name (getenv "TEMP")) 
         "/ange-ftp")))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIX tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; command-line shell - remove the ^M
(unless (eq system-type 'windows-nt)
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t))


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
(when (eq system-type 'windows-nt)
  ;; italic fonts
  (setq w32-enable-italics t) ; This must be done before font settings!
  ;; use interactive set-face-font followed by describe-face to determine this
  (set-face-font 'italic "-*-Courier New-normal-i-*-*-13-*-*-*-c-*-fontset-standard")
  (set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-13-*-*-*-c-*-fontset-standard"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Basic stuff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color  	.	"white")
                (background-color  	.	"black")
                (cursor-color      	.	"green"))))
      
(setq standard-indent 2)

;; set up matching parentheses
(when (> emacs-version-num 19.30)
    (show-paren-mode 1))

;; set the scroll bar to the right side
(when (= emacs-version-num 20.02)
    (set-scroll-bar-mode 'right 'right))
(when (> emacs-version-num 20.02)
    (set-scroll-bar-mode 'right))

;; show marked area
(transient-mark-mode t)

;; scroll
(setq scroll-step 1)
(setq scroll-conservatively 1)

(setq next-line-add-newlines nil)
(setq require-final-newline 'ask)

(when (and window-system (>= emacs-version-num 21))
  (blink-cursor-mode -1)
;;  (tool-bar-mode -1)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))

(when (eq system-type 'darwin)
  (set-default-font "-apple-monaco-medium-r-normal--10-100-75-75-m-100-mac-roman"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Fonts & Colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill the menu bar when there's no window system
(when (eq window-system nil)
  (menu-bar-mode 0))

;; set up some colors for basic stuff
(set-cursor-color "GREEN")
(set-mouse-color "MAGENTA")

;; to know what face is at cursor, use one of these:
;;   M-x describe-text-at RET
;;   M-x text-properties-at RET
;;   M-x list-text-properties-at RET

;; define some safe try-catch blocks to set faces
(defun try-set-face-foreground (face color)
  (condition-case nil
      (set-face-foreground face color)
    (error nil)))
(defun try-set-face-background (face color)
  (condition-case nil
      (set-face-background face color)
    (error nil)))

;; set up colors not dependent on window system
(try-set-face-background 'highlight "DARKSEAGREEN2")
(try-set-face-background 'modeline "MIDNIGHTBLUE")
(try-set-face-background 'region "NAVY")
(try-set-face-background 'show-paren-match-face "NAVY")
(try-set-face-background 'show-paren-mismatch-face "PURPLE")
(try-set-face-background 'fringe "grey30")

(set-foreground-color "WHITE")
(try-set-face-foreground 'default "WHITE")
(try-set-face-foreground 'highlight "WHITE")
(try-set-face-foreground 'modeline "CYAN")
(try-set-face-foreground 'show-paren-match-face "CYAN")
(try-set-face-foreground 'show-paren-mismatch-face "WHITE")

;; set up colors dependent on window-system
(if window-system
    (progn                              ; with a window system
      (try-set-face-foreground 'region "CYAN")
      (set-background-color "BLACK")
      (try-set-face-background 'default "BLACK"))

  (progn                                ; naked terminal
    (try-set-face-foreground 'region "WHITE")
    (set-background-color "unspecified-bg")
    (try-set-face-background 'default "unspecified-bg")))

(defun my-font-lock-mode-hook ()
  (when (not (fboundp 'my-font-lock-mode-hook-done))

    (try-set-face-foreground 'font-lock-function-name-face "LIGHTSKYBLUE")
    (try-set-face-foreground 'font-lock-keyword-face "LIGHTSTEELBLUE")
    (try-set-face-foreground 'font-lock-string-face "LIGHTSALMON")
    (try-set-face-foreground 'font-lock-variable-name-face "LIGHTGOLDENROD")
    (try-set-face-foreground 'font-lock-builtin-face "VIOLET")
    (try-set-face-foreground 'font-lock-warning-face "RED")
    (if (< emacs-version-num 20.02)
        (try-set-face-foreground 'font-lock-reference-face "CADETBLUE")
      (try-set-face-foreground 'font-lock-constant-face "CADETBLUE"))

    (try-set-face-background 'font-lock-comment-face "unspecified-bg")
    (try-set-face-background 'font-lock-function-name-face "unspecified-bg")
    (try-set-face-background 'font-lock-keyword-face "unspecified-bg")
    (try-set-face-background 'font-lock-string-face "unspecified-bg")
    (try-set-face-background 'font-lock-type-face "unspecified-bg")
    (try-set-face-background 'font-lock-variable-name-face "unspecified-bg")
    (if (< emacs-version-num 20.02)
        (try-set-face-background 'font-lock-reference-face "unspecified-bg")
      (try-set-face-background 'font-lock-constant-face "unspecified-bg"))
  
    (if window-system
        (progn
          (try-set-face-foreground 'font-lock-comment-face "ROSYBROWN2")
          (try-set-face-foreground 'font-lock-type-face "PALEGREEN"))

      (progn
        (try-set-face-foreground 'font-lock-comment-face "RED")
        (try-set-face-foreground 'font-lock-type-face "GREEN")))

    (defvar my-font-lock-mode-hook-done t 
      "Indicates that my-font-lock-mode-hook has been called")))
(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up columns
(when (> emacs-version-num 19.28)
    (column-number-mode 1))

;; give us the ability to edit 8 MB files
(setq line-number-display-limit 8388608)

;; date related stuff
(when window-system
  (setq display-time-day-and-date t)
;;(setq display-time-mail-file t)
  (setq display-time-interval 30)
  (display-time))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Titlebar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (> emacs-version-num 19.28)
  (setq frame-title-format
        (concat invocation-name "@" system-name " - %f"))
  (setq icon-title-format
        (concat invocation-name "@" system-name " - %b")))


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

;; (defun my-reindent (FROM TO)
;;   "Refill the region as a paragraph and reindent."
;;   (interactive "r")
;;   (fill-region-as-paragraph FROM TO)
;;   (if indent-region-function
;;       (funcall indent-region-function FROM TO)
;;     (save-excursion
;;       (goto-char TO)
;;       (setq TO (point-marker))
;;       (goto-char FROM)
;;       (or (bolp) (forward-line 1))
;;       (while (< (point) TO)
;; 	(or (and (bolp) (eolp))
;;             (funcall indent-line-function))
;; 	(forward-line 1))
;;       (move-marker TO nil))))

(defun insert-time (&optional nodashes)
  "Insert the current time.
With a prefix argument, it does not insert the dashes below and above the time."
  (interactive "P")
  (let ((time-string (format-time-string "%a %b %d %H:%M:%S %Z %Y")))
    (unless nodashes
      (insert "\n")
      (insert 
       (make-string 
        (length time-string)
        ?-))
      (insert "\n"))
    (insert time-string)
    (unless nodashes
      (insert "\n")
      (insert 
       (make-string 
        (length time-string)
        ?-))
      (insert "\n\n"))))

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
    (unless (equal (buffer-name) "*Buffer List*")
      (other-window 1)
      (goto-char (point-min)))
    (when (= num-win 1)
        (delete-other-windows))
    (when (= (point) (point-min))
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
  (when (equal string "")
      (setq string "#if "))
  (when (> (mark) (point))
      (exchange-point-and-mark))
  (insert "#endif" " /* " string " */\n")
  (backward-char 7)
  (exchange-point-and-mark)
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

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (buffer-name)))

;; Put a nice version of every visited file's file-name into the
;; variable `nice-buffer-file-name'
;; Original code by Miles Bader <miles@gnu.org>
(defun record-nice-file-name ()
  (defun limit-tree (bn count)
    (let ((splitted (split-string bn "/"))
          (retval "")
          (lc 0)
          (cntr 0))
      (setq lc (safe-length splitted))
      (if (>= count lc)
          (setq count (- lc 1)))
      (setq cntr (- lc (+ count 1)))
      (while (< cntr lc)
        (setq retval (concat retval (nth cntr splitted)))
        (if (>= (- lc cntr) 2)
            (setq retval (concat retval "/")))
        (setq cntr (+ 1 cntr)))
      retval))
    
  (if (eq system-type 'windows-nt)
      (defun get-unique-tag (bfn bn)
        (if (or (string-match "<[0-9]+>\\'" bn)
                (not (compare-strings bn 0 nil (file-name-nondirectory bfn) 0 nil 't)))
            (substring bn (match-beginning 0))))
      (defun get-unique-tag (bfn bn)
        (if (or (string-match "<[0-9]+>\\'" bn)
                (not (string= bn (file-name-nondirectory bfn))))
            (substring bn (match-beginning 0)))))

  (let ((shortened-file-name 
         (limit-tree (abbreviate-file-name buffer-file-name) 1))
        (buffer-num 
         (get-unique-tag buffer-file-name (buffer-name))))
    (set (make-local-variable 'nice-buffer-file-name)
         (concat shortened-file-name buffer-num)))
  nil)

;; Make the mode-line identify buffers using their `nice-buffer-file-name'
;; if such a variable exists, otherwise, the buffer's file name, if it
;; has one, otherwise the buffer name.
(when (>= emacs-version-num 20)
  (add-hook 'find-file-hooks 'record-nice-file-name)
  (add-hook 'write-file-hooks 'record-nice-file-name)
  (setq-default mode-line-buffer-identification
                '(nice-buffer-file-name nice-buffer-file-name
                                        (buffer-file-name "%f" "%b"))))
  
(unless window-system
  (setq mode-line-frame-identification '("  ")))

(defun text-tab5 ()
  (interactive)
  (setq tab-width 5)
  (auto-fill-mode)
  (local-set-key "	" (quote self-insert-command)))


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
	  
;; 	  ((= 1 (length iswitchb-matches))
;; 	   ;; only one choice, so select it.
;; 	   (exit-minibuffer))
	                                ;
	  (t
	   ;; else there could be some completions
	   (setq res iswitchb-common-match-string)
	   (if (and (not (memq res '(t nil)))
		    (not (equal res iswitchb-text)))
	       ;; found something to complete, so put it in the minibuffer.
	       (progn
		 (setq iswitchb-rescan nil)
		 (delete-region (minibuffer-prompt-end) (point))
		 (insert  res))
	     ;; else nothing to complete
	     (iswitchb-completion-help)
	     )))))

;; hack so that the "Mail" in the modeline shows only when the file was modified more recently than it was accessed
(when window-system
  (defun display-time-file-nonempty-p (file)
    (and (file-exists-p file)
         (let* ((fa (file-attributes (file-chase-links file)))
                (last-access (nth 4 fa))
                (last-modified (nth 5 fa))
                (last-access-hi (nth 0 last-access))
                (last-access-lo (nth 1 last-access))
                (last-modified-hi (nth 0 last-modified))
                (last-modified-lo (nth 1 last-modified)))
           (or (< last-access-hi last-modified-hi)
               (and (= last-access-hi last-modified-hi)
                    (< last-access-lo last-modified-lo))))))
  (display-time-update))

;; (defun mac-handle-scroll-bar-event (event)
;;   "Handle scroll bar EVENT on Mac OS."
;;   (interactive "e")
;;   (let* ((position (event-start event))
;;          (window (nth 0 position))
;;          (bar-part (nth 4 position)))
;;     (select-window window)
;;     (cond
;;      ((eq bar-part 'up)
;;       (read-event)                      ; discard mouse-1
;;       (scroll-down 1))
;;      ((eq bar-part 'above-handle)
;;       (read-event)
;;       (scroll-down))
;;      ((eq bar-part 'handle)
;;       (read-event)
;;       (scroll-bar-drag event))
;;      ((eq bar-part 'below-handle)
;;       (read-event)
;;       (scroll-up))
;;      ((eq bar-part 'down)
;;       (read-event)
;;       (scroll-up 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conveniences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable auto-save
(setq auto-save-default 1)
(setq auto-save-interval 250)

;; keybindings...
(if (eq window-system nil)
    (progn
      (global-set-key [delete] 'my-delete)
      (global-set-key [insertchar] 'overwrite-mode))
  (if (> emacs-version-num 21)
      (normal-erase-is-backspace-mode)))

(define-key function-key-map [delete] [deletechar])
(when (and (eq system-type 'darwin) (not window-system))
  (define-key function-key-map "OP" [f1])
  (define-key function-key-map "OQ" [f2])
  (define-key function-key-map "OR" [f3])
  (define-key function-key-map "OS" [f4])
  (define-key function-key-map "[28~" [insert])
  (define-key function-key-map "[1~" [home])
  (define-key function-key-map "[4~" [end]))

(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\346" 'forward-char)   ;M-f
(global-set-key "\342" 'backward-char)  ;M-b

(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'match-paren)
(global-set-key [f5] 'insert-time)
(global-set-key [f7] 'grep)
(global-set-key [f8] 'compile)
(global-set-key [f9] 'next-error)
(global-set-key [S-f9] 'previous-error)
(global-set-key [f11] 'my-shrink-window)
(global-set-key [f12] 'my-enlarge-window)
(global-set-key [M-f4] 'save-buffers-kill-emacs)
(global-set-key [C-f4] 'kill-current-buffer)
(global-set-key [M-f5] 'delete-frame)
(global-set-key [C-f5] 'make-frame-command)
(global-set-key "l" 'goto-line)       ;M-l
(global-set-key "\362" 'revert-buffer)  ;M-r
;;(global-set-key "\361" 'my-reindent)    ;M-q
;;(global-set-key "\C-xb" 's-switch-to-buffer)
(global-set-key "\C-x\C-b" 'my-list-buffers)
(global-set-key ";" 'my-comment)
(if (>= emacs-version-num 21)
    (global-set-key "\C-c\C-r" 'uncomment-region)
  (global-set-key "\C-c\C-r" 'region-remove-comment))
(global-set-key "\C-c\C-l" 'comment-line)
(global-set-key "\C-d" 'my-delete)

(global-set-key [M-down] 'scroll-up-line) 
(global-set-key [M-up] 'scroll-down-line)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(global-set-key (quote [27 deletechar]) 'kill-word)

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
      
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
      
      
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

(when (locate-library "eshell")
  (global-set-key "!" 'eshell-command)
  (setq eshell-prefer-to-shell t))

(when (eq system-type 'darwin)
  (setq mac-wheel-button-is-mouse-2 't))

;; To get binding command, do this: First bind the key interactively, 
;; then immediately type "C-x ESC ESC C-a C-k C-g".

(setq compile-command "make ")
(when (string-match "cisco.com" system-name)
  (setq gud-gdb-command-name "/auto/macedon_tools/sde4/bin/sde-gdb /vob/ace/plat-zamboni/mcpu/Images/asiram"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stop debugging messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq debug-on-error nil)

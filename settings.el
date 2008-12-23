;; Nice Emacs Package
;; (Yen-Ting) Tony Tung
;; $Id: emacs,v 10.2 2006/02/08 20:23:32 tonytung Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start debugging messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)


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
;; Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq safe-local-variable-values '((save-buffer-coding-system . undecided-unix)))

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
(add-to-list 'exec-path
             (reduce 'path-join '("emacs" "bin") :initial-value home-directory))
(add-to-list 'exec-path
             (reduce 'path-join '("software" "bin") :initial-value home-directory))
(add-to-list 'exec-path
             (reduce 'path-join '("opt" "local" "bin") :initial-value "/"))
(setenv "PATH" (format "%s%s%s"
                (reduce 'path-join '("opt" "local" "bin") :initial-value "/")
                path-separator
                (getenv "PATH")))
(add-to-list 'load-path
             (reduce 'path-join '("emacs" "elisp") :initial-value home-directory))

(add-to-list 'auto-mode-alist '("\\.c\\'"		. c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"		. c++-mode))
(add-to-list 'auto-mode-alist '("\\.emt\\'"		. text-mode))
(add-to-list 'auto-mode-alist '("\\.sched"		. text-tab5))
(add-to-list 'auto-mode-alist '("\\.crontab\\'"		. text-tab5))
(add-to-list 'auto-mode-alist '("\\.procmailrc\\'"	. text-tab5))
(add-to-list 'auto-mode-alist '("\\..*cshrc"		. shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.t?csh\\'"		. shell-script-mode))
(add-to-list 'auto-mode-alist '("emacs/emacs"		. emacs-lisp-mode))

(add-to-list 'completion-ignored-extensions ".ps")
(add-to-list 'completion-ignored-extensions ".pdf")
(add-to-list 'completion-ignored-extensions ".gz")
(add-to-list 'completion-ignored-extensions ".pyc")

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up the paths for psgml
(add-to-list 'load-path
             (reduce 'path-join '("emacs" "elisp" "psgml") :initial-value home-directory))

(when (and (> emacs-version-num 19.34) (locate-library "psgml"))
  (defvar sgml-data-directory (expand-file-name "~/emacs/etc/sgml"))
  (setq sgml-catalog-files '("~/emacs/etc/sgml/CATALOG" "CATALOG"))
  (setq sgml-ecat-files '("~/emacs/etc/sgml/ECAT" "ECAT"))
  ;;(defvar sgml-trace-entity-lookup t)
  (setq sgml-warn-about-undefined-entities nil)

  ;; initialize psgml
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'html-mode "psgml-html" "Major mode to edit HTML files." t)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
  (setq psgml-html-htmldtd-version "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"))

;; set up html-ize
(when (and (> emacs-version-num 19.28) window-system)
  (want 'htmlize))

(when (want 'iswitchb)
  (iswitchb-default-keybindings)
  (setq iswitchb-default-method 'samewindow) ;always go to the same window
  (setq iswitchb-case t))

(when (>= emacs-version-num 20.0)
  (setq cscope-bind-default-keys nil) ;make sure cscope doesn't bind the default keys
  (defun my-xcscope-setup ()
    (unless (boundp 'my-xcscope-setup-done)
      (setq cscope-database-regexps
            '(
              ( ".*"
                ( t ("-dq") ) )))
      (setq cscope-edit-single-match nil)
      (setq cscope-auto-open-buffer nil)
      (my-xcscope-bind-keys cscope:map)
      (my-xcscope-bind-keys cscope-list-entry-keymap)
      (defvar my-xcscope-setup-done 't "t if my-xcscope-setup has already been executed")))

  (defun my-xcscope-bind-keys (map)
    (define-key map "\C-cs" 'cscope-find-this-symbol)
    (define-key map "\C-cd" 'cscope-find-global-definition)
    (define-key map "\C-cg" 'cscope-find-global-definition)
    (define-key map "\C-cG" 'cscope-find-global-definition-no-prompting)
    (define-key map "\C-cc" 'cscope-find-functions-calling-this-function)
    (define-key map "\C-cC" 'cscope-find-called-functions)
    (define-key map "\C-ct" 'cscope-find-this-text-string)
    (define-key map "\C-ce" 'cscope-find-egrep-pattern)
    (define-key map "\C-cf" 'cscope-find-this-file)
    (define-key map "\C-ci" 'cscope-find-files-including-file)))

;; PHP Mode
(when (locate-library "php-mode")
  (autoload 'php-mode "php-mode" "Mode for editing PHP files" t)
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (font-lock-add-keywords
   'php-mode
   '(("\\<\\(NOTE:\\)"	1 font-lock-warning-face t)
     ("\\<\\(TODO:\\)"	1 font-lock-warning-face t)
     ("\\<\\(FIXME:\\)"	1 font-lock-warning-face t))))

;; CSS Mode
(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (defun my-css-mode-hook ()
    (setq css-tab-mode 'indent))
  (add-hook 'css-mode-hook 'my-css-mode-hook))

;; javascript mode
(when (locate-library "javascript-mode")
  (autoload 'javascript-mode "javascript-mode" "Mode for editing Javascript files" t)
  (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)))


;; mmm mode
;; set up the paths for multiple major modes
;; (add-to-list 'load-path (expand-file-name "~/emacs/elisp/mmm-mode") t)
;; (when (want 'mmm-auto)
;;   (setq mmm-global-mode 'maybe)
;;   (setq mmm-submode-decoration-level 1)
;;   (set-face-background 'mmm-default-submode-face "BLACK")

;;   ;; set up an mmm group for fancy html editing
;;   (mmm-add-group
;;    'fancy-html
;;    '(
;;      (html-javascript-embedded
;;       :submode java-mode
;;       :face mmm-code-submode-face
;;       :front "<script\[^>\]*>"
;;       :back "</script>")
;;      (html-css-embedded
;;       :submode css-mode
;;       :face mmm-code-submode-face
;;       :front "<style\[^>\]*>"
;;       :back "</style>")
;;      (html-php-tagged
;;       :submode php-mode
;;       :face mmm-code-submode-face
;;       :front "<[?]\\(php\\|=\\)?"
;;       :back "[?]>")
;;      (html-css-attribute
;;       :submode css-mode
;;       :face mmm-declaration-submode-face
;;       :front "style=\""
;;       :back "\"")
;;      (html-javascript-attribute
;;       :submode java-mode
;;       :face mmm-code-submode-face
;;       :front "\\bon\\w+=\\s-*\""
;;       :back "\"")
;;      ))

;;   (add-to-list 'mmm-mode-ext-classes-alist '(html-mode "\\.php[34]?\\'" fancy-html)))

;; initialize DVC
(load-file (reduce 'path-join '("emacs" "elisp" "dvc-load.el") :initial-value home-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing existing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-major-mode 'text-mode)
(setq truncate-partial-width-windows nil)

(defun my-text-mode-hook ()
  (auto-fill-mode)
  (setq fill-column 74))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-sgml-mode-hook ()
  (auto-fill-mode))
(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

(defvar vmware-c-style
  '("linux"
    (c-basic-offset                     . 3)
    (c-offsets-alist			. ((case-label   	. +)))
    (comment-column                     . 40)
    (comment-end			. " */")
    (comment-multi-line			. t)
    (comment-start			. "/* ")
    (fill-column                        . 80)))

(defvar facebook-c-style
  '("linux"
    (c-basic-offset                     . 2)
    (c-offsets-alist			. ((case-label   	. +)))
    (comment-column                     . 40)
    (comment-end			. " */")
    (comment-multi-line			. t)
    (comment-start			. "/* ")
    (fill-column                        . 80)))

(defvar my-c-style
  '("linux"
    (c-basic-offset			. 2)
    (c-offsets-alist			. ((case-label   	. +))))
  "my c style")

(defvar my-java-style
  '("java"
    (c-basic-offset			. 2)
    (c-offsets-alist			. ((substatement-open	.	0)))
    (c-hanging-braces-alist             . ((brace-list-open)
                                           (brace-entry-open)))
     )
  "my java style")


;; c-mode customizations that don't load c-mode nor font-lock mode
(defun my-c-mode-common-hook ()
  (setq comment-column 50)
  (auto-fill-mode)
  (setq fill-column 100)
  (abbrev-mode -1)
  (local-set-key "\C-c\C-x\C-w"		'c-wrap-conditional)
  (local-set-key "\C-d"			'my-delete)
  (local-set-key "\C-c>" 		'search-for-matching-endif)
  (local-set-key "\C-c<" 		'search-for-matching-ifdef)
  (when (not (boundp 'my-c-mode-common-hook-done))
    (when (and (> emacs-version-num 20.0) (want 'xcscope))
      (defvar xcscope-loaded 't "t if xcscope is loaded"))
;;       (when (string-match "pasemi\\.com" system-name)
;;         (load "xcscope")
;;         (define-key cscope-list-entry-keymap (kbd "RET") 'cscope-select-entry-other-window)))
    (c-add-style "my-java-style"	my-java-style)
    (c-add-style "my-c-style"		my-c-style)
    (c-add-style "facebook-c-style"	facebook-c-style)
    (c-add-style "vmware-c-style"	vmware-c-style)
    (font-lock-add-keywords
     'c-mode
     '(("\\<\\(NOTE:\\)"	1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)"	1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)"	1 font-lock-warning-face t)))
    (font-lock-add-keywords
     'c++-mode
     '(("\\<\\(NOTE:\\)"	1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)"	1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)"	1 font-lock-warning-face t)))
    (defvar my-c-mode-common-hook-done t
      "Indicates that my-c-mode-common-hook has been called"))
  (cond ((and buffer-file-name (string-match "/elinks/src" buffer-file-name))
         (setq tab-width 2)
         (c-set-style "my-c-style"))
        ((or (not buffer-file-name) (string-match "libtransmission" buffer-file-name))
         (c-set-style "my-c-style")
         (setq c-basic-offset 4))
        ((or (string-match "facebook\\.com" system-name)
             (string-match "Tony-Tung\\.local" system-name))
         (c-set-style "facebook-c-style")
         (when (and buffer-file-name (string-match "/mcproxy.*/" buffer-file-name))
           (setq c-basic-offset 8)
           (setq indent-tabs-mode t)))
        ((string-match "CHRISTINEWU" system-name)
         (c-set-style "vmware-c-style"))
        ('t
         (c-set-style "my-c-style")))
  (when (and (boundp 'xcscope-loaded) xcscope-loaded)
    (my-xcscope-setup)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-asm-mode-hook ()
  (setq comment-start "# ")
  (setq asm-comment-char 35))
(add-hook 'asm-mode-hook 'my-asm-mode-hook)

;; set up the java system
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(defun my-java-mode-hook ()
  (local-set-key "\C-d"			'my-delete)
  (when (not (fboundp 'my-java-mode-hook-done))
    (font-lock-add-keywords
     'java-mode
     '(("\\<\\(NOTE\\):"	1 font-lock-warning-face t)
       ("\\<\\(TODO\\):"	1 font-lock-warning-face t)
       ("\\<\\(FIXME\\):"	1 font-lock-warning-face t)))
    (defvar my-java-mode-hook-done t
      "Indicates that my-java-mode-hook has been called"))
  (c-set-style "my-java-style"))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; javascript-mode customizations
(defun my-javascript-mode-hook ()
  (setq comment-column 70)
  (auto-fill-mode)
  (setq fill-column 100)
  (abbrev-mode -1)
  (local-set-key "\C-d"			'my-delete)
  (when (not (fboundp 'my-javascript-mode-hook-done))
    (font-lock-add-keywords
     'javascript-mode
     '(("\\<\\(NOTE:\\)"	1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)"	1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)"	1 font-lock-warning-face t)))
    (defvar my-javascript-mode-hook-done t
      "Indicates that my-javascript-mode-hook has been called")))
(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)

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
(setq vc-handled-backends (remove* 'Mtn vc-handled-backends))

;; python mode stuff
(when (and (> emacs-version-num 19.34) (locate-library "python-mode"))
  (defun my-python-mode-hook ()
    (auto-fill-mode 't)
    (setq fill-column 90)
    (font-lock-add-keywords
     'python-mode
     '(("\\<\\(NOTE:\\)"	1 font-lock-warning-face t)
       ("\\<\\(TODO:\\)"	1 font-lock-warning-face t)
       ("\\<\\(FIXME:\\)"	1 font-lock-warning-face t))))

  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (autoload 'python-mode "python-mode" "Major mode to edit Python files." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;; support for cvs over ssh
(setenv "CVS_RSH" "ssh")
(let ((ssh-agent-sock (expand-file-name "~/.ssh/agent-sock")))
  (when (file-exists-p ssh-agent-sock)
    (setenv "SSH_AUTH_SOCK" ssh-agent-sock)))


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
  (set-face-font 'italic	"-*-Courier New-normal-i-*-*-13-*-*-*-c-*-fontset-standard")
  (set-face-font 'bold-italic	"-*-Courier New-bold-i-*-*-13-*-*-*-c-*-fontset-standard"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Basic stuff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(foreground-color  	.	"white"))
(add-to-list 'default-frame-alist '(background-color  	.	"black"))
(add-to-list 'default-frame-alist '(cursor-color  	.	"green"))

(setq standard-indent 2)

;; set up matching parentheses
(when (> emacs-version-num 19.30)
    (show-paren-mode 1))

;; set the scroll bar to the right side
(when window-system
  (when (= emacs-version-num 20.02)
    (set-scroll-bar-mode 'right 'right))
  (when (> emacs-version-num 20.02)
    (set-scroll-bar-mode 'right)))

;; show marked area
(transient-mark-mode t)

;; scroll
(setq scroll-step 1)
(setq scroll-conservatively 1)

(setq next-line-add-newlines nil)
(setq require-final-newline 'ask)

(when (and window-system (>= emacs-version-num 21))
  (blink-cursor-mode -1)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (when (want 'tabbar)
    (tabbar-mode)
    (global-set-key [M-S-left] 'tabbar-backward)
    (global-set-key [M-S-right] 'tabbar-forward)

    (defun my-tabbar-buffer-groups (buffer)
      "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
      (with-current-buffer (get-buffer buffer)
        (cond
         ((or (get-buffer-process (current-buffer))
              (memq major-mode
                    '(comint-mode compilation-mode)))
          '("process")
          )
         ((member (buffer-name)
                  '("*scratch*" "*Messages*" "*Completions*" "*Buffer List*"))
          '("misc")
          )
         ((eq major-mode 'dired-mode)
          '("dired")
          )
         ((memq major-mode
                '(fundamental-mode help-mode apropos-mode Info-mode Man-mode gdb-breakpoints-mode))
          '("misc")
          )
         ((memq major-mode
                '(tex-mode latex-mode text-mode xml-mode))
          '("main")
          )
         (t
          '("main")
          )
         )))
    (setq tabbar-cycling-scope 'tabs)
    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
    (set-face-attribute 'tabbar-default-face nil
                        :inherit 'default
                        :height 0.9
                        :foreground "gray60"
                        :background "gray72")
    (set-face-attribute 'tabbar-selected-face nil
                        :foreground "blue"
                        :box '(:line-width 2 :color "white" :style pressed-button))
    (set-face-attribute 'tabbar-unselected-face nil
                        :foreground "midnightblue"
                        :box '(:line-width 2 :color "white" :style released-button))
    ))

;; kill the menu bar when there's no window system
(unless window-system
  (menu-bar-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Fonts & Colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up some colors for basic stuff
(set-cursor-color						"GREEN")
(set-mouse-color						"MAGENTA")

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
(try-set-face-background 'highlight				"DARKSEAGREEN2")
(try-set-face-background 'modeline				"MIDNIGHTBLUE")
(try-set-face-background 'region				"NAVY")
(try-set-face-background 'show-paren-match-face			"NAVY")
(try-set-face-background 'show-paren-mismatch-face		"PURPLE")
(try-set-face-background 'fringe				"grey30")

(set-foreground-color 						"WHITE")
(try-set-face-foreground 'default				"WHITE")
(try-set-face-foreground 'highlight				"WHITE")
(try-set-face-foreground 'modeline				"CYAN")
(try-set-face-foreground 'show-paren-match-face			"CYAN")
(try-set-face-foreground 'show-paren-mismatch-face		"WHITE")
(try-set-face-background 'trailing-whitespace "#900000")

;; set up colors dependent on window-system
(if window-system
    (progn                              ; with a window system
      (try-set-face-foreground 'region				"CYAN")
      (set-background-color					"BLACK")
      (try-set-face-background 'default				"BLACK"))

  (progn                                ; naked terminal
    (try-set-face-foreground 'region				"WHITE")
    (set-background-color					"unspecified-bg")
    (try-set-face-background 'default				"unspecified-bg")))

(defun my-window-system-font-lock-mode-hook ()
  (when (not (fboundp 'my-window-system-font-lock-mode-hook-done))

    (try-set-face-foreground 'font-lock-function-name-face	"LIGHTSKYBLUE")
    (try-set-face-foreground 'font-lock-keyword-face		"LIGHTSTEELBLUE")
    (try-set-face-foreground 'font-lock-string-face		"LIGHTSALMON")
    (try-set-face-foreground 'font-lock-variable-name-face	"LIGHTGOLDENROD")
    (try-set-face-foreground 'font-lock-builtin-face		"VIOLET")
    (try-set-face-foreground 'font-lock-warning-face		"RED")
    (try-set-face-foreground 'font-lock-comment-face		"ROSYBROWN2")
    (try-set-face-foreground 'font-lock-type-face		"PALEGREEN")
    (if (< emacs-version-num 20.02)
        (try-set-face-foreground 'font-lock-reference-face	"CADETBLUE")
      (try-set-face-foreground 'font-lock-constant-face		"CADETBLUE"))

  (defvar my-window-system-font-lock-mode-hook-done t
    "Indicates that my-window-system-font-lock-mode-hook has been called")))

(defun my-console-font-lock-mode-hook ()
  (when (not (fboundp 'my-console-font-lock-mode-hook-done))

    (try-set-face-foreground 'font-lock-function-name-face	"LIGHTSKYBLUE")
    (try-set-face-foreground 'font-lock-keyword-face		"LIGHTSTEELBLUE")
    (try-set-face-foreground 'font-lock-string-face		"LIGHTSALMON")
    (try-set-face-foreground 'font-lock-variable-name-face	"LIGHTGOLDENROD")
    (try-set-face-foreground 'font-lock-builtin-face		"VIOLET")
    (try-set-face-foreground 'font-lock-warning-face		"RED")
    (if (< emacs-version-num 20.02)
        (try-set-face-foreground 'font-lock-reference-face	"CADETBLUE")
      (try-set-face-foreground 'font-lock-constant-face		"CADETBLUE"))
    (try-set-face-foreground 'diff-context-face                 "WHITE")
    (try-set-face-foreground 'cscope-line-face                  "LIGHTGOLDENROD")

    (try-set-face-background 'font-lock-comment-face		"unspecified-bg")
    (try-set-face-background 'font-lock-function-name-face	"unspecified-bg")
    (try-set-face-background 'font-lock-keyword-face		"unspecified-bg")
    (try-set-face-background 'font-lock-string-face		"unspecified-bg")
    (try-set-face-background 'font-lock-type-face		"unspecified-bg")
    (try-set-face-background 'font-lock-variable-name-face	"unspecified-bg")
    (try-set-face-foreground 'font-lock-comment-face		"RED")
    (try-set-face-foreground 'font-lock-type-face		"GREEN")
    (if (< emacs-version-num 20.02)
        (try-set-face-background 'font-lock-reference-face	"unspecified-bg")
      (try-set-face-background 'font-lock-constant-face		"unspecified-bg"))

    (try-set-face-foreground 'ediff-fine-diff-A                 "BLUE")
    (try-set-face-foreground 'ediff-fine-diff-B                 "BLUE")

    (defvar my-console-font-lock-mode-hook-done t
      "Indicates that my-console-font-lock-mode-hook has been called")))

(if window-system
    (add-hook 'font-lock-mode-hook 'my-window-system-font-lock-mode-hook)
  (add-hook 'font-lock-mode-hook 'my-console-font-lock-mode-hook))

(if (and window-system (eq system-type 'darwin))
    (progn
      (set-keyboard-coding-system 'mac-roman)

      (create-fontset-from-fontset-spec
       "-apple-monaco-medium-r-normal--10-*-*-*-*-*-fontset-mac,
  ascii:-apple-monaco-medium-r-normal--10-*-*-*-m-*-mac-roman,
  latin-iso88510-1:-apple-monaco-medium-r-normal--10-*-*-*-m-*-mac-roman,
  mule-unicode-0100-24ff:-apple-monaco-medium-r-normal--10-*-*-*-m-*-mac-roman")
      (set-frame-font "fontset-mac" 'keep)
      (add-to-list 'default-frame-alist
                   '(font . "fontset-mac"))
      (setq mac-allow-anti-aliasing nil))
  (add-to-list 'default-frame-alist '(font . "-schumacher-clean-medium-r-normal--12-*")))

(defun maybe-delete-trailing-whitespace ()
  (when (and
         buffer-file-name
         (string-match "/Users/tonytung/work/" buffer-file-name)
         (not (or
               (string-match "/viewmtn/" buffer-file-name)
               (eq 'diff-mode major-mode))))
    (delete-trailing-whitespace)))
(when (fboundp 'delete-trailing-whitespace)
  (when (or (string-match "facebook\\.com" system-name)
            (string-match "Tony-Tung\\.local" system-name))
    (setq-default show-trailing-whitespace t)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace))
  (when (string-match "fourier\\.local" system-name)
    (add-hook 'write-file-hooks 'maybe-delete-trailing-whitespace)))

(add-hook 'find-file-hooks (lambda () "enable trailing whitespace"
                             (setq show-trailing-whitespace t)))


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
  (setq display-time-interval 30)
  (setq display-time-mail-file t)
  (display-time))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance (Titlebar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (> emacs-version-num 19.28)
  (setq start (string-match "\\." system-name))
  (setq tag (concat invocation-name "@" (substring system-name 0 start)))

  (setq frame-title-format
        (concat tag " - %f"))
  (setq icon-title-format
        (concat tag " - %b")))


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
  "Comments a region if the mark is active.  Otherwise starts a comment."
  (interactive)
  (if mark-active
      (comment-region (point) (mark) nil)
    (indent-for-comment)))

(defun region-remove-comment (from to)
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

(defun my-comint-delete (&optional arg)
  "Deletes the region if the mark is active.  Otherwise runs comint-delchar-or-maybe-eof."
  (interactive "P")
  (if mark-active
      (delete-region (point) (mark))
    (if (eq arg nil)
        (comint-delchar-or-maybe-eof 1)             ;no prefix
      (comint-delchar-or-maybe-eof arg))))

(eval-after-load "shell"
  '(define-key shell-mode-map "\C-d" 'my-comint-delete))

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

(setq-default mode-line-mule-info '(""))
(setq-default mode-line-frame-identification '("  "))

(defun text-tab5 ()
  (interactive)
  (setq tab-width 5)
  (auto-fill-mode)
  (local-set-key "	" (quote self-insert-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Borrowed" functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Go to matching parentheses
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((looking-at "\\s\{") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\}") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;; move cursor to the matching `#ifdef' or `#if'
;;;
(defun search-for-matching-ifdef ()
  "Move cursor to the matching `#if' or `#endif'"
  (interactive)
  (let ((m (point-marker))              ; marker for the original position
        (m2)                            ; marker for loop
        (depth 0))                      ; depth of #ifdef/#endif nesting
    (forward-line)
    (beginning-of-line)
    (if (not (re-search-backward "^#\\s-*endif" nil t))
        (progn
          (goto-char (marker-position m))
          (error "%s" "no #endif")))
    (setq m (point-marker))
    (forward-line)
    (beginning-of-line)
    (if (catch 'loop
          (while (> (point) (point-min))
            (if (not (re-search-backward "^#\\s-*" nil t))
                (throw 'loop nil))
            (backward-char 1)
            (setq m2 (point-marker))
            (cond ((search-forward "endif" (+ (point) 7) t)
                     (setq depth (1+ depth)))
                  ((search-forward "if" (+ (point) 4) t)
                   (progn
                     (setq depth (1- depth))
                     (if (= depth 0)
                         (throw 'loop t)))))
            (goto-char (marker-position m2))))
        (push-mark (marker-position m)) ; t
        (progn                          ; nil
          (goto-char (marker-position m))
          (error "no matching #ifdef, #if")))))


;;; move cursor to the matching `#endif'
;;;
(defun search-for-matching-endif ()
  "Move cursor to the matching `#endif'"
  (interactive)
  (let ((m)                             ;marker
        (depth 0))                      ;depth of #ifdef/#endif nesting
    (beginning-of-line)
    (if (not (re-search-forward "^#\\s-*if" nil t))
        (error "%s" "no #ifdef, #if"))
    (beginning-of-line)
    (setq m (point-marker))
    (if (catch 'loop
          (while (< (point) (point-max))
            (if (not (re-search-forward "^#\\s-*" nil t))
                (throw 'loop nil))
            (cond ((search-forward "if" (+ (point) 2) t)
                   (setq depth (1+ depth)))
                  ((search-forward "endif" (+ (point) 5) t)
                   (progn
                     (setq depth (1- depth))
                     (if (= depth 0)
                         (throw 'loop t)))))))
        (push-mark (marker-position m)) ; t
        (progn                          ; nil
          (goto-char (marker-position m))
          (error "no matching #endif")))))

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

;; if the scratch buffer is not in the buflist, then manually add it in.
(defun my-iswitchb-make-buflist-hook ()
  (unless (member "*scratch*" iswitchb-temp-buflist)
    (setq iswitchb-temp-buflist (append iswitchb-temp-buflist (list "*scratch*")))))
(add-hook 'iswitchb-make-buflist-hook 'my-iswitchb-make-buflist-hook)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conveniences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable auto-save
(unless (or (string-match "CHRISTINEWU" system-name)
            (not (string= init-file-user user-login-name)))
  (let ((dir (expand-file-name "~/.emacs.d/auto-saves/")))
    (when (or (file-exists-p dir)
              (not (condition-case nil
                       (make-directory dir 't)
                     (error t))))
      (setq auto-save-default 1)
      (setq auto-save-interval 250)
      (setq auto-save-file-name-transforms '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
                                             ("\\`\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.d/auto-saves/\\2" t))))))

;; keybindings...
(if (eq window-system nil)
    (progn
      (global-set-key [delete] 'my-delete)
      (global-set-key [insertchar] 'overwrite-mode))
  (if (> emacs-version-num 21)
      (normal-erase-is-backspace-mode)))

(defun keymap-setup ()
  (define-key function-key-map [delete] [deletechar])
  (when (and (string-match "\\(darwin\\|usg-unix-v\\)" (symbol-name system-type)) (not window-system))
    (define-key function-key-map "OP"		[f1])
    (define-key function-key-map "OQ"		[f2])
    (define-key function-key-map "OR"		[f3])
    (define-key function-key-map "OS"		[f4])
    (define-key function-key-map "[28~"	[insert])
    (define-key function-key-map "[1~"	[home])
    (define-key function-key-map "[4~"	[end])))
(setq term-setup-hook 'keymap-setup)

(global-set-key "\C-f"         'forward-word)
(global-set-key "\C-b"         'backward-word)
(global-set-key "\346"         'forward-char)   ;M-f
(global-set-key "\342"         'backward-char)  ;M-b

(global-set-key [f2]		'save-buffer)
(global-set-key [f3]		'find-file)
(global-set-key [f4]		'match-paren)
(global-set-key [f5]		'insert-time)
(global-set-key [f7]		'grep)
(global-set-key [f8]		'compile)
(global-set-key [f9]		'next-error)
(global-set-key [S-f9]		'previous-error)
(global-set-key [f11]		'my-shrink-window)
(global-set-key [f12]		'my-enlarge-window)
(global-set-key [M-f4]		'save-buffers-kill-emacs)
(global-set-key [C-f4]		'kill-current-buffer)
(global-set-key [M-f5]		'delete-frame)
(global-set-key [C-f5]		'make-frame-command)
(global-set-key "l"		'goto-line)       ;M-l
(global-set-key "\362"		'revert-buffer)  ;M-r
(global-set-key "\C-x\C-b"	'my-list-buffers)
(global-set-key ";"		'my-comment)
(if (>= emacs-version-num 21)
    (global-set-key "\C-c\C-r"	'uncomment-region)
  (global-set-key "\C-c\C-r"	'region-remove-comment))
(global-set-key "\C-c\C-l"	'comment-line)
(global-set-key "\C-d"		'my-delete)

(global-set-key [M-up]		'scroll-down-line)
(global-set-key [M-down]	'scroll-up-line)
(global-set-key [27 up]         'scroll-down-line)
(global-set-key [27 down]       'scroll-up-line)

(global-set-key [home]		'beginning-of-buffer)
(global-set-key [end]		'end-of-buffer)

(global-set-key [27 deletechar]	'kill-word)

(defun up-slightly   () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4]	'down-slightly)
(global-set-key [mouse-5]	'up-slightly)
(global-set-key [wheel-down]	'up-slightly)
(global-set-key [wheel-up]	'down-slightly)

(defun up-one   () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4]	'down-one)
(global-set-key [S-mouse-5]	'up-one)


(defun up-a-lot   () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4]	'down-a-lot)
(global-set-key [C-mouse-5]	'up-a-lot)


(setq mouse-yank-at-point 't)
(when (eq system-type 'darwin)
  (setq mac-wheel-button-is-mouse-2 't))

(when (locate-library "eshell")
  (global-set-key "!" 'eshell-command)
  (setq eshell-prefer-to-shell t))

;; To get binding command, do this: First bind the key interactively,
;; then immediately type "C-x ESC ESC C-a C-k C-g".


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stop debugging messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error nil)
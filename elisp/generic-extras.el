;;; generic-extras.el --- Extra Modes for generic-mode
;;
;; Author:  Peter Breton <pbreton@i-kinetics.com>
;; Created: Tue Oct 08 1996
;; Version: $Id: generic-extras.el,v 1.2 1997/04/02 07:02:42 voelker Exp $
;; Keywords: 
;; Time-stamp: <97/03/27 09:35:57 pbreton>
;;
;; Copyright (C) Peter Breton 01Nov96
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; generic-extras.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; LCD Archive Entry:
;; generic-extras|Peter Breton|pbreton@i-kinetics.com|
;; Sample modes for 'generic-mode'|
;; 01-Nov-1996|1.0|~/misc/generic-extras.el.gz|
;;
;;; Commentary:
;;
;; This file contains some pre-defined generic-modes.
;; 
;; INSTALLATION:
;;
;; Add this line to your .emacs file:
;;
;;   (require 'generic-extras)
;;
;; You can decide which modes to load by setting the variable
;; 'generic-extras-enable-list'. Some platform-specific modes are
;; affected by the variables 'generic-define-mswindows-modes' and
;; 'generic-define-unix-modes' (which see).
;;
;; ALTERING THESE MODES:
;;
;; To alter the definition of these modes, use the 'alter-generic-mode-'
;; convenience functions defined in generic-mode.el. Each of these functions
;; takes an optional how-to-alter argument, which can be one of the following
;; symbols: 'overwrite, 'append, 'prepend.
;; 
;; You can also send me new modes (I'll accept ones for file types which are
;; reasonably common) or patches to these ones.
;; 
;;; Change log:
;; $Log: generic-extras.el,v $
; Revision 1.2  1997/04/02  07:02:42  voelker
; *** empty log message ***
;
;; Revision 1.4  1996/11/01 16:51:20  peter
;; Added GPL and LCD information.
;;
;; Revision 1.3  1996/10/19 12:22:07  peter
;; Added new versions of rc and rul modes
;; Regexp patches for generic-bat-mode
;;
;; Revision 1.2  1996/10/17 01:02:41  peter
;; Improved samba and apache modes
;; Added fvwm and x-resource modes
;;

;;; Code:

(require 'generic-mode)
(require 'font-lock)

(defvar generic-extras-enable-list nil
  "*List of generic modes to enable by default.
Each entry in the list should be a symbol.
The variables `generic-define-mswindows-modes' and `generic-define-unix-modes'
also affect which generic modes are defined")

(defvar generic-define-mswindows-modes 
  (memq system-type (list 'windows-nt 'ms-dos))
  "*If non-nil, some MS-Windows specific generic modes will be defined.")

(defvar generic-define-unix-modes
  (not generic-define-mswindows-modes)
  "*If non-nil, some Unix specific generic modes will be defined.")

(if generic-define-mswindows-modes
    (setq generic-extras-enable-list
	  (append (list 'bat-generic-mode 'ini-generic-mode 
			'inf-generic-mode 'rc-generic-mode 
			'reg-generic-mode 'rul-generic-mode)
		  generic-extras-enable-list)))

(if generic-define-unix-modes
    (setq generic-extras-enable-list
	  (append (list 'apache-generic-mode 'samba-generic-mode 
			'hosts-generic-mode  'fvwm-generic-mode 
			'x-resource-generic-mode 
			'crontab-generic-mode)
		  generic-extras-enable-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic-modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Apache
(and 
 (memq 'apache-generic-mode generic-extras-enable-list)

 (define-generic-mode 'apache-generic-mode
   (list ?#)  
   nil 
   '(("^\\(<.*>\\)"       1 'font-lock-reference-face)
     ("^\\(\\sw+\\)\\s-"  1 'font-lock-variable-name-face))    
   (list "srm\\.conf$" "httpd\\.conf$" "access\\.conf$")
   nil 
   "Generic mode for Apache or HTTPD configuration files."))
 
;;; Samba
(and 
 (memq 'samba-generic-mode generic-extras-enable-list)

 (define-generic-mode 'samba-generic-mode
   (list ?\;)
   nil
   '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face))
   (list "smb\\.conf$")
   (list 'generic-bracket-support)
   "Generic mode for Samba configuration files."))

;;; Fvwm
;; This is pretty basic. Also, modes for other window managers could
;; be defined as well.
(and 
 (memq 'fvwm-generic-mode generic-extras-enable-list)

 (define-generic-mode 'fvwm-generic-mode
   (list ?#)
   (list "Style" "Function" "EndFunction" "Popup" "EndPopup")
   nil
   (list "\\.fvwmrc")
   nil
   "Generic mode for FVWM configuration files."))

;;; X Resource
;; I'm pretty sure I've seen an actual mode to do this, but I don't
;; think it's standard with Emacs
(and 
 (memq 'x-resource-generic-mode generic-extras-enable-list)

 (define-generic-mode 'x-resource-generic-mode
   (list ?!)
   nil
   '(("^\\([^:\n]+:\\)" 1 'font-lock-variable-name-face))
   (list "\\.Xdefaults" "\\.Xresources")
   nil
   "Generic mode for X Resource configuration files."))

;;; Hosts
(and 
 (memq 'hosts-generic-mode generic-extras-enable-list)

 (define-generic-mode 'hosts-generic-mode
   (list ?#)
   (list "localhost")
   '(("\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" 1 'font-lock-reference-face))
   (list "[hH][oO][sS][tT][sS]$")
   nil
   "Generic mode for HOSTS files."))

;;; Windows INF files
(and 
 (memq 'inf-generic-mode generic-extras-enable-list)

 (define-generic-mode 'inf-generic-mode
   (list ?\;)
   nil 
   '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face))
   (list "\\.[iI][nN][fF]")
   (list 'generic-bracket-support)
   "Generic mode for MS-Windows INF files."))

;;; Windows INI files
;; Should define escape character as well!
(and 
 (memq 'ini-generic-mode generic-extras-enable-list)

 (define-generic-mode 'ini-generic-mode
   (list ?\;)
   nil
   '(("^\\(\\[.*\\]\\)"   1 'font-lock-reference-face)
     ("^\\(.*\\)="        1 'font-lock-variable-name-face))
   (list "\\.[iI][nN][iI]$")
   (list 'generic-bracket-support)
   "Generic mode for MS-Windows INI files."))

;;; Windows REG files
(and 
 (memq 'reg-generic-mode generic-extras-enable-list)

 (define-generic-mode 'reg-generic-mode
   '(?\;)
   '("key" "classes_root")
   '(("\\(\\[.*]\\)" 1 'font-lock-reference-face))
   '("\\.[rR][eE][gG]$")
   nil
   "Generic mode for MS-Windows Registry files."))

;;; Windows BAT files
(if (not (memq 'bat-generic-mode generic-extras-enable-list))
    nil
  (define-generic-mode 'bat-generic-mode
    nil
    nil
    (list
     ;; Make this one first in the list, otherwise comments will
     ;; be over-written by other variables
     (list "^[@ \t]*\\([rR][eE][mM].*\\)" 1 'font-lock-comment-face t)
     (list "^[ \t]*\\(::-.*\\)"		  1 'font-lock-comment-face t)
     ;; These keywords appear as the first word on a line
     (generic-make-keywords-list
      (list
       "[cC][aA][lL][lL]"
       "[eE][cC][hH][oO]"
       "[fF][oO][rR]"
       "[iI][fF]"
       "[pP][aA][tT][hH]"
       "[pP][aA][uU][sS][eE]"
       "[pP][rR][oO][mM][pP][tT]"
       "[sS][eE][tT]"
       "[sS][tT][aA][rR][tT]"
       )
      'font-lock-keyword-face "^[@ \t]*")
     ;; These keywords can be anywhere on a line
     (generic-make-keywords-list
      (list
       "[eE][xX][iI][sS][tT]"
       "[eE][rR][rR][oO][rR][lL][eE][vV][eE][lL]"
       "[gG][oO][tT][oO]"
       "[nN][oO][tT]"
       ) 'font-lock-keyword-face)
     (list "^[ \t]*\\(:\\sw+\\)"         1 'font-lock-function-name-face t)
     (list "\\(%\\sw+%\\)"		 1 'font-lock-reference-face)
     (list "\\(%[0-9]\\)"		 1 'font-lock-reference-face)
     (list "\\(/[^/ \"\t\n]+\\)"	 1 'font-lock-type-face)
     (list "[\t ]+\\([+-][^\t\n\" ]+\\)" 1 'font-lock-type-face)
     (list "\\<\\([gG][oO][tT][oO]\\)\\>[ \t]*\\(\\sw+\\)?" 
	   '(1 font-lock-keyword-face)
	   '(2 font-lock-function-name-face nil t))
     
     )
    (list "\\.[bB][aA][tT]$" "CONFIG\\." "AUTOEXEC\\." )
    (list 'generic-bat-mode-setup-function)
    "Generic mode for MS-Windows BAT files.")

  (defvar bat-generic-mode-syntax-table nil
    "Syntax table in use in bat-generic-mode buffers.")
  
  ;; Make underscores count as words
  (if bat-generic-mode-syntax-table
      nil
    (setq bat-generic-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_  "w"  bat-generic-mode-syntax-table))
  
  ;; bat-generic-mode doesn't use the comment functionality of generic-mode
  ;; because it has a three-letter comment-string, so we do it
  ;; here manually instead
  (defun generic-bat-mode-setup-function ()
    (make-local-variable	     'parse-sexp-ignore-comments)
    (make-local-variable	     'comment-start)
    (make-local-variable	     'comment-start-skip)
    (make-local-variable	     'comment-end)
    (setq imenu-generic-expression  '((nil "^:\\(\\sw+\\)" 1))
	  parse-sexp-ignore-comments t
	  comment-end                ""
	  comment-start		     "[Rr][Ee][Mm] "
	  comment-start-skip	     "[Rr][Ee][Mm] *"
	  )
    (set-syntax-table	      bat-generic-mode-syntax-table)
    )
  )

;;; Windows RC files
;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
(and 
 (memq 'rc-generic-mode generic-extras-enable-list)

 (define-generic-mode 'rc-generic-mode
   (list ?\/)
   '("ACCELERATORS"
     "AUTO3STATE"
     "AUTOCHECKBOX"
     "AUTORADIOBUTTON"
     "BITMAP"
     "CAPTION"
     "CHARACTERISTICS"
     "CHECKBOX"
     "CLASS"
     "COMBOBOX"
     "CONTROL"
     "CTEXT"
     "CURSOR"
     "DEFPUSHBUTTON"
     "DIALOG"
     "EDITTEXT"
     "EXSTYLE"
     "FONT"
     "GROUPBOX"
     "ICON"
     "LANGUAGE"
     "LISTBOX"
     "LTEXT"
     "MENUITEM SEPARATOR" 
     "MENUITEM" 
     "MENU"
     "POPUP"
     "PUSHBOX"
     "PUSHBUTTON"
     "RADIOBUTTON"
     "RCDATA"
     "RTEXT"
     "SCROLLBAR"
     "SEPARATOR"
     "STATE3"
     "STRINGTABLE"
     "STYLE"
     "VERSIONINFO"
     "VERSION"
     )
   ;; the choice of what tokens go where is somewhat arbitrary,
   ;; as is the choice of which value tokens are included, as
   ;; the choice of face for each token group
   (list
   (generic-make-keywords-list
    (list
     "FILEFLAGSMASK"
     "FILEFLAGS"
     "FILEOS"
     "FILESUBTYPE"
     "FILETYPE"
     "FILEVERSION"
     "PRODUCTVERSION"
     ) 'font-lock-type-face)
   (generic-make-keywords-list
    (list
     "BEGIN"
     "BLOCK"
     "END"
     "VALUE"
     ) 'font-lock-function-name-face)
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   '("^#[ \t]*define[ \t]+\\(\\sw+\\)("       1 font-lock-function-name-face)
   '("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
   '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
   (list "\\.[rR][cC]$")
   nil
   "Generic mode for MS-Windows Resource files."))

;;; InstallShield RUL files
;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
;; Additional contributions by alex@brainstorm.fr (Alex Lemaresquier)
(and 
 (memq 'rul-generic-mode generic-extras-enable-list)

 (define-generic-mode 'rul-generic-mode 
   (list ?\/)
   '("begin"
     "call"
     "case"
     "declare"
     "default"
     "downto"
     "elseif"
     "else"
     "endfor"
     "endif"
     "endswitch"
     "endwhile"
     "end"
     "exit"
     "external"
     "for"
     "function"
					; "goto" -- handled elsewhere
     "if"
     "program"
     "prototype"
     "repeat"
     "return"
     "step"
     "switch"
     "then"
     "to"
     "typedef"
     "until"
     "void"
     "while")
  (list
   ;; preprocessor constructs
   '("#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
     1 font-lock-string-face)
   '("#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face)
     (2 font-lock-variable-name-face nil t))
   ;; gotos
   '("[ \t]*\\(\\sw+:\\)" 1 font-lock-reference-face)
   '("\\<\\(goto\\)\\>[ \t]*\\(\\sw+\\)?" 
     (1 font-lock-keyword-face)
     (2 font-lock-reference-face nil t))
   ;; system variables
   (generic-make-keywords-list
    (list
     "CMDLINE"
     "ERRORFILENAME"
     "INFOFILENAME"
     "ISRES"
     "ISUSER"
     "ISVERSION"
     "SRCDIR"
     "SRCDISK"
     "SUPPORTDIR"
     "TARGETDIR"
     "TARGETDISK"
     "WINDIR"
     "WINDISK"
     "WINMAJOR"
     "WINSYSDIR"
     "WINSYSDISK"
     )
    'font-lock-variable-name-face)
   ;; system functions
   (generic-make-keywords-list
    (list
      "AddFolderIcon"
      "AppCommand"
      "AskDestPath"
      "AskOptions"
      "AskPath"
      "AskText"
      "AskYesNo"
      "CloseFile"
      "CmdGetHwndDlg"
      "CompressEnum"
      "CompressGet"
      "CopyFile"
      "CreateDir"
      "CreateProgramFolder"
      "DeinstallStart"
      "Delay"
      "DeleteDir"
      "DeleteFile"
      "Disable"
      "DoInstall"
      "Do"
      "Enable"
      "EnterDisk"
      "ExistsDir"
      "EzDefineDialog"
      "FindFile"
      "FindWindow"
      "FileCompare"
      "FileSetBeginDefine"
      "FileSetEndDefine"
      "FileSetPerformEz"
      "FileSetPerform"
      "GetDiskSpace"
      "GetDisk"
      "GetExtents"
      "GetProfString"
      "GetSystemInfo"
      "GetVersion"
      "GetWindowHandle"
      "InstallationInfo"
      "Is"
      "LaunchApp"
      "ListCreate"
      "ListDestroy"
      "ListGetFirstString"
      "ListGetNextString"
      "ListSetIndex"
      "LongPathToQuote"
      "LongPathToShortPath"
      "MessageBox"
      "NumToStr"
      "OpenFile"
      "ParsePath"
      "PlaceBitmap"
      "PlaceWindow"
      "ProgDefGroupType"
      "RegDBCreateKeyEx"
      "RegDBGetItem"
      "RegDBSetItem"
      "RegDBGetKeyValueEx"
      "RegDBSetKeyValueEx"
      "RegDBSetDefaultRoot"
      "RenameFile"
      "SdSelectFolder"
      "SdShowMsg"
      "SdWelcome"
      "SetColor"
      "SetDialogTitle"
      "SetFileInfo"
      "SetForegroundWindow"
      "SetStatusWindow"
      "SetTitle"
      "ShowProgramFolder"
      "Sprintf"
      "StatusUpdate"
      "StrCompare"
      "StrFind"
      "StrGetTokens"
      "StrLength"
      "StrRemoveLastSlash"
      "StrToLower"
      "StrToUpper"
      "StrSub"
      "VarRestore"
      "VarSave"
      "WaitOnDialog"
      "Welcome"
      "XCopyFile"
      )
    'font-lock-function-name-face)
   ;; type keywords
   (generic-make-keywords-list
    (list
      "BOOL"
      "BYREF"
      "CHAR"
      "HIWORD"
      "HWND"
      "INT"
      "LIST"
      "LONG"
      "LOWORD"
      "NUMBER"
      "POINTER"
      "QUAD"
      "RGB"
      "SHORT"
      "STRINGLIST"
      "STRING"
      )
    'font-lock-type-face)
   ;;; system variables
   (generic-make-keywords-list
    (list
     "CMDLINE"
     "ERRORFILENAME"
     "INFOFILENAME"
     "ISRES"
     "ISUSER"
     "ISVERSION"
     "SRCDIR"
     "SRCDISK"
     "SUPPORTDIR"
     "TARGETDIR"
     "TARGETDISK"
     "WINDIR"
     "WINDISK"
     "WINSYSDIR"
     "WINSYSDISK"
     )
    'font-lock-variable-name-face)
   ;; pre-defined constants (not exhaustive -- just my favorites)
   (generic-make-keywords-list
    (list
      "AFTER"
      "APPEND"
      "BACKGROUNDCAPTION"
      "BACKGROUND"
      "BACK"
      "BEFORE"
      "BK_BLUE"
      "BK_GREEN"
      "BK_RED"
      "CANCEL"
      "COMMANDEX"
      "COMMAND"
      "CONTINUE"
      "DEFWINDOWMODE"
      "DISABLE"
      "DLG_ERR"
      "ENABLE"
      "END_OF_LIST"
      "EXCLUSIVE"
      "EXISTS"
      "EXIT"
      "FAILIFEXISTS"
      "FALSE"
      "FULL"
      "INDVFILESTATUS"
      "INFORMATION"
      "LIST_NULL"
      "LISTFIRST"
      "LISTNEXT"
      "LOGGING"
      "NEXT"
      "NONEXCLUSIVE"
      "NOSET"
      "NO"
      "OFF"
      "ON"
      "PARTIAL"
      "REPLACE_ITEM"
      "REPLACE"
      "RESET"
      "RESTART"
      "SET"
      "SEVERE"
      "SRCTARGETDIR"
      "STATUS"
      "TRUE"
      "YES"
      "WARNING"
      )
    'font-lock-variable-name-face)     ; is this face the best choice?
   )
  (list "\\.[rR][uU][lL]$")
  nil
  "Generic mode for InstallShield RUL files"))

;;; Info-Mac abstracts
;; Contributed by Jacques Duthen Prestataire (duthen@cegelec-red.fr)
;; 
;; For an example of such a file, you can download (the small):
;; http://hyperarchive.lcs.mit.edu/HyperArchive/Archive/_Font/00font-abstracts.txt
(and 
 (memq 'info-mac-abstract-generic-mode generic-extras-enable-list)

 (define-generic-mode 'info-mac-abstract-generic-mode 
   () 
   (list "Date" "From" "Subject") 
   '(("^#### .*" . font-lock-function-name-face))
   (list "00.*-abstracts\\.txt") 
   nil 
   "Generic mode for info-mac abstract files."))

;;; Mailagent
;; Mailagent is a Unix mail filtering program. Anyone wanna do a generic mode
;; for procmail?
(and 
 (memq 'mailagent-rules-generic-mode generic-extras-enable-list)

 (define-generic-mode 'mailagent-rules-generic-mode
   (list ?#)  
   (list "SAVE" "DELETE" "PIPE" "ANNOTATE" "REJECT")
   '(("^\\(\\sw+\\)\\s-*="         1 'font-lock-variable-name-face)
     ("\\s-/\\([^/]+\\)/[i, \t\n]" 1 'font-lock-reference-face))
   (list "\\.rules$")
   (list 'mailagent-rules-setup-function)
   "Mode for Mailagent rules files.")
 
 (defun mailagent-rules-setup-function () 
   (make-local-variable 'imenu-generic-expression)
   (setq imenu-generic-expression 
	 '((nil "\\s-/\\([^/]+\\)/[i, \t\n]" 1))))
 )

;;; Crontab
;; I didn't write this, I only adapted it for generic-mode
;; If anyone knows who wrote it originally, I'd be glad to credit them
(and 
 (memq 'crontab-generic-mode generic-extras-enable-list)

 (define-generic-mode 'crontab-generic-mode
   (list ?#)  
   nil
   (list 
    (list 
     (concat "^\\(" 
	     ;; Repeated 5 times for minute, hour, day of month,
	     ;; month and day of week fields
	     (mapconcat 'identity (make-list 5 "[*0-9,]+[ \t]+") "") 
	     "\\)\\(.*\\)")
     (list 1 'font-lock-reference-face)
     (list 2 'font-lock-function-name-face)))
   nil
   (list 'crontab-setup-function)
   "Mode for Crontab files.")
 
 (defun crontab-setup-function () 
   (local-set-key "\C-c\C-c" 'crontab-update)
   (local-set-key "\C-x\C-s" 'crontab-update)
   )
 
 (defun crontab ()
   "Edit a crontab file.  
Type \\[save-buffer] to feed the buffer to the crontab command."
   (interactive)
   (switch-to-buffer "*Crontab*")
   (erase-buffer)
   (message "Reading crontab file ... ")(sit-for 0) ; redisplay
   (if (eq (call-process-region (point) (point) "crontab" nil t t "-l") 0)
       (message "Reading crontab file ... done")
     (message "No crontab file")
     (erase-buffer)
     (insert "#min hour dom mon dow (0=Sun) cmd\n"))
   (set-buffer-modified-p nil)
   (crontab-generic-mode))
 
 (defun crontab-update ()
   "Use the current buffer to update the crontab file."
   (interactive)
   (message "Updating crontab file ... ")(sit-for 0) ; redisplay
   (shell-command-on-region (point-min) (point-max) "crontab" nil)
   (message "Updating crontab file ... done")
   (set-buffer-modified-p nil))
 )

;; Contributed by Jacques Duthen Prestataire (duthen@cegelec-red.fr)
(and 
 (memq 'ps-generic-mode generic-extras-enable-list)

 (define-generic-mode 'ps-generic-mode
   () ;; (list ?%) would not permit to differentiate DSC comments.
   (list "def" "if" "ifelse" "forall")              ; some keywords
   '(("^%%[^ \n]*" . font-lock-reference-face)      ; DSC comments
     ("^/[^ \n]*"  . font-lock-function-name-face)  ; func or glob var def
     ("%.*"        . font-lock-comment-face)        ; normal comments
     ("(.*)"       . font-lock-string-face)         ; ps strings
     ("/[^ \n]*"   . font-lock-variable-name-face)  ; symbols
     )
   (list "\\.ps") ;; extension of Postscript files
   nil            ;; no hook
   "Generic mode for PostScript files")
 )

;; Solaris/Sys V prototype files
(and 
 (memq 'prototype-generic-mode generic-extras-enable-list)

 (define-generic-mode 'prototype-generic-mode
   (list ?#)
   nil
   '(
     ("^\\([0-9]\\)?\\s-*\\([a-z]\\)\\s-+\\([A-Za-z_]+\\)\\s-+\\(.*\\)$"
      (2 font-lock-reference-face)
      (3 font-lock-keyword-face))
     ("^\\([a-z]\\) \\([A-Za-z_]+\\)=\\(.*\\)$"
      (1 font-lock-reference-face)
	  (2 font-lock-keyword-face)
	  (3 font-lock-variable-name-face))
     ("^\\(!\\s-*\\(search\\|include\\|default\\)\\)\\s-*\\(.*\\)$"
      (1 font-lock-keyword-face)
      (3 font-lock-variable-name-face))
     ("^\\(!\\s-*\\sw+\\)=\\(.*\\)$"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face))
     )
   (list "prototype$")
   nil
   "Mode for Sys V prototype files"))

;; Solaris/Sys V pkginfo files
(and 
 (memq 'pkginfo-generic-mode generic-extras-enable-list)

 (define-generic-mode 'pkginfo-generic-mode
   (list ?#)
   nil
   '(
     ("^\\([A-Za-z_]+\\)=\\(.*\\)$"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face))
     )
   (list "pkginfo$")
   nil
   "Mode for Sys V pkginfo files"))

(provide 'generic-extras)

;;; generic-extras.el ends here


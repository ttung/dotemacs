;-*-emacs-lisp-*-

;; Fixes for Debian

(setq load-path (cons "=F" load-path))

(setq sgml-ecat-files '("ECAT" "~/sgml/ECAT" "/usr/share/sgml/ECAT"
                         "/usr/local/lib/sgml/ECAT" ))

(or (assoc "\\.s?html?\\'" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.s?html?\\'" . html-mode)
				auto-mode-alist)))

;;(while (rassoc 'html-mode  auto-mode-alist)
;;  (setcdr (rassoc 'html-mode  auto-mode-alist) 'sgml-html-mode))
;; (defalias 'html-mode 'sgml-html-mode)

(setq auto-mode-alist (append '(("\\.xml$" . xml-mode)) auto-mode-alist))

(setq-default sgml-indent-data t)
(setq
 sgml-always-quote-attributes t
 sgml-auto-insert-required-elements t
 sgml-auto-activate-dtd t
 sgml-data-directory "/usr/share/sgml/declaration/"
 sgml-indent-data t
 sgml-indent-step             2
 sgml-minimize-attributes     nil
 sgml-omittag                 nil
 sgml-shortag                 nil
 sgml-custom-markup
 '(("Version1" "<![%Version1[\r]]>")
   ("New page"  "<?NewPage>"))
 sgml-xml-declaration "/usr/share/sgml/declaration/xml.dcl"
 sgml-display-char-list-filename "/usr/share/sgml/charsets/iso88591.map"
 sgml-live-element-indicator t
 sgml-public-map '("%S"  "/usr/share/sgml/%S" "/usr/share/sgml/%o/%c/%d"
		   "/usr/local/lib/sgml/%o/%c/%d")
 sgml-set-face (eq 'x  window-system)
 sgml-system-path '("/usr/share/sgml" "/usr/share/sgml/cdtd" "/usr/local/lib/sgml")
 sgml-tag-region-if-active t
 sgml-validate-command "nsgmls -e -g -s -u %s %s "
 )
(if (eq 'x  window-system)
    (setq sgml-markup-faces
	  '((start-tag . font-lock-keyword-face)
	    (end-tag . font-lock-keyword-face)
	    (ignored . font-lock-string-face)
	    (ms-start . font-lock-other-type-face)
	    (ms-end . font-lock-other-type-face)
	    (shortref . bold)
	    (entity . font-lock-reference-face)
	    (comment . font-lock-comment-face)
	    (pi . font-lock-other-emphasized-face)
	    (sgml . font-lock-function-name-face)
	    (doctype . font-lock-emphasized-face)))
  )

;;;; Autoloads and hooks

(autoload 'compile-internal "compile" "")
(autoload 'html-mode "psgml-html" "HTML mode." t)
(autoload 'reporter-submit-bug-report "reporter" nil)
(autoload 'sgml-attrib-menu "psgml-edit" nil)
(autoload 'sgml-backward-element "psgml-edit" nil)
(autoload 'sgml-backward-up-element "psgml-edit" nil)
(autoload 'sgml-beginning-of-element "psgml-edit" nil)
(autoload 'sgml-change-element-name "psgml-edit" nil)
(autoload 'sgml-charent-to-display-char "psgml-charent" nil)
(autoload 'sgml-check-dtd-subset  "psgml-dtd")
(autoload 'sgml-complete "psgml-edit" nil)
(autoload 'sgml-custom-dtd "psgml-edit" nil)
(autoload 'sgml-custom-markup "psgml-edit" nil)
(autoload 'sgml-describe-element-type "psgml-info" nil)
(autoload 'sgml-describe-entity "psgml-info" nil)
(autoload 'sgml-display-char-to-charent "psgml-charent" nil)
(autoload 'sgml-do-set-option "psgml-edit" nil)
(autoload 'sgml-do-usemap-element  "psgml-dtd" nil)
(autoload 'sgml-doctype-insert "psgml-edit" nil)
(autoload 'sgml-down-element "psgml-edit" nil)
(autoload 'sgml-edit-attrib-clear "psgml-edit" nil)
(autoload 'sgml-edit-attrib-default "psgml-edit" nil)
(autoload 'sgml-edit-attrib-field-end "psgml-edit" nil)
(autoload 'sgml-edit-attrib-field-start "psgml-edit" nil)
(autoload 'sgml-edit-attrib-finish "psgml-edit" nil)
(autoload 'sgml-edit-attrib-next "psgml-edit" nil)
(autoload 'sgml-edit-attributes "psgml-edit" nil)
(autoload 'sgml-element-endable-p "psgml-edit" nil)
(autoload 'sgml-element-menu "psgml-edit" nil)
(autoload 'sgml-end-of-element "psgml-edit" nil)
(autoload 'sgml-end-tag-menu "psgml-edit" nil)
(autoload 'sgml-entities-menu "psgml-edit" nil)
(autoload 'sgml-expand-all-shortrefs "psgml-edit" nil)
(autoload 'sgml-expand-element "psgml-edit" nil)
(autoload 'sgml-expand-entity-reference "psgml-edit" nil)
(autoload 'sgml-file-options-menu "psgml-edit" nil)
(autoload 'sgml-fill-element "psgml-edit" nil)
(autoload 'sgml-fold-element "psgml-edit" nil)
(autoload 'sgml-fold-region "psgml-edit" nil)
(autoload 'sgml-fold-subelement "psgml-edit" nil)
(autoload 'sgml-forward-element "psgml-edit" nil)
(autoload 'sgml-general-dtd-info "psgml-info" nil)
(autoload 'sgml-hide-attributes "psgml-edit" nil)
(autoload 'sgml-hide-tags "psgml-edit" nil)
(autoload 'sgml-indent-line "psgml-edit" nil)
(autoload 'sgml-insert-attribute "psgml-edit" nil)
(autoload 'sgml-insert-element "psgml-edit" nil)
(autoload 'sgml-insert-end-tag "psgml-edit" nil)
(autoload 'sgml-insert-tag "psgml-edit" nil)
(autoload 'sgml-kill-element "psgml-edit" nil)
(autoload 'sgml-kill-markup "psgml-edit" nil)
(autoload 'sgml-list-attributes "psgml-info" nil)
(autoload 'sgml-list-content-elements "psgml-info" nil)
(autoload 'sgml-list-elements "psgml-info" nil)
(autoload 'sgml-list-occur-in-elements "psgml-info" nil)
(autoload 'sgml-list-terminals "psgml-info" nil)
(autoload 'sgml-list-valid-tags "psgml-edit" nil)
(autoload 'sgml-load-dtd "psgml-parse" nil)
(autoload 'sgml-make-character-reference "psgml-edit" nil)
(autoload 'sgml-mark-current-element "psgml-edit" nil)
(autoload 'sgml-mark-element "psgml-edit" nil)
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
(autoload 'sgml-next-data-field "psgml-edit" nil)
(autoload 'sgml-next-trouble-spot "psgml-edit" nil)
(autoload 'sgml-normalize "psgml-edit" nil)
(autoload 'sgml-normalize-element "psgml-edit" nil)
(autoload 'sgml-parse-prolog "psgml-parse" nil)
(autoload 'sgml-save-dtd "psgml-dtd" nil)
(autoload 'sgml-show-attributes "psgml-edit" nil)
(autoload 'sgml-show-context "psgml-edit" nil)
(autoload 'sgml-show-or-clear-log "psgml-parse" nil)
(autoload 'sgml-show-tags "psgml-edit" nil)
(autoload 'sgml-split-element "psgml-edit" nil)
(autoload 'sgml-start-tag-menu "psgml-edit" nil)
(autoload 'sgml-tag-region "psgml-edit" nil)
(autoload 'sgml-tag-region-menu "psgml-edit" nil)
(autoload 'sgml-tags-menu "psgml-edit" nil)
(autoload 'sgml-translate-model "psgml-dtd" "" nil)
(autoload 'sgml-transpose-element "psgml-edit" nil)
(autoload 'sgml-unfold-all "psgml-edit" nil)
(autoload 'sgml-unfold-element "psgml-edit" nil)
(autoload 'sgml-unfold-line "psgml-edit" nil)
(autoload 'sgml-untag-element "psgml-edit" nil)
(autoload 'sgml-up-element "psgml-edit" nil)
(autoload 'sgml-user-options-menu "psgml-edit" nil)
(autoload 'sgml-what-element "psgml-edit" nil)
(autoload 'sgml-write-dtd  "psgml-dtd")
(autoload 'xml-mode "psgml" nil t)
;;;(autoload 'sgml-xpointer "psgml-xpointer" nil t)


(provide 'psgml-init)


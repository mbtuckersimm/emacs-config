;; bunch of stuff removed from custom.el

 '(TeX-save-query nil)
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list '(("Ok" "okular --unique %o#src:%n`pwd`/%b")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Ok")
     (output-html "xdg-open")))
 '(bibtex-entry-format '(opts-or-alts required-fields))
 '(bibtex-generate-url-list
   '((("url" . ".*:.*"))
     (("doi" . "10\\.[0-9]+/.+")
      "http://dx.doi.org/%s"
      ("doi" ".*" 0))
     (("mrkey" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrkey" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("mrnumber" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrnumber" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("arxiv" . "\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\|[-a-z]+/[0-9]\\{7\\}\\|[-a-z]+\\.[a-z]\\{2\\}/[0-9]\\{7\\}\\)\\(v[0-9]+\\)?")
      "http://arxiv.org/abs/%s"
      ("arxiv" ".*" 0))
     (("zblnumber" . "\\([0-9]\\{4\\}\\.[0-9]\\{5\\}\\|[0-9]\\{8\\}\\)")
      "http://zbmath.org/?q=an:%s"
      ("zblnumber" ".*" 0))))


(use-package bibtex
  :custom
  (bibtex-generate-url-list
   (quote
    ((("url" . ".*:.*"))
     (("doi" . "10\\.[0-9]+/.+")
      "http://dx.doi.org/%s"
      ("doi" ".*" 0))
     (("mrkey" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrkey" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("mrnumber" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrnumber" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("arxiv" . "\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\|[-a-z]+/[0-9]\\{7\\}\\|[-a-z]+\\.[a-z]\\{2\\}/[0-9]\\{7\\}\\)\\(v[0-9]+\\)?")
      "http://arxiv.org/abs/%s"
      ("arxiv" ".*" 0))
     (("zblnumber" . "\\([0-9]\\{4\\}\\.[0-9]\\{5\\}\\|[0-9]\\{8\\}\\)")
      "http://zbmath.org/?q=an:%s"
      ("zblnumber" ".*" 0))))))




;;; AUCTeX AND MATH SETTINGS ;;;

;; make > into a comment character in text mode
;; (useful for quoting stuff in email replies, eg)
;; (add-hook 'text-mode-hook (lambda ()
;;             (set (make-local-variable 'comment-start) ">")))

;; explicitly set % for comments in LaTeX mode
;; (otherwise our setting for text-mode overrides it)
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;             (set (make-local-variable 'comment-start) "%")))

;; make $ and $$ pair properly in LaTeX mode
(add-hook 'LaTeX-mode-hook
	  (function (lambda ()
		      (paren-toggle-matching-quoted-paren 1)
		      (paren-rtoggle-matching-paired-delimiter 1))))

;; Math mode for LaTeX
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; spellcheck in LaTeX mode
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; make pdflatex the default
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; hopefully this should make autopair work with dollar signs
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")))

;; make links clickable in LaTeX mode
(add-hook 'LaTeX-mode-hook 'goto-address-mode)

;; set LaTeX-command
(setq-default LaTeX-command "latex -file-line-error-style -synctex=1")

;; not sure what this is: was told to put it in by auctex QuickStart documentation
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; disable annoying comment behavior
(setq LaTeX-syntactic-comments nil)

;; set fill-prefix to nothing in bibtex mode
(add-hook 'bibtex-mode-hook
	  (function (lambda ()
		      (setq fill-prefix nil))))


;; Have AUCTeX report warnings as well as errors.
; (add-hook 'LaTeX-mode-hook 'TeX-toggle-debug-warnings)

;; makes emacs query for the master-file when creating a tex file
;; (setq-default TeX-master nil)

;; Make emacs go into LaTeX-mode when editing .pdf_tex files
(add-to-list 'auto-mode-alist '("\\.pdf_tex\\'" . LaTeX-mode))

;; teach auctex about various msp and ams macros
(setq font-latex-match-function-keywords
      '(
	("alterref" "{{")
	("eqalign" "{")
	("eqaligntop" "{")
	("eqalignbot" "{")
	("displaylines")
	("def")
	("let")
	("qandq")
	("qr")
	("email" "{")
	("keyword" "{")
	("subject" "{{{")
	("numberwithin" "{{")
	("AtBeginDocument" "{")
	("hypertarget" "{{")
	("hyperlink" "{{")
	("includegraphics" "[{")
	("labellist")
	("endlabellist")
	("vadjust" "{")
	("vspace")
	("qua")
))

(setq font-latex-match-textual-keywords
      '(
	("marginpar")
	("marginparhere")
))

(setq font-latex-match-reference-keywords
    '(
	   ("fullref" "{")
	   ("citeyear" "[{")
	   ("citeNP" "[{")
	   ("citeyearNP" "[{")
	   ("citeN" "[{")
	   ("citeANP" "{")
	   ("citeib" "[[{")
))

(setq font-latex-match-warning-keywords
      '(("redden" "{")
	("cr")
	("vspace*")
	("begingroup")
	("endgroup")))


;;; SyncTeX ;;;

;; synctex settings all in custom.el now


;;; RefTeX settings ;;;

;; activate RefTeX and make it interact with AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; don't prompt with intermediate menu
(setq reftex-ref-macro-prompt nil)

;; tell RefTeX about MSP style refs (fullref)
(eval-after-load "reftex"
  '(progn
     (add-to-list 'reftex-ref-style-alist
      '("MSP" t (("\\fullref" ?f) ("\\ref" ?r) ("\\eqref" ?e) ("\\pageref" ?p))))
     (setq reftex-ref-style-default-list '("MSP"))
     (setq reftex-label-alist '(AMSTeX))))

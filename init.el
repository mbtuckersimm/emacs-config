(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(server-start)

;; this stuff changes the locations for autosaves and backups
(setq backup-directory-alist
      `((".*" . ,"/home/matthew/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"/home/matthew/.emacs.d/autosaves/" t)))


(add-to-list 'load-path "/home/matthew/.emacs.d/lisp/")
(load "msp.el")

;; (setq debug-on-quit t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PACKAGE REPO SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
			  ("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; load self-installed packages 
(package-initialize)

;; because we're using a newer version of org-mode downloaded from elpa,
;; and org-setup.el contains code that only works with the new version,
;; we have to load org-setup *after* initializing packages
(load "org-setup.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  i-do mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(setq ido-save-directory-list-file "/home/matthew/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-file-extensions-order '(".tex" ".bib" ".log" ".cls" ".sty"))
(setq ido-separator "\n")
(ido-mode t)


;;; YASnippet ;;;

;; Set YASnippet load-directory
(add-to-list 'load-path "/home/matthew/.emacs.d/snippets/")
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)
;(yas/initialize)
;(yas-load-directory yas-load-dir)
;; (setq yas-triggers-in-field t); Enable nested triggering of snippets
;; (setq yas/wrap-around-region t); Allow snippets to wrap around region - couldn't figure out.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end YASnippet stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  fixing paired delimiters 
(require 'hl-sexp)
(load "change-delims") ;; move this to msp.el
(global-set-key (kbd "C-c d") 'fix-next-sized-delim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "<f6>") 'split-window-horizontally)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s c") 'how-many)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-c e") 'TeX-error-overview)
;; next chunk are for functions in msp.el
(global-set-key (kbd "C-c c") 'comment-and-kill-ring-save)
;; (global-set-key (kbd "<f12>") 'save-and-compile)
(global-set-key (kbd "M-Z") 'comment-to-char)
(global-set-key (kbd "M-s a") 'azaz)
(global-set-key (kbd "M-s r") 'ords)
(global-set-key (kbd "M-s n") 'occur-next-occurrence)
(global-set-key (kbd "M-s p") 'occur-prev-occurrence)
(global-set-key (kbd "C-x p") 'putpaper)
;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  APPEARANCE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ZenBurn color theme
;; to change the highlight-line color, it is under the hl-line-mode
;; settings in zenburn-theme.el
;; (load-theme 'zenburn t)

;; solarized color theme
;; have a setting in custom.el that helps load the dark theme

(add-to-list 'custom-theme-load-path "/home/matthew/.emacs.d/themes/solarized/")
(load-theme 'solarized t)

(display-time-mode 1)

;; smart-mode-line settings

(setq sml/theme 'dark)
;; (setq sml/theme 'light)
;; (setq sml/theme 'respectful)

(sml/setup)

(setq sml/name-width 35)
(setq sml/mode-width "full")

;; (add-to-list 'sml/replacer-regexp-list '("^~/sbcc/137/" ":SBCC:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/pjm/src/work/" ":PJM:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/ant/src/work/" ":ANT:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/akt/src/work/" ":AKT:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/apde/src/work/" ":APDE:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/camcos/src/work/" ":CAMCOS:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/jomms/src/work/" ":JOMMS:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/memocs/src/work/" ":MEMOCS:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/involve/src/work/" ":INVOLVE:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/agt/src/work/" ":AGT:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/gt/src/work/" ":GT:") t)
(add-to-list 'sml/replacer-regexp-list '("^:ANT:[0-9-]+\\([A-Za-z]+\\)/" ":ANT:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:AKT:[0-9-]+\\([A-Za-z]+\\)/" ":AKT:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PJM:[0-9-]+\\([A-Za-z]+\\)/" ":PJM:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:APDE:[0-9-]+\\([A-Za-z]+\\)/" ":APDE:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CAMCOS:[0-9-]+\\([A-Za-z]+\\)/" ":CAMCOS:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:JOMMS:[0-9-]+\\([A-Za-z]+\\)/" ":JOMMS:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:MEMOCS:[0-9-]+\\([A-Za-z]+\\)/" ":MEMOCS:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:INVOLVE:[0-9-]+\\([A-Za-z]+\\)/" ":INVOLVE:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:AGT:[0-9-]+\\([A-Za-z]+\\)/" ":AGT:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:GT:[0-9-]+\\([A-Za-z]+\\)/" ":GT:\\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/training/" ":TRAINING:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/mspdoc/" ":MSPDOC:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/dev/" ":DEV:") t)
(add-to-list 'sml/replacer-regexp-list '("^:ED:snippets" ":YAS:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/warwick/gtpub/" ":GTPUB:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/msp/warwick/agtpub/" ":AGTPUB:") t)


(setq blink-cursor-blinks 0)

;; highlight the current line
(add-to-list 'load-path "/home/matthew/.emacs.d/elpa/highlight-current-line-20051013.1756")
(require 'highlight-current-line)
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))


; text decoration
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

; if there is size information associated with text, change the text
; size to reflect it
(size-indication-mode t)

; highlight parentheses when the cursor is next to them
;; (require 'paren)
;; (show-paren-mode t)


(require 'mic-paren) ; loading
(paren-activate)     ; activating
;; (setq paren-match-face 'highlight)
(setq paren-sexp-mode t)
(add-hook 'LaTeX-mode-hook
	  (function (lambda ()
		      (paren-toggle-matching-quoted-paren 1)
		      (paren-toggle-matching-paired-delimiter 1))))

;; stop editor from breaking line into fields
(setq comint-use-prompt-regexp t)

;; Add colors when running the shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; directory tracking in shell-mode
;; we want to use the dirtrack package rather than shell-dirtrack-mode
(setq dirtrack-list '("^.*?:\\(.*\\)\n" 1 nil))
(add-hook 'shell-mode-hook 'dirtrack-mode)

;; save and restore desktop state
(setq desktop-dirname "~/.emacs.d/desktop")
(setq desktop-base-file-name "emacsdesktop-save")
(setq desktop-base-lock-name "lock")
(setq desktop-path (list desktop-dirname))
(setq desktop-save t)
(desktop-save-mode 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

;; Start without toolbar
(tool-bar-mode -1)

;; Show line-number and column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Allows to move through mark ring with C-<SPC> after one initial C-u C-<SPC>
(setq set-mark-command-repeat-pop t)

;; Visual-line mode (word-wrapping)
;; (global-visual-line-mode nil)

;; displays filename (or buffername if no filename) in title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; mouse scrolling
(mouse-wheel-mode t)

;; disable scroll bar
(scroll-bar-mode -1)

(horizontal-scroll-bar-mode -1)

;; turn on Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEXT-EDITING SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require cases to match when replacing
(setq case-replace nil)

(setq require-final-newline t)

;; turns on AutoPair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 
;; enable auto-wrap for AutoPair
(setq autopair-autowrap t)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; expand-region package
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; make > into a comment character in text mode
;; (useful for quoting stuff in email replies, eg)
(add-hook 'text-mode-hook (lambda ()
            (set (make-local-variable 'comment-start) ">")))

;;; AUCTeX AND MATH SETTINGS ;;;

;; Math mode for LaTeX
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; spellcheck in LaTeX mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; make pdflatex the default
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; hopefully this should make autopair work with dollar signs
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")))

;; set LaTeX-command
(setq-default LaTeX-command "latex -file-line-error-style -synctex=1")

;; not sure what this is: was told to put it in by auctex QuickStart documentation
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; set fill-prefix to nothing in bibtex mode
(add-hook 'bibtex-mode-hook
	  (function (lambda ()
		      (setq fill-prefix nil))))


;; Have AUCTeX report warnings as well as errors.
; (add-hook 'LaTeX-mode-hook 'TeX-toggle-debug-warnings)

;; makes emacs query for the master-file when creating a tex file
;; (setq-default TeX-master nil)

;; Make emacs go into LaTeX-mode when editing .ltb files (for amsrefs)
;; (add-to-list 'auto-mode-alist '("\\.ltb\\'" . LaTeX-mode))
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
      '(
	("redden" "{")
	("cr")
	("vspace*")
	("begingroup")
	("endgroup")
))


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


;;; Auto-Complete ;;;
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20140519.650")
(require 'auto-complete) 
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140519.650/dict")
(require 'auto-complete-config) 
(ac-config-default)
(global-auto-complete-mode t)

(provide 'auto-complete-settings)


;;; miscellaneous ;;;
(add-to-list 'auto-mode-alist '("\\.plx\\'" . perl-mode))
(setq python-shell-interpreter "python3")



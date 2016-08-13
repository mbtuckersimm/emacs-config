;; some basic commands to ease editing

;; ideas: make functions for fullref, stripping comments, wrapping lines
;; make function for adding blank lines around theorems, etc.

;; make a version of M-x occur that is case-sensitive
;; -- need to set case-fold-search to nil in order to do this
;; see definition of azaz below

;; rebind C-s to do the search with fuzzy whitespace matching?

;; make regexps for searching for numerical spans, hardcoded xrefs,
;; common abbreviations such as i.e., e.g., cf. and variants


;; sets environment variable MSPHOME in order to use putpaper from within Emacs
(setenv "MSPHOME" "Documents/msp")
(setq vc-rcs-checkin-switches '("-l" "-d"))

;; function to run shell command putpaper
;; relies on setting environment variable MSPHOME to Documents/msp (in)
(defun putpaper ()
  (interactive)
  (shell-command "putpaper")
  (other-window 1))


;; next two commands are for moving to next/previous occurrence of a
;; regex just searched for with occur
(defun occur-next-occurrence ()
  (interactive)
  (other-window 1)
  (occur-next)
  (occur-mode-goto-occurrence))

(defun occur-prev-occurrence ()
  (interactive)
  (other-window 1)
  (occur-prev)
  (occur-mode-goto-occurrence))
;;

(defun save-and-compile ()
  "Save the current file and then call TeX-command-master"
  (interactive)
  (save-buffer)
  (TeX-command-master))

(defun comment-and-kill-ring-save ()
  "Copy the current region into the kill ring and then comment it out"
  (interactive)
  (save-excursion
    (kill-ring-save (region-beginning) (region-end))
    (comment-region (region-beginning) (region-end))))

(defun comment-to-char (arg char)
  "Comment up to and including ARG'th occurrence of CHAR.
     Case is ignored if `case-fold-search' is non-nil in the current buffer.
     Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncComment to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (comment-region (point) (progn
			    (search-forward (char-to-string char)
					    nil nil arg)
			    (point))))

(defun azaz ()
  (interactive)
  (setq case-fold-search nil)
  (occur "\[a-z\]-\[A-Z\]")
  (setq case-fold-search t))

(defun hardwired-refs ()
  (interactive)
  (setq case-fold-search nil)
  (occur "[A-Z][a-z]+\.?\\([\n\t ]+\\|~\\)[0-9]")
  (setq case-fold-search t))

(defun manual-formatting ()
  (interactive)
  (occur "\\(vskip\\|hskip\\|vspace\\|hspace\\|noindent\\|smallbreak\\|medbreak\\|bigbreak\\|smallskip\\|medskip\\|bigskip\\|newpage\\|clearpage\\|linebreak\\|pagebreak\\|\\<par\\>\\)"))

(defun abbrevs ()
  (interactive)
  (setq case-fold-search nil)
  (occur "\\.\[\n\t \]+\[a-z\]")
  (setq case-fold-search t))

(defun ords ()
  (interactive)
  (setq case-fold-search nil)
  (occur "\\(\\<th\\>\\|\\<st\\>\\|\\<nd\\>\\|\\<rd\\>\\)")
  (setq case-fold-search t))

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

;; see ~/.emacs.d/lisp/bibtexurlstuff.el
;; the code contained there goes into custom-set-variables
;; (currently in ~/.emacs, but should go into ~/.emacs.d/custom.el eventually)
;; it turns mrkey, zblnumber, and arxiv ids into clickable links in a .bib file
;; for redundancy, that code is also below

 ;; '(bibtex-generate-url-list
 ;;   '((("url" . ".*:.*"))
 ;;     (("doi" . "10\\.[0-9]+/.+")
 ;;      "http://dx.doi.org/%s"
 ;;      ("doi" ".*" 0))
 ;;     (("mrkey" . "\\(mr\\)?[0-9]\\{1,8\\}")
 ;;      "http://www.ams.org/mathscinet-getitem?mr=%s"
 ;;      ("mrkey" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
 ;;     (("arxiv" . "\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\|[-a-z]+/[0-9]\\{7\\}\\|[-a-z]+\\.[a-z]\\{2\\}/[0-9]\\{7\\}\\)\\(v[0-9]+\\)?")
 ;;      "http://arxiv.org/abs/%s"
 ;;      ("arxiv" ".*" 0))
 ;;     (("zblnumber" . "\\([0-9]\\{4\\}\\.[0-9]\\{5\\}\\|[0-9]\\{8\\}\\)")
 ;;      "http://zbmath.org/?q=an:%s"
 ;;      ("zblnumber" ".*" 0))))


;; everything below is still a work in progress

;; (defun find-begin-document ()
;;   (interactive)
;;   (goto-char (point-min)) ;; to beginning of buffer
;;   (search-forward "\\begin{document}")
;;   (beginning-of-line)) ;; move to beggining of line

;; (defun fullref ()
;;   (interactive)
;;   (mark-whole-buffer) ;; the internet says not to use this in interactive programs
;;     ) ;; look into shell-command-region and call-process-region

;; (defun num-span ()
;;   (interactive)
;;   ("")
;; )

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

(eval-after-load "vc"
  '(defun vc-steal-lock (file rev owner)
  "Steal the lock on FILE."
  (let (file-description)
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (when (not (yes-or-no-p (format "Steal the lock on %s from %s? "
				    file-description owner)))
      (error "Steal canceled"))
    (message "Stealing lock on %s..." file)
    (with-vc-properties
     (list file)
     (vc-call steal-lock file rev)
     `((vc-state . edited)))
    (vc-resynch-buffer file t t)
    (message "Stealing lock on %s...done" file)
    ;; commented this chunk out to avoid emailing re: stealing the lock
    ;;
    ;; Write mail after actually stealing, because if the stealing
    ;; goes wrong, we don't want to send any mail.
    ;; (compose-mail owner (format "Stolen lock on %s" file-description))
    ;; (setq default-directory (expand-file-name "~/"))
    ;; (goto-char (point-max))
    ;; (insert
    ;;  (format "I stole the lock on %s, " file-description)
    ;;  (current-time-string)
    ;;  ".\n")
    ;; (message "Please explain why you stole the lock.  Type C-c C-c when done.")
    )))

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
  (occur "\\(vskip\\|hskip\\|vspace\\|hspace\\|noindent\\|smallbreak\\|medbreak\\|bigbreak\\|smallskip\\|medskip\\|bigskip\\|newpage\\|clearpage\\|linebreak\\|newline\\|pagebreak\\|\\<par\\>\\|setlength\\)"))

(defun ieeg ()
  (interactive)
  (setq case-fold-search nil)
  (occur "\\<\\(e\\.?g\\.?\\|i\\.?e\\.?\\|c\\.?f\\.?\\)\\>")
  (setq case-fold-search t))

;; work in progress
(defun endashes ()
  (interactive)
)

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

;; next bunch of stuff adds patch command to TeX command list
;; accessed by TeX-compile, C-c C-c

;; no idea if this is the right thing to do
;; copied from TeX-run-BibTeX in tex-buf.el
(defun TeX-run-patch (name command file)
  "Create a process for NAME using COMMAND to patch bib of FILE."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-patch-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

;; need to modify this one still
;; copied from TeX-BibTeX-sentinel in tex-buf.el
(defun TeX-patch-sentinel (_process _name)
  "Cleanup output buffer after running patch."
  (goto-char (point-max))
  (cond
   ;; Check whether BibTeX reports any warnings or errors.
   ((re-search-backward (concat
			 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			 "\\(warnings?\\|error messages?\\))")
                        nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "BibTeX finished with %s %s. "
		     "Type `%s' to display output.")
	     (match-string 1) (match-string 2)
	     (substitute-command-keys
	      "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "BibTeX finished successfully. "
		     "Run LaTeX again to get citations right."))))
  ;; In any case, run the default next command.
  (setq TeX-command-next TeX-command-default))

(eval-after-load "tex"
  '(progn
     ;; add command to patch bib to TeX-command-list
     (add-to-list 'TeX-command-list
     		  '("Patch" "patch %s.bbl %s.pat" TeX-run-compile t t
     		    :help "Patch the .bbl file using the .pat file"))
     ;; remove annoying biber command
     (delete '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") TeX-command-list)))

;; found basis for this at https://tex.stackexchange.com/a/36914/4642,
;; unfortunately it doesn't automatically set the next command
;; to LaTeX, as desired

;; (add-to-list 'TeX-command-list
;;     '("Patch" "patch %s.bbl %s.pat"
;; 	      (lambda (name command file)
;; 		 (TeX-run-compile name command file)
;; 		 ;; (TeX-process-set-variable file 'TeX-command-next "LaTeX"))
;; 		 (setq TeX-command-next "LaTeX"))
;;          ;; first t could be set to nil to avoid having to confirm
;; 	      ;; the patch command
;;          t t :help "Patch the .bbl file using the .pat file"))



(defun blank-line ()
  "Return true if there are only whitespace characters on line"
  (string-match-p "^\\s-*$" (buffer-substring-no-properties
			    (line-beginning-position) (line-end-position))))

(defun white-line-to-point ()
  "Return true if there are no nonwhitespace characters before point on line"
  (string-match-p "^\\s-*$" (buffer-substring-no-properties
			    (line-beginning-position) (point))))

(defun white-line-from-point ()
  "Return true if there are no nonwhitespace characters after point on line"
  (string-match-p "^\\s-*$" (buffer-substring-no-properties
			    (point) (line-end-position))))

(defun mpar-replace (orig-string new-string)
  "Replace region with prompted string and marginpar the change"
  (interactive
   (let ((oldstring
	  (buffer-substring-no-properties (region-beginning) (region-end))))
     (list
      oldstring
      (read-string (format "Replace %s with: " oldstring) oldstring))))
  (delete-region (region-beginning) (region-end))
  (unless (white-line-to-point) (newline))
  (insert (format "\\marginpar{%s $\\to$ %s}" orig-string new-string))
  (newline) 
  (insert new-string)
  (forward-whitespace 1)
  (unless (or (bolp) (white-line-from-point)) (newline)))

(defun comment-copy-region ()
  "Comment out the current region and copy it below (adding newlines if needed)"
  (interactive)
  (save-excursion
    (let ((cpstring
	   (buffer-substring-no-properties (region-beginning) (region-end))))
      (save-excursion
      (goto-char (region-beginning))
      (unless (white-line-to-point) (newline))
      (goto-char (region-end)))
      (unless (white-line-from-point) (newline))
      (comment-region (region-beginning) (region-end))
      (insert cpstring)
      (forward-whitespace 1)
      (unless (white-line-from-point) (newline)))))


;; set keybindings for msp/tex stuff
(defun msp-kbd-config ()
  "Set keybindings for MSP/TeX editing. For use in `LaTeX-mode-hook'."
  (local-set-key (kbd "C-c e") 'TeX-error-overview)
  (local-set-key (kbd "M-s a") 'azaz)
  (local-set-key (kbd "M-s r") 'ords)
  (local-set-key (kbd "C-x p") 'putpaper)
  (local-set-key (kbd "C-c r") 'mpar-replace))

;; actually add the hook so the keyboard is configured
;; when LaTeX-mode is entered
(add-hook 'LaTeX-mode-hook 'msp-kbd-config)




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

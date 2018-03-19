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

;; expand-region package
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(defun expand-region-LaTeX-mode-hook ()
  (require 'latex-mode-expansions)
  (er/add-latex-mode-expansions))

(add-hook 'LaTeX-mode-hook 'expand-region-LaTeX-mode-hook)

;; workaround needed for expand-region to interact nicely
;; with transient-mark-mode
(setq shift-select-mode nil)

;; embrace package
(require 'embrace)
(global-set-key (kbd "C-,") #'embrace-commander)

;; fastnav a bit too much at this point, maybe later
;; (require 'fastnav)

(require 'hl-sexp)


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

;;; Ramsay Dyer 20171003. Matt asked me some reasonable questions about this function
;;; mpar-replace that I wrote less than half a year ago. I couldn't help him
;;; at all, because I'd already forgotten the reasons why I did what I did.
;;; So I'm going to digest it once again, and comment it sufficiently that
;;; it hopefully cannot be opaque anymore.

;; This is an auxilliary function, whose purpose is well-described by the 
;; comment string. I think this short snippet of code is pretty much 
;; self-documenting. The documentation for string-match-p says it is the same
;; as string-match, except that it doesn't change the match data.
(defun white-line-to-point ()
  "Return true if there are no nonwhitespace characters before point on line"
  (string-match-p "^\\s-*$" (buffer-substring-no-properties
			    (line-beginning-position) (point))))

;; Likewise, this auxilliary function should be clear: return true if there
;; is nothing on the line after the point, except possibly for whitespace (\s-*)
(defun white-line-from-point ()
  "Return true if there are no nonwhitespace characters after point on line"
  (string-match-p "^\\s-*$" (buffer-substring-no-properties
			    (point) (line-end-position))))

;; This is the function we want to understand. It is called interactively, 
;; but in principle, there is no reason why it needs to be. It could be called
;; from within the massive semisentient super elisp editor that will eventually
;; make me redundant. It is for this dreamy reason that the function takes 
;; arguments: if it were to be strictly only called interactively (a realistic
;; assumption), then there would be no reason to have a nonempty argument list
;; for the function: the orig-string and new-string are defined by interactive
;; input.
;;
(defun mpar-replace (orig-string new-string)
  "Replace region with prompted string and marginpar the change"
  ; first thing we do is call interactive. From the C-h f documentation:
  ; "If the argument [to interactive] is not a string, it is evaluated to 
  ; get a list of arguments to pass to the function." In our case the 
  ; argument is the let form.
  (interactive
   ; Doing `C-h f let', we see that the return value of (let VARLIST BODY) is
   ; the last form in body. In our case, the body consists of a single form,
   ; (list ...) which is a list (described below). The varlist is a single
   ; variable-definition pair: the variable oldstring, defined by the current
   ; active region.
   (let ((oldstring
	  (buffer-substring-no-properties (region-beginning) (region-end))))
     ; list is just what you expect: we are making a list. This list has two
     ; items: the oldstring we just defined above, and the return value from
     ; the read-string form. Thus this two-item list defines those arguments
     ; to our function: interactive will now give orig-string the value of
     ; oldstring, and new-string will be whatever we got out of read-string.
     (list
      oldstring
      (read-string (format "Replace \'%s\' with: " oldstring) oldstring))))
  ; Okay, now our arguments are defined. The actual function begins here.
  ; First we delete the highlighted region.
  (delete-region (region-beginning) (region-end))
  ; now we insert a newline, unless we are already at the beginning of a line
  (unless (white-line-to-point) (newline))
  ; now we insert our marginpar
  (insert (format "\\marginpar{%s $\\to$ %s}" orig-string new-string))
  (newline)
  ; and now we insert the new-string. I want it on a line by itself, so
  ; we may need a newline after it.
  (insert new-string)
  ; go to the next nonwhitespace character: this may take us to the beginning
  ; of the next line:
  (forward-whitespace 1)
  ; If the point is not at the beginning of a line, and there are nonwhitespace
  ; characters after the point, then insert a newline (so that our new-string
  ; is all by itself on a line.)
  (unless (or (bolp) (white-line-from-point)) (newline))
  ; move to the end of the marginpar so it can be expanded if desired
  (re-search-backward "\\marginpar")
  (move-end-of-line 1)
  (backward-char 1))


(defun mpar-delete (orig-string)
  "Delete region and marginpar the change"
  (interactive
   (let ((oldstring
	  (buffer-substring-no-properties (region-beginning) (region-end))))
     (list oldstring)))
  (delete-region (region-beginning) (region-end))
  (unless (white-line-to-point) (newline))
  (insert (format "\\marginpar{deleted ``%s''}" orig-string))
  (newline)
  ;; regexp search puts us back at the end of the marginpar
  (re-search-backward "}"))

(defun mpar-insert (new-string)
  "Prompt user for input, insert it at point and marginpar the change"
  (interactive
   (let ((inputstring (read-string "Text to insert: " ) ))
     (list inputstring)))
  (unless (white-line-to-point) (newline))
  (insert (format "\\marginpar{inserted ``%s''}" new-string))
  (newline)
  (insert new-string)
  (forward-whitespace 1)
  (unless (or (bolp) (white-line-from-point)) (newline))
  (re-search-backward "\\marginpar")
  (move-end-of-line 1)
  (backward-char 1))

(defun mpar-combine ()
  "With point on same line as a marginpar, combine current marginpar with 
   previous one. Assumes there is only whitespace preceding the marginpar
   on the current line, and that there is nothing following the marginpar.
   ('marginpar' can also be marginparhere throughout)"
  (interactive)
  (move-beginning-of-line 1)
  (re-search-forward "\\\\marginpar\\(here\\)?\\s-*{")
  (er/mark-inside-pairs)
  (kill-region (region-beginning) (region-end))
  (move-beginning-of-line 1)
  (delete-region (point) (progn (re-search-forward "\n") (point)))
  (re-search-backward "\\\\marginpar")
  (re-search-forward "{")
  (backward-char 1)
  (forward-sexp 1)
  (backward-char 1)
  (insert "\\\\[2pt] ")
  (yank 1))

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



;; need to have this highlight the match
(defun embrace-with-size ()
  (setq lsizeprefs '((0 . "")
		     (1 . "\\bigl")
		     (2 . "\\Bigl")
		     (3 . "\\biggl")
		     (4 . "\\Biggl")
		     (5 . "\\left")))
  (setq rsizeprefs '((0 . "")
		     (1 . "\\bigr")
		     (2 . "\\Bigr")
		     (3 . "\\biggr")
		     (4 . "\\Biggr")
		     (5 . "\\right")))
  (embrace--hide-help-buffer)
  (let ((dsize (string-to-number (char-to-string (read-char-choice "Delimiter size desired (0 --> none, 1 --> \\big, ..., 4 --> \\Bigg, 5--> \\left): " '(?0 ?1 ?2 ?3 ?4 ?5)))) ))
    (cons (cdr (assoc dsize lsizeprefs)) (cdr (assoc dsize rsizeprefs)) )))

(defun embrace-change-sized-delims ()
    (interactive)
    (let* ((char ?s)
	   (overlay (embrace--delete ?s t)))
    (embrace--insert ?s overlay)))

(defun my-embrace-LaTeX-mode-hook ()
  (embrace-add-pair-regexp ?s "\\(\\\\left\\>\\|\\\\[bB]ig+l?\\)"  "\\(\\\\right\\>\\|\\\\[bB]ig+r?\\)" 'embrace-with-size (embrace-build-help  "\\left" "\\right")))

(add-hook 'LaTeX-mode-hook 'my-embrace-LaTeX-mode-hook)





;;; so far this works to change existing sizing delims to anything else
;;; now have some goals:
;;; 1. write a wrapper to replace change-delims that allows you
;;;    to bypass most of the work to call embrace-with-size
;;; 2. write another function that strictly adds sizing to an
;;;    an existing pair that doesn't have sizing
;;; 3. combine these two into something that can do both
;;; 4. get it to interact nicely with transient-mark-mode


(defun change-delims (dsize)
  "Change matched pair of sized delimiters to appropriate size.
   Use 0--5 for normal size, big, Big, bigg, Bigg, left/right respectively.
   This assumes that point is between the size command and the delimiter.
   Also assumes that mic-paren has been activated and that the option has been enabled to match \{ and \}.
   This will probably fail if there are unbalanced delimiters
   inside the delimiters you are trying to change."
  (interactive "nDelimiter size desired: ")
  (setq lsizeprefs '(
		    (0 . "")
		    (1 . "\\bigl")
		    (2 . "\\Bigl")
		    (3 . "\\biggl")
		    (4 . "\\Biggl")
		    (5 . "\\left")))
  (setq rsizeprefs '(
		    (0 . "")
		    (1 . "\\bigr")
		    (2 . "\\Bigr")
		    (3 . "\\biggr")
		    (4 . "\\Biggr")
		    (5 . "\\right")))
  ;; need to add a fix for what happens if dsize is out of the desired range
(unless (eq 5 dsize)
  (backward-kill-word 1)
  (backward-delete-char 1)
  (insert (cdr (assoc dsize rsizeprefs)))
  (find-right-delimiter)
  (backward-sexp)
  (backward-word)
  (backward-delete-char 1)
  (kill-word 1)
  (insert (cdr (assoc dsize lsizeprefs)))))

(defun find-right-delimiter ()
  "Moves point immediately to the right of the next right delimiter."
  (interactive)
  (re-search-forward "\\()\\|]\\|\\}\\)"))

(defun find-size-macro ()
  (interactive)
  (re-search-forward "\\\\right\\>"))
  ;; (re-search-forward "\\(\\\\right\\>\\|\\\\[bB]ig\\{1,2\\}r?\\)"))

(defun fix-next-sized-delim ()
  (interactive)
  (find-size-macro)
  (hl-sexp-mode 1)
  (hl-sexp-highlight)
  (condition-case nil  (change-delims (string-to-number (char-to-string (read-char-choice "Delimiter size desired: " '(?0 ?1 ?2 ?3 ?4 ?5)))))
(quit (hl-sexp-mode 0)))
  (hl-sexp-mode 0))



;; set keybindings for msp/tex stuff
(defun msp-kbd-config ()
  "Set keybindings for MSP/TeX editing. For use in `LaTeX-mode-hook'."
  (local-set-key (kbd "C-c e") 'TeX-error-overview)
  ;; (local-set-key (kbd "C-x p") 'putpaper)
  (local-set-key (kbd "M-s a") 'azaz)
  (local-set-key (kbd "M-s m") 'manual-formatting)
  (local-set-key (kbd "M-s r") 'ords)
  (local-set-key (kbd "C-; r") 'mpar-replace)
  (local-set-key (kbd "C-; d") 'mpar-delete)
  (local-set-key (kbd "C-; i") 'mpar-insert)
  (local-set-key (kbd "C-; c") 'mpar-combine)
  (local-set-key (kbd "C-=") 'er/expand-region)
  (local-set-key (kbd "C-; p") 'er/mark-inside-pairs)
  (local-set-key (kbd "C-; m") 'er/mark-LaTeX-math)
  (local-set-key (kbd "C-; e") 'er/mark-LaTeX-inside-environment)
  (local-set-key (kbd "C-,") #'embrace-commander)
  ;; still working on this one
  ;; (local-set-key (kbd "C-c d") 'fix-next-sized-delim)
  (local-set-key (kbd "C-; f") 'embrace-change-sized-delims))

;; actually add the hook so the keyboard is configured
;; when LaTeX-mode is entered
(add-hook 'LaTeX-mode-hook 'msp-kbd-config)

(defun add-msp-envs ()
  (add-to-list 'LaTeX-environment-list '("noparthm"))
  (add-to-list 'LaTeX-environment-list '("squeeze"))
  (add-to-list 'LaTeX-environment-list '("tcases"))
  (add-to-list 'LaTeX-environment-list '("fussyeq")))
(add-hook 'LaTeX-mode-hook 'add-msp-envs)

(defun add-msp-classes ()
  (add-to-list 'LaTeX-style-list '("agtart"))
  (add-to-list 'LaTeX-style-list '("akt"))
  (add-to-list 'LaTeX-style-list '("ant"))
  (add-to-list 'LaTeX-style-list '("apde"))
  (add-to-list 'LaTeX-style-list '("camcos"))
  (add-to-list 'LaTeX-style-list '("gtart_m"))
  (add-to-list 'LaTeX-style-list '("gtpart"))
  (add-to-list 'LaTeX-style-list '("involve"))
  (add-to-list 'LaTeX-style-list '("jomms"))
  (add-to-list 'LaTeX-style-list '("memocs"))
  (add-to-list 'LaTeX-style-list '("pjmnew")))
(add-hook 'LaTeX-mode-hook 'add-msp-classes)


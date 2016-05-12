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

;; current problems/things to fix/features to add
;;
;; * make repeatable with single keystroke
;;     * could do this by having a loop within the main function
;; * make this work with [Bb]ig*[lr]? fences also
;;     * current issue is that fix-next-sized-delim works by searching for a
;;       size macro rather than a delimiter, so it doesn't see eg \big);
;;     * but if we change find-size-macro to see \big, then it doesn't
;;       distinguish between left and right anymore
;;     * solution: make it look for a size followed by a right delimiter

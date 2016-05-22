;; if current working directory (ie default-directory) is /path/to/here,
;; this function inserts "here"
;; used in the paper-capture snippet

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
	(sequence "NITPICKING(!)" "NITS_SENT(!)" "NITS_DONE(!)" "PROOFS_SENT(!)"
		  "PROOFS_RECEIVED(!)" "ACS_RECEIVED(!)" "ACS_ASSIGNED" "ACS_DONE(!)"
		  "|" "READY(!)")))

(setq org-todo-keyword-faces
      '(("NITPICKING" . (:foreground "#002b36" :background "tomato" :weight bold))
	("NITS_SENT" . (:foreground "#002b36" :background "SeaGreen3" :weight bold))
	("NITS_DONE" . (:foreground "#002b36" :background "chocolate" :weight bold))
	("PROOFS_SENT" . (:foreground "#002b36" :background "OliveDrab3" :weight bold))
	("PROOFS_RECEIVED" . (:foreground "#002b36" :background "cyan3" :weight bold))
	("ACS_RECEIVED" . (:foreground "#002b36" :background "goldenrod" :weight bold))
	("ACS_ASSIGNED" . (:foreground "#002b36" :background "Plum4" :weight bold))
	("ACS_DONE" . (:foreground "#002b36" :background "DeepPink" :weight bold))
	("READY" . org-done)))

(defun top-level-dir ()
    (interactive)
    (file-name-nondirectory (directory-file-name (file-name-directory default-directory))))


(defun set-msp-agenda-files ()
  (interactive)
  (let ((gt-org-files (file-expand-wildcards
		      "/home/matthew/msp/gt/src/work/*/*.org" t))
        (agt-org-files (file-expand-wildcards
			"/home/matthew/msp/agt/src/work/*/*.org" t))
	(warwick-todo-file '("/home/matthew/msp/warwick/warwick.org"))
        (mspdoc-todo-file '("/home/matthew/msp/mspdoc/mspdoc.org")))
       (setq org-agenda-files (append gt-org-files agt-org-files warwick-todo-file mspdoc-todo-file))))

(defun org-msp-agenda ()
  (interactive)
  (set-msp-agenda-files)
  (org-agenda))

(defun extract-journal ()
  (interactive)
  (string-match "msp/\\(gt\\|agt\\)/src/work/[0-9]+-[A-Za-z]+" default-directory)
  (let ((journal  (match-string 1 default-directory)))
       (upcase journal)))

(defun extract-paper-id ()
  (interactive)
  (string-match "msp/.*?work/\\([0-9]+-[A-Za-z]+\\)" default-directory)
  (match-string 1 default-directory))

(defun pages-in-main ()
  (interactive)
  (substring (shell-command-to-string "pdftk main.pdf dump_data | grep NumberOfPages | sed s/[^0-9]*//g") 0 -1))

(global-set-key (kbd "\C-c a") 'org-msp-agenda)
(setq org-agenda-restore-windows-after-quit t)

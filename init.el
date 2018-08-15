;;; init.el --- Summary
;;; Author: Matthew Tucker-Simmons
;;; Time-stamp: <2018-08-15 08:25:27 matthew>

;;; Commentary:
;;; This is only here to stop flycheck from giving me a warning.
;;; Maybe I should tell flycheck not to warn me about that instead.

;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  basic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(server-start)

;; this stuff changes the locations for autosaves and backups
(setq backup-directory-alist
      `((".*" . ,"/home/matthew/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"/home/matthew/.emacs.d/autosaves/" t)))

(add-to-list 'load-path "/home/matthew/.emacs.d/lisp/")

(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(mouse-wheel-mode t)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(transient-mark-mode 1)
(display-time-mode 1)
(size-indication-mode t)
(setq blink-cursor-blinks 0)
(setq require-final-newline t)

;; move through mark ring with C-<SPC> after one initial C-u C-<SPC>
(setq set-mark-command-repeat-pop t)

;; displays filename (or buffername if no filename) in title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; just used for this file, so far
(add-hook 'before-save-hook #'time-stamp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end basic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ;; ("marmalade" . "http://marmalade-repo.org/packages/")
         		  ("melpa" . "http://melpa.milkbox.net/packages/")
	        	  ("melpa-stable" . "https://stable.melpa.org/packages/")
		          ("org" . "http://orgmode.org/elpa/")))

;; load self-installed packages
(package-initialize)

(require 'use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  packages with minimal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-="   . er/expand-region)
	 ("C-; p" . er/mark-inside-pairs))
  :config
  (setq shift-select-mode nil)
  :ensure t)

(use-package embrace
  :bind ("C-," . embrace-commander)
  :ensure t)

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :ensure t)

(use-package yasnippet
  :init
  (add-to-list 'load-path "/home/matthew/.emacs.d/snippets/")
  :config
  (yas-global-mode 1)
  :ensure t)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package autopair
  :config
  (autopair-global-mode)
  (setq autopair-autowrap t)
  :diminish autopair-mode
  :ensure t)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :ensure t)

(use-package whitespace
  :custom
  (whitespace-style '(face trailing))
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'LaTeX-mode-hook #'whitespace-mode)
  :diminish whitespace-mode
  :ensure t)

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  desktop mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-dirname "~/.emacs.d/desktop")
(setq desktop-base-file-name "emacsdesktop-save")
(setq desktop-base-lock-name "lock")
(setq desktop-auto-save-timeout 300)
(setq desktop-path (list desktop-dirname))
(setq desktop-save t)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end desktop mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(use-package orgit
  :after (org magit)
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'helm)
(use-package helm-config
  ;; :init
  ;; (custom-set-variables '(helm-command-prefix-key "C-;"))
  :config
  (bind-keys :map helm-command-map
             ("a" . helm-ag)
             ("o" . helm-occur)
             ("y" . yas-insert-snippet)
	     ("g" . helm-google-suggest)))

(use-package helm
  :custom
   (helm-split-window-default-side 'right) ;; open helm buffer in another window
   (helm-candidate-number-limit 200) ; limit the number of displayed canidates
   (helm-M-x-requires-pattern   0)     ; show all candidates when set to 0
   (helm-M-x-fuzzy-match        t)
   (helm-buffers-fuzzy-matching t)
   (helm-recentf-fuzzy-match    t)
   (helm-ff-fuzzy-matching      t)
   (helm-mode-fuzzy-match       t)
   :bind
   (("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    ("C-x C-f" . helm-find-files)
    ("C-h a"   . helm-apropos)
    ("C-x C-b" . helm-buffers-list)
    ("M-s o"   . helm-occur))
   :ensure t)

(use-package helm-mode
  :config
  (helm-mode 1)
  :diminish helm-mode)

(use-package helm-projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (setq projectile-mode-line
    (quote
      (:eval
        (format " Proj[%s]"
	     (projectile-project-name)))))
  (helm-projectile-on)
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-directory "~/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :config
  (setq org-msp-directory (concat org-directory "/msp"))
  (setq org-personal-file (concat org-directory "/personal.org"))
  (setq org-sysadmin-file (concat org-directory "/sysadmin.org"))
  (setq org-golem-file (concat org-msp-directory "/golem.org"))
  (setq org-templates-directory (concat org-directory "/templates"))
  (setq org-agenda-files (list org-msp-directory org-personal-file org-sysadmin-file))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-archive-location (concat org-directory "/archive/%s_archive::"))
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t)
	(perl . t)
	(python . t)))
  (setq org-todo-keywords
	'((type "TODO(t)"
		"BACKBURNER(b)"
		"WAITING(w)"
		"IN PROGRESS(p)"
		"NEXT(n)"
		"|" "DONE(d)" "CANCELED(c)")
	  (sequence "QUESTION(q)" "|" "ANSWER(a)")
	  (sequence "DESIGNING(D)"
		    "CODING(C)"
		    "IN REVIEW(R)"
		    "VALIDATING(V)"
		    "|" "DEPLOYED(Y)" "CLOSED(L)")
	  (sequence "TO REVIEW" "CURRENTLY REVIEWING" "|" "REVIEW DONE")))
  (setq org-capture-templates
	'(("r" "Redmine ticket" entry (file org-golem-file)
	   (file "~/org/templates/redmine_ticket"))
	  ("R" "Review ticket" entry (file org-golem-file)
	   (file "~/org/templates/review_ticket"))
	  ("t" "Todo" entry (file "") "* TODO %?  %^G\n  %i\n")))
  (defface org-backburner-face
    '((default (:foreground "DarkMagenta" :weight bold)))
    "Face for back burner projects"
    :group 'matt-org-faces)
  (defface org-waiting-face
    '((default (:foreground "DarkCyan" :weight bold)))
    "Face for stuff I'm waiting on"
    :group 'matt-org-faces)
  (defface org-in-progress-face
    '((default (:foreground "Darkgoldenrod3" :weight bold)))
    "Face for stuff I'm waiting on"
    :group 'matt-org-faces)
  (defface org-canceled-face
    '((default (:foreground "Firebrick" :weight bold)))
    "Face for canceled items"
    :group 'matt-org-faces)
  (defface org-question-face
    '((default (:foreground "MediumSeaGreen" :weight bold)))
    "Face for canceled items"
    :group 'matt-org-faces)
  (setq org-todo-keyword-faces
	'(("BACKBURNER" . org-backburner-face)
	  ("WAITING" . org-waiting-face)
	  ("IN PROGRESS" . org-in-progress-face)
	  ("CANCELED" . org-canceled-face)
	  ("QUESTION" . org-question-face)))
  (setq org-log-done 'time)
  (setq org-log-done 'note)
  :ensure t
  :pin org)

;; move these into :bindings section of use-package form
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stop editor from breaking line into fields
(setq comint-use-prompt-regexp t)

;; Add colors when running the shell
(use-package ansi-color
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; directory tracking in shell-mode
;; we want to use the dirtrack package rather than shell-dirtrack-mode
(use-package dirtrack
  :hook (shell-mode . dirtrack-mode)
  :init
  (setq dirtrack-list '("^.*?:\\(.*\\)\n" 1 nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; theme
(use-package hc-zenburn-theme
  :config
  (load-theme 'hc-zenburn t)
  :ensure t)

;; diminish minor modes that we don't care about
(diminish 'auto-revert-mode)
(diminish 'abbrev-mode)

(use-package smart-mode-line
  :init
  (setq sml/theme 'dark) ;; can use one of 'dark, 'light or 'respectful
  (setq sml/name-width 35)
  (setq sml/mode-width 'full)
  :config
  (sml/setup))

(use-package highlight-current-line
  :custom
  (highlight-current-line-globally t)
  (highlight-current-line-high-faces nil)
  (highlight-current-line-whole-line t)
  :custom-face
  ;; (highlight-current-line-face ((t (:background "gray18"))))
  :config
  (global-hl-line-mode t)
  :ensure t)

(use-package font-lock
  :custom
  (font-lock-maximum-decoration t)
  :config
  (global-font-lock-mode t)
  (global-hi-lock-mode nil))

(use-package jit-lock
  :custom
  (jit-lock-contextually t)
  (jit-lock-stealth-verbose t))

(use-package mic-paren
  :custom
  (paren-sexp-mode t)
  :config
  (paren-activate)
  :ensure t)

(use-package rainbow-delimiters
  :hook ((prog-mode LaTeX-mode) . rainbow-delimiters-mode)
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  text-editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require cases to match when replacing
(setq case-replace t)

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-list-command "list")
  :hook (prog-mode . flyspell-prog-mode)
  ;; currently disabled b/c it binds C-,  which we want for embrace-commander
  :disabled)

;; make > into a comment character in text mode
;; (useful for quoting stuff in email replies, eg)
(add-hook 'text-mode-hook (lambda ()
            (set (make-local-variable 'comment-start) ">")))

;; explicitly set % for comments in LaTeX mode
;; (otherwise our setting for text-mode overrides it)
(add-hook 'LaTeX-mode-hook (lambda ()
            (set (make-local-variable 'comment-start) "%")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end text-editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  custom functions (move to other file?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-and-kill-ring-save ()
  "Copy the current region into the kill ring and then comment it out."
  (interactive)
  (save-excursion
    (kill-ring-save (region-beginning) (region-end))
    (comment-region (region-beginning) (region-end))))
;; (global-set-key (kbd "C-c c") 'comment-and-kill-ring-save)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeX setup moved to separate file since it's bulky
;; (load "tex-config.el")

;; Python (need to convert to use-package)
(use-package isortify
  :hook (python-mode . isort-mode))

;; (use-package elpy
;;   :custom
;;   (py-shell-name "Python shell")
;;   :config
;;   (elpy-enable))

(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter-args "-i")
(setq py-shell-name "python3")
(setq elpy-shell-echo-input nil)
(setq elpy-shell-display-buffer-after-send t)

;; Perl
(defalias 'perl-mode 'cperl-mode)

;; PHP
(use-package web-mode
  :ensure t
  :mode "\\.php\\'"
  :custom
  (web-mode-code-indent-offset 2))

;; JavaScript
(use-package js
  :ensure t
  :custom
  (js-indent-level 2))

(use-package nodejs-repl
  :ensure t)

;; auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; vagrant (for MSP work)
(use-package vagrant
  :ensure t)

(use-package vagrant-tramp
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'split-window-horizontally)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s c") 'how-many)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here

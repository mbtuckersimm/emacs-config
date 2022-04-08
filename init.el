;;; init.el --- Summary
;;; Author: Matthew Tucker-Simmons

;;; Commentary:
;;; This is only here to stop flycheck from giving me a warning.
;;; Maybe I should tell flycheck not to warn me about that instead.

;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  basic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set high garbage-collection threshold to speed startup
(setq gc-cons-threshold (* 100 1000 1000))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(server-start)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; this stuff changes the locations for autosaves and backups
(setq backup-directory-alist
      `((".*" . ,"/home/matthew/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"/home/matthew/.emacs.d/autosaves/" t)))

(add-to-list 'load-path "/home/matthew/.emacs.d/lisp/")

;; settings that only apply in windowed mode
(when (display-graphic-p)
  (tool-bar-mode -1)
  (mouse-wheel-mode t)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(line-number-mode 1)
(column-number-mode 1)
(transient-mark-mode 1)
(display-time-mode 1)
(size-indication-mode t)
(global-hl-line-mode t)
(setq blink-cursor-blinks 0)
(setq require-final-newline t)
(setq tab-width 4)

;; move through mark ring with C-<SPC> after one initial C-u C-<SPC>
(setq set-mark-command-repeat-pop t)

;; displays filename (or buffername if no filename) in title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end basic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

(use-package paradox
  :config
  (paradox-enable)
  :ensure t)
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
  :defer t
  :init
  (add-to-list 'load-path "/home/matthew/.emacs.d/snippets/")
  :config
  (yas-global-mode 1)
  :ensure t)

;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup)
;;   :ensure t)

(use-package electric
  :init (electric-pair-mode t))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :ensure t)

(use-package whitespace
  :custom
  (whitespace-style '(face trailing))
  :hook ((prog-mode LaTeX-mode) . whitespace-mode)
  :diminish whitespace-mode)

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package all-the-icons
  :ensure t)
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
  :bind ("C-x g" . magit-status)
  :init
  (setq auto-revert-check-vc-info t)
  (diminish 'auto-revert-mode)
  :custom
   (magit-repository-directories
    '(("~/.emacs.d" . 0)
      ("~/msp/code" . 4)
      ("~/dotfiles" . 0)))
  :ensure t)

;; reload files when switching git branches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'helm)
;; (use-package helm-config
  ;; :init
  ;; (custom-set-variables '(helm-command-prefix-key "C-;"))
  ;; :config
  ;; TODO: it seems helm-command-map is outdated
  ;; (bind-keys :map helm-command-map
  ;;            ("a" . helm-ag)
  ;;            ("o" . helm-occur)
  ;;            ("y" . yas-insert-snippet)
  ;;            ("g" . helm-google-suggest)))

(use-package helm
  :custom
   (helm-split-window-default-side 'right) ;; open helm buffer in another window
   (helm-candidate-number-limit 200) ; limit the number of displayed canidates
   (helm-M-x-requires-pattern   0)   ; show all candidates when set to 0
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

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config
  (projectile-mode)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (setq projectile-mode-line-prefix " Proj")
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-directory "~/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-hide-leading-stars t)
  (org-adapt-indentation t)
  :config
  (setq org-msp-directory (concat org-directory "/msp"))
  (setq org-personal-file (concat org-directory "/personal.org"))
  (setq org-sysadmin-file (concat org-directory "/sysadmin.org"))
  (setq org-golem-file (concat org-msp-directory "/golem.org"))
  (setq org-golem-admin-file (concat org-msp-directory "/golem-admin.org"))
  (setq org-templates-directory "~/.emacs.d/org_templates")
  (setq org-agenda-files (list org-msp-directory org-personal-file org-sysadmin-file))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-archive-location (concat org-directory "/archive/%s_archive::"))
  (setq org-todo-keywords
	'((type "TODO(t)"
		"BACKBURNER(b)"
		"WAITING(w)"
		"IN PROGRESS(p)"
		"NEXT(n)"
		"|" "DONE(d)" "CANCELED(c)")
	  (sequence "QUESTION(q)" "|" "ANSWER(a)")))
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
  :pin gnu)

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end org
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
(diminish 'abbrev-mode)

(use-package smart-mode-line
  :custom
  (sml/theme 'dark) ;; can use one of 'dark, 'light or 'respectful
  (sml/name-width 35)
  (sml/mode-width 'full)
  :config
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/org/" ":Org:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/msp/" ":MSP:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:Doc:msp/" ":MSP:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/editflow/ef/" ":orcus:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/editflow/ef-devenv/" ":devenv:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/editflow/ef-local/" ":ef-local:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/ef/kgb_site/" ":KGB:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/ef/ef-api_site/" ":ef-api:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/ef/corkboard_site/" ":CorkBoard:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/ef/corkthrower_site/" ":CorkThrower:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/ef/casting_site/" ":Casting:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/publish/copykit/" ":CopyKit:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/publish/prodkit/" ":ProdKit:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/publish/publicast_site/" ":PubliCast:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/office/metawiz_site/" ":MetaWiz:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/office/newmsp/" ":newMSP:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/office/plutonium/" ":Plutonium:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/office/skorptoys/" ":SkorpToys:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:MSP:code/web/msp_org_site" ":msp_org:") t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end text-editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeX setup moved to separate file since it's bulky
;; (load "tex-config.el")

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  ;; Fix conflict between web-mode and editorconfig; cribbed from
  ;; https://github.com/editorconfig/editorconfig-emacs#editorconfig-after-apply-functions
  (add-hook 'editorconfig-after-apply-functions
            (lambda (props) (setq web-mode-block-padding 0)))
  :ensure t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c f")
  (setq read-process-output-max (* 1024 1024))
  :custom
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 3000)
  (lsp-intelephense-licence-key "00PATU5JSL4WFDD")
  ;; (lsp-log-io t)  ;; for debugging only
  :hook ((web-mode . lsp)
         (javascript-mode . lsp)
         (CPerl-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)  ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)  ;; M-?
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor nil)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-code-actions t)
  :commands lsp-ui-mode
  :ensure t)

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)  ;; c f g a, C-M-.
  :commands helm-lsp-workspace-symbol
  :ensure t)

;; just needed for the header-line icons, really
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :ensure t)


(use-package elpy
  :defer t
  :init
  (elpy-enable)
  :custom
  (elpy-rpc-virtualenv-path 'current)
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  ;; disable flymake since we use flycheck instead
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :ensure t)

;; (setq python-shell-interpreter "python3")
;; (setq elpy-rpc-python-command "python3")
;; (setq python-shell-interpreter-args "-i")
;; (setq py-shell-name "python3")
;; (setq elpy-shell-echo-input nil)
;; (setq elpy-shell-display-buffer-after-send t)

;; Perl
(defalias 'perl-mode 'cperl-mode)

;; PHP
(use-package web-mode
  :ensure t
  :mode "\\.php\\'"
  :custom
  (web-mode-enable-auto-indentation nil)
)

;; JavaScript
(use-package js
  :defer t
  :ensure t)

;; auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; shell script mode
(add-to-list 'auto-mode-alist '("\\.fish\\'" . shell-script-mode))
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
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
;; I hit this all the time when my browser has focus but I think emacs does:
(global-unset-key (kbd "<C-next>"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reset garbage-collection threshold
;; commented b/c it's better to have a larger threshold for lsp-mode
;; (setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here

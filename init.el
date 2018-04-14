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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end basic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
         		  ("melpa" . "http://melpa.milkbox.net/packages/")
	        	  ("melpa-stable" . "https://stable.melpa.org/packages/")
		          ("org" . "http://orgmode.org/elpa/")))

;; load self-installed packages
(package-initialize)

(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  packages with minimal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-="   . er/expand-region)
	 ("C-; p" . er/mark-inside-pairs))
  :config
  (setq shift-select-mode nil))
;;(require 'expand-region)
  ;; (global-set-key (kbd "C-=") 'er/expand-region)
  ;; (global-set-key (kbd "C-; p") 'er/mark-inside-pairs)
  ;; (setq shift-select-mode nil)
;; workaround for expand-region to interact nicely with transient-mark-mode

(use-package embrace
  :bind ("C-," . embrace-commander))
;; (require 'embrace)
;; (global-set-key (kbd "C-,") #'embrace-commander)

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
;; (require 'ace-jump-mode)
;; (global-set-key (kbd "C-.") 'ace-jump-mode)

(use-package yasnippet
  :init
  (add-to-list 'load-path "/home/matthew/.emacs.d/snippets/")
  :config
  (yas-global-mode 1))
;; (add-to-list 'load-path "/home/matthew/.emacs.d/snippets/")
;; (require 'yasnippet)
;; (yas-global-mode 1)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

(use-package autopair
  :config
  (autopair-global-mode)
  (setq autopair-autowrap t))
;; (require 'autopair)
;; (autopair-global-mode)
;; (setq autopair-autowrap t)

(use-package flycheck
  :init
  (global-flycheck-mode))

;;(add-hook 'after-init-hook #'global-flycheck-mode)
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
    ("M-s o"   . helm-occur)))

(use-package helm-mode
  :config
  (helm-mode 1))

(use-package helm-projectile
  :config
  (projectile-mode)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :custom
  (org-agenda-restore-windows-after-quit t)
  :config
  (setq org-agenda-files '("/home/matthew/msp/orgmode/"))
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (sh . t)
	(shell . t)
	(perl . t)
	(python . t)))
  (setq org-todo-keywords '((sequence "TODO(t)" "BACKBURNER(b)" "WAITING(w)" "IN PROGRESS(p)""|" "DONE(d)")))
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
  (setq org-todo-keyword-faces
	'(("BACKBURNER" . org-backburner-face)
	  ("WAITING" . org-waiting-face)
	  ("IN PROGRESS" . org-in-progress-face)))
  (setq org-log-done 'time)
  (setq org-log-done 'note)
  :defer t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "\C-c a") 'org-agenda)

;;(require 'org)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "\C-c a") 'org-agenda)
;; (setq org-agenda-files '("/home/matthew/msp/orgmode/"))
;; (setq org-agenda-restore-windows-after-quit t)

;; languages to allow for code block execution
;; (org-babel-do-load-languages
;;       'org-babel-load-languages
;;       '((emacs-lisp . t)
;;         (sh . t)
;; 	(shell . t)
;; 	(perl . t)
;; 	(python . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stop editor from breaking line into fields
(setq comint-use-prompt-regexp t)

;; Add colors when running the shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

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
;;  global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'split-window-horizontally)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s c") 'how-many)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'atom-one-dark t)
;; had this setting in custom.el that helps load the dark solarized theme
; '(frame-background-mode (quote dark))
;; (add-to-list 'custom-theme-load-path "/home/matthew/.emacs.d/themes/solarized/")
;; (load-theme 'solarized t)

(sml/setup)
(setq sml/theme 'dark) ;; can use one of 'dark, 'light or 'respectful
(setq sml/name-width 35)
(setq sml/mode-width 'full)

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

(require 'highlight-current-line)
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)

(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

(require 'mic-paren)
(paren-activate)
(setq paren-sexp-mode t)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  text-editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require cases to match when replacing
(setq case-replace t)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; make > into a comment character in text mode
;; (useful for quoting stuff in email replies, eg)
(add-hook 'text-mode-hook (lambda ()
            (set (make-local-variable 'comment-start) ">")))

;; explicitly set % for comments in LaTeX mode too
;; otherwise the previous setting overrides it
(add-hook 'LaTeX-mode-hook (lambda ()
            (set (make-local-variable 'comment-start) "%")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end text-editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  custom functions (move to other file?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-and-kill-ring-save ()
  "Copy the current region into the kill ring and then comment it out"
  (interactive)
  (save-excursion
    (kill-ring-save (region-beginning) (region-end))
    (comment-region (region-beginning) (region-end))))
(global-set-key (kbd "C-c c") 'comment-and-kill-ring-save)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  language-specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeX setup moved to separate file since it's bulky
(load "tex-config.el")

;; Python

(require 'isortify)
(add-hook 'python-mode-hook 'isort-mode)

(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter-args "-i")
(setq py-shell-name "Python shell")
(setq elpy-shell-echo-input nil)
(setq elpy-shell-display-buffer-after-send t)

;; Perl
(defalias 'perl-mode 'cperl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end language-specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

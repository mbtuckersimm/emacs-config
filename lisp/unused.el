;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  shell/comint modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shell
  :custom
  (explicit-shell-file-name "/bin/bash"))

stop editor from breaking line into fields
(setq comint-use-prompt-regexp t)

Add colors when running the shell
(use-package ansi-color
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

directory tracking in shell-mode
we want to use the dirtrack package rather than shell-dirtrack-mode
(use-package dirtrack
  :hook (shell-mode . dirtrack-mode)
  :init
  (setq dirtrack-list '("^.*?:\\(.*\\)\n" 1 nil)))

(use-package sqlup-mode
  :hook ((sql-interactive-mode . sqlup-mode)
         (sql-mode . sqlup-mode))
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  end shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

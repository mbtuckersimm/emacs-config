(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Ok" "okular --unique %o#src:%n`pwd`/%b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Ok")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(bibtex-entry-format (quote (opts-or-alts required-fields)))
 '(bibtex-generate-url-list
   (quote
    ((("url" . ".*:.*"))
     (("doi" . "10\\.[0-9]+/.+")
      "http://dx.doi.org/%s"
      ("doi" ".*" 0))
     (("mrkey" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrkey" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("mrnumber" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://mathscinet.ams.org/mathscinet-getitem?mr=%s"
      ("mrnumber" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("arxiv" . "\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\|[-a-z]+/[0-9]\\{7\\}\\|[-a-z]+\\.[a-z]\\{2\\}/[0-9]\\{7\\}\\)\\(v[0-9]+\\)?")
      "http://arxiv.org/abs/%s"
      ("arxiv" ".*" 0))
     (("zblnumber" . "\\([0-9]\\{4\\}\\.[0-9]\\{5\\}\\|[0-9]\\{8\\}\\)")
      "http://zbmath.org/?q=an:%s"
      ("zblnumber" ".*" 0)))))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "7b4a48ba440cf63b7e9f3183f085a2f306ba8405b7c4c5f74126349cf8fe141f" "c42ef18cfebf144a7c39ec8a95dd268dbe8276b858e793b0a2e8aa3d20e6edb1" "602cf380fedc6ce52b135ffd1be4bfedc96aafd80645844817f3de0b12e6e077" "d6115669a9d0bdde235c33549bab6927910b5f7162a1c618548b59285bdd58f1" "fc43b3ef4682a5826485515cccef323ac71219e0698bed7b8ed806ffb93270df" "009bf2d575807e71e244c2223362bf48f348f05f7d2bf5ed23dc2991fca83ecf" "34543312860bbc58b2fcf4d24a9bdc5c114347f16903ac9d7ae70f3c44616a9e" "9a4aca5cddf34a7f70afe7b1057232d1b86c26daed1e1d7324ea804ba3911fcd" default)))
 '(fci-rule-color "#383838")
 '(helm-M-x-fuzzy-match t)
 '(helm-M-x-requires-pattern 0)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")))
 '(helm-boring-file-regexp-list
   (quote
    ("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.elc$" "\\.glo$" "\\.lot$" "\\.svn/\\|\\.svn$" "\\.hg/\\|\\.hg$" "\\.git/\\|\\.git$" "\\.bzr/\\|\\.bzr$" "CVS/\\|CVS$" "_darcs/\\|_darcs$" "_MTN/\\|_MTN$" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$")))
 '(helm-buffer-max-length 30)
 '(helm-buffers-end-truncated-string "…")
 '(helm-buffers-fuzzy-matching t)
 '(helm-buffers-truncate-lines t)
 '(helm-candidate-number-limit 200)
 '(helm-ff-fuzzy-matching t)
 '(helm-moccur-show-buffer-fontification t)
 '(helm-moccur-truncate-lines nil)
 '(helm-mode-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(helm-split-window-default-side (quote right))
 '(mh-variant "GNU Mailutils 2.99.98")
 '(org-agenda-restore-windows-after-quit t)
 '(package-selected-packages
   (quote
    (diminish company-php company-web web-mode flycheck highlight-current-line use-package isortify atom-dark-theme atom-one-dark-theme elpy material-theme helm-projectile ag helm-ag zen-and-art-theme hc-zenburn-theme zenburn-theme undo-tree unicode-fonts iedit wgrep-helm helm ace-jump-mode embrace org smart-mode-line rainbow-delimiters python-mode python-info pymacs palette mic-paren magit expand-region autopair auto-complete-auctex auctex ac-math)))
 '(py-split-window-on-execute t)
 '(pyvenv-tracking-ask-before-change t)
 '(safe-local-variable-values
   (quote
    ((eval show-all)
     (TeX-master . t)
     (eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1)))))
 '(send-mail-function nil)
 '(tool-bar-mode nil)
 '(whitespace-style (quote (face trailing))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal))))
 '(highlight-current-line-face ((t (:background "gray18"))))
 '(paren-face-match ((t (:foreground "medium aquamarine")))))

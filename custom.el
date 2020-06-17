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
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" "3788e589eb432e6a515d557cbeb8dc4eaca9e00ae54f932b4bd43ed78605532e" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" "a7928e99b48819aac3203355cbffac9b825df50d2b3347ceeec1e7f6b592c647" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "834cbeacb6837f3ddca4a1a7b19b1af3834f36a701e8b15b628cad3d85c970ff" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "7b4a48ba440cf63b7e9f3183f085a2f306ba8405b7c4c5f74126349cf8fe141f" "c42ef18cfebf144a7c39ec8a95dd268dbe8276b858e793b0a2e8aa3d20e6edb1" "602cf380fedc6ce52b135ffd1be4bfedc96aafd80645844817f3de0b12e6e077" "d6115669a9d0bdde235c33549bab6927910b5f7162a1c618548b59285bdd58f1" "fc43b3ef4682a5826485515cccef323ac71219e0698bed7b8ed806ffb93270df" "009bf2d575807e71e244c2223362bf48f348f05f7d2bf5ed23dc2991fca83ecf" "34543312860bbc58b2fcf4d24a9bdc5c114347f16903ac9d7ae70f3c44616a9e" "9a4aca5cddf34a7f70afe7b1057232d1b86c26daed1e1d7324ea804ba3911fcd" default)))
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(elpy-rpc-virtualenv-path (quote current))
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#383838")
 '(font-lock-maximum-decoration t)
 '(helm-M-x-fuzzy-match t t)
 '(helm-M-x-requires-pattern 0 t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`magit-")))
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
 '(indent-tabs-mode nil)
 '(jit-lock-contextually t)
 '(jit-lock-stealth-verbose t)
 '(js-indent-level 2)
 '(magit-repository-directories
   (quote
    (("~/.emacs.d" . 0)
     ("~/Documents/msp/code" . 4)
     ("~/dotfiles" . 0))))
 '(mh-variant "GNU Mailutils 2.99.98")
 '(neo-theme (quote icons))
 '(org-agenda-restore-windows-after-quit t t)
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path (quote file))
 '(package-selected-packages
   (quote
    (docker-compose-mode dockerfile-mode kaolin-themes color-theme-sanityinc-solarized solarized-theme markdown-mode elpy helm-swoop paradox all-the-icons neotree sqlup-mode org-bullets gnugo diminish company-php company-web web-mode flycheck highlight-current-line use-package isortify atom-dark-theme atom-one-dark-theme material-theme helm-projectile ag helm-ag zen-and-art-theme hc-zenburn-theme zenburn-theme unicode-fonts iedit wgrep-helm helm ace-jump-mode embrace org smart-mode-line rainbow-delimiters python-mode python-info pymacs palette mic-paren magit expand-region autopair auto-complete-auctex auctex ac-math)))
 '(paren-sexp-mode t)
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
 '(size-indication-mode t)
 '(sml/mode-width (quote full))
 '(sml/name-width 35)
 '(sml/theme (quote dark))
 '(tool-bar-mode nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 4)
 '(whitespace-style (quote (face trailing))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVuSansMono Nerd Font Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))

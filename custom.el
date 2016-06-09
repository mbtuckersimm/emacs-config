(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
      "http://www.ams.org/mathscinet-getitem?mr=%s"
      ("mrkey" "\\(mr\\)?\\([0-9]\\{1,8\\}\\)" 2))
     (("mrnumber" . "\\(mr\\)?[0-9]\\{1,8\\}")
      "http://www.ams.org/mathscinet-getitem?mr=%s"
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
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "7b4a48ba440cf63b7e9f3183f085a2f306ba8405b7c4c5f74126349cf8fe141f" "c42ef18cfebf144a7c39ec8a95dd268dbe8276b858e793b0a2e8aa3d20e6edb1" "602cf380fedc6ce52b135ffd1be4bfedc96aafd80645844817f3de0b12e6e077" "d6115669a9d0bdde235c33549bab6927910b5f7162a1c618548b59285bdd58f1" "fc43b3ef4682a5826485515cccef323ac71219e0698bed7b8ed806ffb93270df" "009bf2d575807e71e244c2223362bf48f348f05f7d2bf5ed23dc2991fca83ecf" "34543312860bbc58b2fcf4d24a9bdc5c114347f16903ac9d7ae70f3c44616a9e" "9a4aca5cddf34a7f70afe7b1057232d1b86c26daed1e1d7324ea804ba3911fcd" default)))
 '(fci-rule-color "#383838")
 '(frame-background-mode (quote dark))
 '(mh-variant "GNU Mailutils 2.99.98")
 '(org-agenda-files
   (quote
    ("/home/matthew/msp/gt/src/work/150511-MichaelLock/150511-MichaelLock.org" "/home/matthew/msp/gt/src/work/150204-Weiss/150204-Weiss.org" "/home/matthew/msp/gt/src/work/140414-Ching/140414-Ching.org" "/home/matthew/msp/gt/src/work/140320-Lanneau/140320-Lanneau.org" "/home/matthew/msp/gt/src/work/140203-Cohn/140203-Cohn.org" "/home/matthew/msp/gt/src/work/131212-Lytchak/131212-Lytchak.org" "/home/matthew/msp/agt/src/work/150621-Musin/150621-Musin.org" "/home/matthew/msp/agt/src/work/150428-Motegi/150428-Motegi.org" "/home/matthew/msp/agt/src/work/141130-Ducoulombier/141130-Ducoulombier.org" "/home/matthew/msp/agt/src/work/141008-Starkston/141088-Starkston.org" "/home/matthew/msp/agt/src/work/140915-Varisco/140915-Varisco.org" "/home/matthew/msp/agt/src/work/140818-Kim/140818-Kim.org" "/home/matthew/msp/agt/src/work/140812-Vera/140812-Vera.org" "/home/matthew/msp/warwick/warwick.org" "/home/matthew/msp/mspdoc/mspdoc.org")))
 '(safe-local-variable-values
   (quote
    ((eval show-all)
     (TeX-master . t)
     (eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1)))))
 '(send-mail-function nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 140 :width normal))))
 '(paren-face-match ((t (:foreground "medium aquamarine")))))

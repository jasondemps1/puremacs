
;; IBuffer for now? Consult later?
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ibuffer filters
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("org"     (or
                       (mode . org-mode)
                       (name . "^\\*Org Src")
                       (name . "^\\*Org Agenda\\*$")))
           ("tramp"   (name . "^\\*tramp.*"))
           ("emacs"   (or
                       (name . "^\\*scratch\\*$")
                       (name . "^\\*Messages\\*$")
                       (name . "^\\*Warnings\\*$")
                       (name . "^\\*Shell Command Output\\*$")
                       (name . "^\\*Async-native-compile-log\\*$")))
           ("ediff"   (name . "^\\*[Ee]diff.*"))
           ("vc"      (name . "^\\*vc-.*"))
           ("dired"   (mode . dired-mode))
           ("terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eshell-mode)))
           ("help"    (or
                       (name . "^\\*Help\\*$")
                       (name . "^\\*info\\*$")))
           ("news"    (name . "^\\*Newsticker.*"))
           ("gnus"    (or
                       (mode . message-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\*Group\\*")
                       (name . "^\\*Summary\\*")
                       (name . "^\\*Article\\*")
                       (name . "^\\*BBDB\\*")))
           ("chat"    (or
                       (mode . rcirc-mode)
                       (mode . erc-mode)
                       (name . "^\\*rcirc.*")
                       (name . "^\\*ERC.*"))))))

(add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups

(provide 'init-buffers)

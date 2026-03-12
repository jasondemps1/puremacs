
(use-package eglot
  :hook (('c-mode-hook 'eglot-ensure)
         ('c++-mode-hook 'eglot-ensure)
         ('c-or-c++-mode 'eglot-ensure)))

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))
;;              ("C-c ! t" . toggle-flymake-diagnostics-at-eol)))

(provide 'init-prog)

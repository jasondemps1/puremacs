
(use-package eglot
  :hook (('c-mode-hook 'eglot-ensure)
         ('c++-mode-hook 'eglot-ensure)
         ('c-or-c++-mode 'eglot-ensure)))

(provide 'init-prog)

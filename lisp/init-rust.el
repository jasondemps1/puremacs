
(add-hook 'rust-mode-hook 'eglot)

(use-package eglot
  :init
  ;; Enable clippy
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(provide 'init-rust)

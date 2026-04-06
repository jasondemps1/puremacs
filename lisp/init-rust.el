(if (and (treesit-available-p)
         (treesit-language-available-p 'rust))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :hook ((prog-mode . eglot-ensure)
         (before-save . eglot-format-buffer))
  :bind (("C-c i e" . eglot)
         ("C-c i i" . eglot-find-implementation)
         ("C-c i r" . eglot-rename)
         ("C-c i m" . eglot-menu)
         ("C-c i f" . eglot-format-buffer)
         ("C-c i h" . eglot-inlay-hints-mode)
         ("C-c i c" . eglot-code-actions))
  :init
  (setq eglot-autoshutdown t
        eglot-confirm-server-edits nil
        eglot-report-progress t
        eglot-extend-to-xref t
        eglot-autoreconnect t)
  :config
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:check (:command "clippy")
                                         :cargo (:sysroot "discover"
                                                          :features "all"
                                                          :buildScripts (:enable t))
                                         :diagnostics (:disabled ["macro-error"])
                                         :procMacro (:enable t))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))
  ;; Enable clippy
;;  (add-to-list 'eglot-server-programs
;;               '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(provide 'init-rust)


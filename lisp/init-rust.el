
;(add-hook 'rust-mode-hook 'eglot-ensure)
;(add-hook 'rust-ts-mode-hook 'eglot-ensure)

(use-package eglot
  :ensure nil
  :hook ((rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (before-save . eglot-format-buffer))
  :bind (("C-c i e" . eglot)
         ("C-c i i" . eglot-find-implementation)
         ("C-c i r" . eglot-rename)
         ("C-c i m" . eglot-menu)
         ("C-c i f" . eglot-format-buffer)
         ("C-c i h" . eglot-inlay-hints-mode))
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

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless)))))

(provide 'init-rust)


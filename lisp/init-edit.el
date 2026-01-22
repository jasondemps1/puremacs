(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(use-package iedit
  :bind (:map global-map
              ("C-;" . iedit-mode)
              ("C-x r RET" . iedit-rectangle-mode)
              :map isearch-mode-map
              ("C-;" . iedit-mode-from-isearch)
              :map esc-map
              ("C-;" . iedit-execute-last-modification)
              :map help-map
              ("C-;" . iedit-mode-toggle-on-fuction)))

(provide 'init-edit)

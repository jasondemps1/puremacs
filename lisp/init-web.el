
(use-package web-mode
  :ensure t
  :mode
  ("\\.svelte\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.css\\'" . web-mode)
  ("\\.jsx?\\'" . web-mode)
  ("\\.tsx?\\'" . web-mode)
  ("\\.json\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :init
  (setq web-mode-engines-alist
        '(("svelte" . "\\.svelte\\'"))))

(provide 'init-web)

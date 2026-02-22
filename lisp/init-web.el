
(use-package web-mode
  :ensure t
  :mode
  ("\\.svelte\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.css\\'" . web-mode)
  ("\\.jsx?\\'" . web-mode)
  ("\\.tsx?\\'" . web-mode)
  ("\\.json\\'" . web-mode)
  ("\\.vue\\'" . web-mode))

(provide 'init-web)

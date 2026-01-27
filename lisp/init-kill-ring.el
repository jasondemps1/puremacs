
(setq kill-ring-max 200)

(setq save-interprogram-paste-before-kill t)

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(provide 'init-kill-ring)

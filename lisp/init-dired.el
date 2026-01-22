

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	      ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
	dired-recursive-copies 'always)

  ;; Show directories first
  (setq dired-listing-switches "-alh --group-directories-first"))

(provide 'init-dired)

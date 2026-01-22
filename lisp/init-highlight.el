

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
	 ("M-n" . symbol-overlay-next)
         ("M-p" . symbol-overlay-prev)
	 ("M-N" . symbol-overlay-switch-forward)
	 ("M-P" . symbol-overlay-switch-backward)
	 ("M-C" . symbol-overlay-remove-all)
	 ([M-f2] . symbol-overlay-mode))
  :bind-keymap ("M-s s" . symbol-overlay-map)
  :hook ((prog-mode))
  :custom (symbol-overlay-idle-time 0.3)
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1))))

(provide 'init-highlight)

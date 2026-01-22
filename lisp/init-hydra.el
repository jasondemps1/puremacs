(use-package hydra
  :defines (consult-imenu-config posframe-border-width)
  :functions childframe-completion-workable-p
  :hook (;;(emacs-lisp-mode . hydra-add-imenu)
	 (after-load-theme . hydra-set-posframe-appearance))
  :init
  (with-eval-after-load 'consult-imenu
    (setq consult-imenu-config
	  '((emacs-lisp-mode :toplevel "Functions"
			     :types ((?f "Functions" font-lock-function-name-face)
				     (?h "Hydras" font-lock-constant-face)
				     (?m "Macros" font-lock-function-name-face)
				     (?p "Packages" font-lock-constant-face)
				     (?t "Types" font-lock-type-face)
				     (?v "Variables" font-lock-variable-name-face))))))
  (defun hydra-set-posframe-appearance ()
    "Set appearance of hydra."
    (when (childframe-completion-workable-p)
      (setq hydra-hint-display-type 'posframe)
      (setq hydra-posframe-show-params
	    `(:left-fringe 8
              :right-fringe 8
              :internal-border-width ,posframe-border-width
              :internal-border-color ,(face-background 'posframe-border nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center-near-bottom))))
  (hydra-set-posframe-appearance))

(provide 'init-hydra)

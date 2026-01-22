(defcustom personal-completion-style 'childframe
  "Completion display style."
  :group 'personal
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(provide 'init-custom)
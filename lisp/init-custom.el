(defcustom personal-completion-style 'childframe
  "Completion display style."
  :group 'personal
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

;; Template pattern for custom-file, since this file is shared across machines, but we dont want the files tracked
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (let ((template (expand-file-name "custom.el.template" user-emacs-directory)))
    (when (file-exists-p template)
      (copy-file template custom-file))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-custom)

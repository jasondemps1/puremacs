
(cond
 ((eq system-type 'windows-nt)
  (set-frame-font "BlexMono Nerd Font Mono SemiBol 12" nil t))
 ((eq system-type 'gnu/linux)
  (set-frame-font "BlexMono Nerd Font Mono SemiBold 12" nil t))
 ((eq system-type 'darwin)
  (set-frame-font "BlexMono Nerd Font Mono SemiBold 16" nil t)))

;;(print (font-family-list))
(use-package dracula-theme
  :init
  (load-theme 'dracula t))

(provide 'init-style)

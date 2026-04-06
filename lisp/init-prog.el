
(use-package eglot
  :hook (('c-mode-hook 'eglot-ensure)
         ('c++-mode-hook 'eglot-ensure)
         ('c-or-c++-mode 'eglot-ensure)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-auto-trigger ".")
  (corfu-on-exact-match 'insert)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary t)
  :config
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; show documentation next to completion
  ;; sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-prefixes))
  (completion-ignore-case t)
  
  ;;(completion-styles '(orderless basic))
  (completion-styles '(basic substring initials orderless))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless)))))

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c p n" . flymake-goto-next-error)
              ("C-c p p" . flymake-goto-prev-error)
              ("C-c p l" . flymake-show-buffer-diagnostics)))
;;              ("C-c ! t" . toggle-flymake-diagnostics-at-eol)))

(defun jd/toggle-eldoc-buffer ()
  (interactive)
  (let* ((buf (eldoc-doc-buffer))
         (win (get-buffer-window buf)))
    (if win
        (delete-window win)
      (display-buffer-in-side-window buf
                                     '((side . right)
                                       (slot . 0)
                                       (window-width . 0.33)
                                       (preserve-size . (t . nil)))))))

(global-set-key (kbd "C-c d") #'jd/toggle-eldoc-buffer)

(provide 'init-prog)


(use-package cc-mode
  :init (setq-default c-basic-offset 4))

(use-package c-ts-mode
  :init (setq c-ts-mode-indent-offset 4)
  (when (boundp 'major-mode-remap-alist)
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))))

;; Associate '.cxx' with C++
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
(provide 'init-c)


;; Optimization
(setq w32-get-true-file-attributes nil   ; decrease file IO workload
      w32-use-native-image-API t         ; use native w32 API
      w32-pipe-read-delay 0              ; faster IPC
      w32-pipe-buffer-size 65536)       ; read more at a time (64K, was 4K)

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x100000)  ; 1MB

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init (setq gcmh-idle-delay 'auto
              gcmh-auto-idle-delay-factor 10
              gcmh-high-cons-threshold #x4000000)) ; 64MB

(setq visible-bell t
      inhibit-compacting-font-caches t
      make-backup-files nil ; Forbid backup files
      auto-save-default nil ; Disable auto-save (may re-enable in future)
      delete-by-moving-to-trash t
      uniquify-buffer-name-style 'post-forward-angle-brackets
      word-wrap-by-category t)

;; Asynchronous processing
(use-package async
  :diminish (async-bytecomp-package-mode dired-async-mode)
  :functions (async-bytecomp-package-mode dired-async-mode)
  :init
  ;;(unless sys/win32p
   ;; (async-bytecomp-package-mode 1))
  (dired-async-mode 1))

;; Child frame
(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

;; Global keybindings
(bind-keys ("s-r"     . revert-buffer-quick)
           ("C-x K"   . delete-this-file)
           ("C-c C-l" . reload-init-file))

(setq use-short-answers t)

;; For default text mode
(setq-default major-mode 'text-mode
	      fill-column 80
	      tab-width 4
	      indent-tabs-mode nil) ;; Permanently indent with spaces, never with tabs

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'init-base)

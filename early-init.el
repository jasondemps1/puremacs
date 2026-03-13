;; Defer garbage collection further back in the startup process
;;(if noninteractive  ; in CLI sessions
;;    (setq gc-cons-threshold #x8000000   ; 128MB
;;          ;; Backport from 29 (see emacs-mirror/emacs@73a384a98698)
;;          gc-cons-percentage 1.0)
;;  (setq gc-cons-threshold most-positive-fixnum))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; To speedup the Emacs windows, reducing the count on searching `load-path'
(when (eq system-type 'windows-nt)
  (setq load-suffixes '(".elc" ".el")) ;; to avoid searching .so/.dll
  (setq load-file-rep-suffixes '(""))) ;; to avoid searching *.gz

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Explicitly set the preferred coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better Window Management handling
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
            (concat "Emacs - " (buffer-name))))))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; Prevent flash of unstyled mode line
;; (setq mode-line-format nil)

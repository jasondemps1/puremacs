;;
;; Speed up Startup Process
;;

;; Optimize Garbage Collection for Startup
;; (setq gc-cons-threshold most-positive-fixnum)

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;
;; Configure Load Path
;;

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Add subdirectories inside "site-lisp" to `load-path`
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

;; UI

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))


(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)))

;; Key Modifiers
(cond
 ((eq system-type 'windows-nt)
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super     ; Left Windows key
        w32-apps-modifier 'hyper)       ; Menu/App key
  (w32-register-hot-key [s-t]))
 ((eq window-system 'mac)
  ;; Compatible with Emacs Mac port
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)
  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super z)] #'undo)))

;; Keybindings
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

(global-visual-line-mode t)

(require 'init-custom)
(require 'init-funcs)

(require 'init-package)
(require 'init-base)
(require 'init-hydra)
(require 'init-window)
(require 'init-dired)
(require 'init-kill-ring)

(require 'init-style)
(require 'init-highlight)
(require 'init-org)

(require 'init-edit)
(require 'init-snippet)
(require 'init-vcs)

(require 'init-dap)
(require 'init-c)
(put 'scroll-left 'disabled nil)

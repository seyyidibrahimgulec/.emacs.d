;; Install straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; To keep folders clean
(use-package no-littering)
(setq create-lockfiles nil)

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-snazzy t))

(set-face-attribute 'default nil :font "Fira Code")
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(setq user-full-name "Seyyid İbrahim Güleç"
      user-mail-address "seyyidibrahimgulec@gmail.com")

(setq inhibit-startup-message t) ; Don't show the splash screen
(setq ring-bell-function 'ignore) ; prevent beep sound.
(setq require-final-newline t) ; Add new line end of the file
(setq truncate-lines t)

;; Modes
(global-display-line-numbers-mode -1)
(column-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(blink-cursor-mode -1)
;; (setq global-auto-revert-non-file-buffers t)
;; (recentf-mode 1)
;; (savehist-mode 1)
;; (setq history-length 25)
;; (save-place-mode 1)

;; Move custom variables to seperate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Macos Key Bindings
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Some Extra Keybindings
;; source: spacemacs' better default layer
(defun backward-kill-word-or-region ()
  "Calls `kill-region' when a region is active and
	  `backward-kill-word' otherwise."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'backward-kill-word-or-region)

;; Use shell-like backspace C-h, rebind help to C-?
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-?") 'help-command)


(use-package beacon
  :config
  (beacon-mode 1))

(use-package git-link
  :commands git-link)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :bind*
  (("C-M-n" . mc/mark-next-like-this)
   ("C-M-p" . mc/mark-previous-like-this)
   ("C-M-S-n" . mc/skip-to-next-like-this)
   ("C-M-S-p" . mc/skip-to-previous-like-this)
   ("C-S-n" . mc/unmark-previous-like-this)
   ("C-S-p" . mc/unmark-next-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

;; Expand Region
(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil
	expand-region-subword-enabled t)
  :bind (("C-t" . er/expand-region)))

;; Projectile
(use-package projectile
  :config (projectile-mode)
  :demand t)

;; Counsel Projectile
(use-package counsel-projectile
  :after projectile
  :bind (("C-x f" . counsel-projectile))
  :config
  (counsel-projectile-mode))

(use-package ivy
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-e" . ivy-alt-done))
  :init
  (ivy-mode 1)
  :config
  ;; Don't start searches with ^
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel)

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(use-package winner
  :bind
  (("M-u" . winner-undo)
   ("M-U" . winner-redo))
  :config
  (winner-mode))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t))

(use-package page-break-lines)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package magit
  :commands magit)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package lsp-mode
  :commands lsp)

(setq lsp-ui-doc-enable nil)

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package company
  :custom
  (company-idle-delay 0))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

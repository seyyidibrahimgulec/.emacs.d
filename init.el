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

;; (set-face-attribute 'default nil :font "Fira Code")
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(setq user-full-name "Seyyid İbrahim Güleç"
      user-mail-address "seyyidibrahimgulec@gmail.com")

(setq inhibit-startup-message t) ; Don't show the splash screen
(setq ring-bell-function 'ignore) ; prevent beep sound.
(setq require-final-newline t) ; Add new line end of the file
(setq truncate-lines t)
(setq-default fill-column 80
              sentence-end-double-space nil
              indent-tabs-mode nil  ; Use spaces instead of tabs
              tab-width 4)

;; Modes
(global-display-line-numbers-mode -1)
(column-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(blink-cursor-mode -1)
(delete-selection-mode 1)
(electric-pair-mode 1)
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
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-subword-enabled t)
  :bind (("C-t" . er/expand-region)))

;; Projectile
(use-package projectile
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; Counsel Projectile
(use-package counsel-projectile
  :after projectile
  :bind (("C-x f" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package ivy
  :bind (("C-s" . swiper))
  ;; :map ivy-minibuffer-map
  ;; ("TAB" . ivy-alt-done)
  ;; ("C-e" . ivy-alt-done))
  :init
  (ivy-mode 1)
  :custom-face
  (ivy-current-match ((t (:extend t))))
  :config
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line) ;; to extend ivy line
  :custom
  (ivy-format-function 'ivy-format-function-line)
  (ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

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

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

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

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package magit
  :commands magit)

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :none) ;; To disable default lsp flycheck
  (lsp-file-watch-threshold 10000))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred))

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package python
  :straight (:type built-in))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :custom
  (flycheck-checker-error-threshold 1000))

(use-package company
  :custom
  (company-idle-delay 0))

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

(use-package turkish
  :commands turkish-mode turkish-correct-region turkish-asciify-region)

(use-package sozluk
  :straight (:host github :repo "isamert/sozluk.el")
  :commands sozluk)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package xwidget
  :straight (:type built-in)
  :commands xwidget-webkit-browse-url)

(defun ig/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1))
;; (auto-fill-mode 0)
;;(visual-line-mode 1))

(use-package org
  :straight (:type built-in)
  :hook (org-mode . ig/org-mode-setup)
  :custom
  (org-ellipsis " ⤵") ;; ↴, ▼, ▶, ⤵
  (org-hide-emphasis-markers t)
  (org-agenda-files `(,(expand-file-name "agenda.org" org-directory)))
  :custom-face
  (org-document-title ((t (:font "Iosevka Aile" :height 1.5 :weight bold))))
  (org-level-7 ((t (:font "Iosevka Aile" :inherit outline-7 :height 1.1 :weight bold))))
  (org-level-6 ((t (:font "Iosevka Aile" :inherit outline-6 :height 1.15 :weight bold))))
  (org-level-5 ((t (:font "Iosevka Aile" :inherit outline-5 :height 1.2 :weight bold))))
  (org-level-4 ((t (:font "Iosevka Aile" :inherit outline-4 :height 1.25 :weight bold))))
  (org-level-3 ((t (:font "Iosevka Aile" :inherit outline-3 :height 1.3 :weight bold))))
  (org-level-2 ((t (:font "Iosevka Aile" :inherit outline-2 :height 1.4 :weight bold))))
  (org-level-1 ((t (:font "Iosevka Aile" :inherit outline-1 :height 1.5 :weight bold))))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (org-block ((t (:foreground nil :inherit fixed-pitch))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-formula ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-table ((t (:inherit (shadow fixed-pitch)))))
  ;; (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-checkbox ((t (:inherit (fixed-pitch org-todo))))))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun ig/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . ig/org-mode-visual-fill))

(defun ig/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ig/display-startup-time)

;;; init.el -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

;; Set up the visible bell
(setq visible-bell nil)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
;; use-package with Elpaca:

(setq use-package-always-ensure t)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package command-log-mode)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun drmoscovium/dont-arrow ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(define-key evil-normal-state-map (kbd "<left>") 'drmoscovium/dont-arrow)
(define-key evil-normal-state-map (kbd "<right>") 'drmoscovium/dont-arrow)
(define-key evil-normal-state-map (kbd "<down>") 'drmoscovium/dont-arrow)
(define-key evil-normal-state-map (kbd "<up>") 'drmoscovium/dont-arrow)
(evil-global-set-key 'motion (kbd "<left>") 'drmoscovium/dont-arrow)
(evil-global-set-key 'motion (kbd "<right>") 'drmoscovium/dont-arrow)
(evil-global-set-key 'motion (kbd "<down>") 'drmoscovium/dont-arrow)
(evil-global-set-key 'motion (kbd "<up>") 'drmoscovium/dont-arrow)


;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook (
	 (go-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (lsp-enable-which-key-integration t))


(use-package ccls)

(setq lsp-go-analyses '((shadow . t)
                        (simplifycompositelit . :json-false)))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)


;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package tree-sitter)
(use-package tree-sitter-langs)


(global-tree-sitter-mode)

(use-package go-mode)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook 'lsp-deferred)


(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))




(use-package doom-themes)

(load-theme 'doom-gruvbox t)



(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )



(use-package rust-mode)
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))



(use-package elscreen)
(elscreen-start)

(use-package mood-line
  :ensure t
  :if window-system
  :init
  (mood-line-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30)
  (setq-local mode-line-format nil))



(load-file "~/.emacs.d/00_config_agenda.el")
(load-file "~/.emacs.d/01_config_keybinds.el")

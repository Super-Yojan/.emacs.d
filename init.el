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

(setq inhibit-splash-screen t)
(add-hook 'after-init-hook #'(lambda ()
                               (let ((org-agenda-window-setup 'only-window))
                                 (org-agenda-list nil "z"))))


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
  (setq vterm-shell "bash")
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


(setq lsp-go-analyses '((shadow . t)
                        (simplifycompositelit . :json-false)))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)


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

;;(load-theme 'doom-gruvbox t)

(setq calendar-location-name "Fairfax, VA") 
(setq calendar-latitude 38.84)
(setq calendar-longitude -77.306)
(use-package theme-changer)
(require 'theme-changer)
(change-theme 'doom-one-light 'doom-one)

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


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
;;(load-file "~/.emacs.d/02_config_coding_rust.el")
(load-file "~/.emacs.d/03_config_cpp.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++-googlelint-executable "~/.emacs.d/scripts/cpplint.py")
 '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
 '(flycheck-google-cpplint-linelength "120")
 '(flycheck-google-cpplint-verbose "3")
 '(package-selected-packages
   '(cmake-mode flycheck-google-cpplint google-c-style irony flycheck-rust cargo ido-vertical-mode helm popper mood-line elscreen rust-mode which-key vterm use-package tree-sitter-langs shrink-path org-bullets nerd-icons magit lsp-ui lsp-treemacs lsp-ivy go-mode general evil-collection eterm-256color doom-themes dired-single company-box command-log-mode centaur-tabs ccls all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

(use-package helm)

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
	  "\\*vterm\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

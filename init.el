;;; init.el -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
(column-number-mode)
(global-display-line-numbers-mode t)


;; Set up the visible bell
(setq visible-bell nil)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

(load-theme 'wombat)

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


(use-package general)
(require 'general)

;; * Global Keybindings
;; `general-define-key' acts like `evil-define-key' when :states is specified
(general-define-key
 :states 'motion
 ;; swap ; and :
 ";" 'evil-ex
 ":" 'evil-repeat-find-char)
;; same as
(general-define-key
 :states 'motion
 ";" 'evil-ex
 ":" 'evil-repeat-find-char)
;; `general-def' can be used instead for `evil-global-set-key'-like syntax
(general-def 'motion
  ";" 'evil-ex
  ":" 'evil-repeat-find-char)

;; alternative using `general-translate-key'
;; swap ; and : in `evil-motion-state-map'
(general-swap-key nil 'motion
  ";" ":")

;; * Mode Keybindings
(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 ;; or xref equivalent
 "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;; `general-def' can be used instead for `evil-define-key'-like syntax
(general-def 'normal emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  ;; bind "SPC a"
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture
  "SPC" 'file
  "ff" 'find-file
  "l" 'evil-window-right
  "h" 'evil-window-left
  "k" 'evil-window-up
  "j" 'evil-window-bottom
  "[" 'evil-buffer
  "wv" 'evil-window-vnew
  "ws" 'evil-window-new
  "wq" 'evil-quit
  "eb" 'eval-buffer
  )
;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-global-set-key'-like syntax is also supported
(my-leader-def 'normal
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture)

;; to prevent your leader keybindings from ever being overridden (e.g. an evil
;; package may bind "SPC"), use :keymaps 'override
(my-leader-def
  :states 'normal
  :keymaps 'override
  "a" 'org-agenda)
;; or
(my-leader-def 'normal 'override
  "a" 'org-agenda)

;; ** Mode Keybindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  ;; ...
  )
;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-define-key'-like syntax is also supported
(my-local-leader-def 'normal org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  ;; ...
  )

;; * Settings
;; change evil's search module after evil has been loaded (`setq' will not work)
(general-setq evil-search-module 'evil-search)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
  ;; :after magit)

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(centaur-tabs doom-themes dired-single company-box company go-mode tree-sitter-langs tree-sitter lsp-treemacs lsp-ivy helm-lsp lsp-ui nano-emacs rougier/nano-emacs ccls lsp-mode all-the-icons-dired vterm eterm-256color doom-modeline ivy use-package evil-tutor evil-collection command-log-mode))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "fish")
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

(load-file "~/.emacs.d/splash-screen.el")
(splash-screen)
(load-file "~/.emacs.d/bespoke-modeline.el")
  (setq bespoke-modeline-position 'top)
  ;; Set mode-line height
  (setq bespoke-modeline-size 3)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell t)
  ;; Set vc symbol
  (setq  bespoke-modeline-vc-symbol "G:")
(require 'bespoke-modeline)
(bespoke-modeline-mode)


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



;; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))


;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(require 'org-agenda)

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


;;;; General Agenda Settings

(setq org-agenda-files (quote ("~/org")))
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)


(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; -*- lexical-binding: t -*-

  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           (or (bound-and-true-p straight-base-dir)
                               user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
(setq straight-git-executable "/usr/bin/git") ;; Adjust path as needed

  ;; Configure use-package to work with straight.el
  (straight-use-package 'use-package) ; Ensure use-package itself is installed via straight

  (use-package use-package
    :ensure t ; Tells straight.el to manage use-package
    :config
    ;; For packages managed by straight.el, :ensure t will use straight.
    ;; For built-in features not managed by straight, use :straight nil.
    (setq use-package-always-ensure t)
    ;; Make straight.el the default method for use-package :ensure t
    (setq straight-use-package-by-default t))

  ;; Basic Emacs behavior tweaks
  (setq-default
   ring-bell-function 'ignore
   column-number-mode t
   show-paren-mode t
   delete-selection-mode t
   transient-mark-mode t
   sentence-end-double-space nil
   fill-column 80
   mouse-yank-at-point t)
  (setq initial-major-mode 'org-mode)

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

;; Configure backup and auto-save directories inside ~/.emacs.d/.cache/
(let* ((emacs-dotfiles-cache-dir (expand-file-name ".cache" user-emacs-directory))
       (emacs-backups-dir (expand-file-name "backups" emacs-dotfiles-cache-dir))
       (emacs-auto-save-dir (expand-file-name "auto-save" emacs-dotfiles-cache-dir)))
  ;; Set the variables
  (setq backup-directory-alist `(("." . ,emacs-backups-dir)))
  (setq auto-save-file-name-transforms `((".*" ,emacs-auto-save-dir t)))
  (setq auto-save-list-file-prefix (concat emacs-auto-save-dir "/sessions/")) ; For auto-save session data

  ;; Ensure the directories exist
  (make-directory emacs-backups-dir :parents)
  (make-directory emacs-auto-save-dir :parents)
  (make-directory (concat emacs-auto-save-dir "/sessions/") :parents) ; For session files

  ;; Other backup settings
  (setq backup-by-copying t          ; Avoid issues with hard links
        delete-old-versions t        ; Delete old backups automatically
        kept-new-versions 6
        kept-old-versions 2
        version-control t            ; Use version numbers for backups
        auto-save-default t          ; Ensure auto-saving is on
        create-lockfiles nil))       ; Disable lockfiles if you find them annoying (optional)

(use-package exec-path-from-shell
  :ensure t ; Managed by straight.el
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono-14")

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
(use-package vterm
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'truncate-with-project))

(use-package dashboard
  :ensure t
  :commands dashboard-open
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (expand-file-name "assets/profile.gif" user-emacs-directory))
  (setq dashboard-image-banner-max-height 12)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5) (projects . 5) (agenda . 5) (bookmarks . 3)))
  (setq dashboard-agenda-release-buffers t)
  (setq dashboard-org-agenda-custom-command "w"))

(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package neotree
  :ensure t
  :commands neotree-toggle
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-window-width 30))

(use-package popper
  :ensure t
  :commands (popper-toggle popper-cycle popper-toggle-type popper-kill-latest-popup popper-toggle-type-this)
  :init
  (setq popper-display-function #'popper-display-function-bottom)
  (setq popper-window-height 20)
  (setq popper-reference-buffers
        '(help-mode compilation-mode occur-mode Info-mode Man-mode woman-mode helpful-mode
          vterm-mode magit-process-mode
          ("\\*Messages\\*" . bottom) ("\\*Warnings\\*" . bottom) ("\\*Compile-Log\\*" . bottom)
          ("\\*compilation\\*" . bottom) ("\\*Help\\*" . bottom) ("\\*Apropos\\*" . bottom)
          ("\\*info\\*" . bottom) "Output\\*$" "\\*Async Shell Command\\*"))
  :config
  (popper-mode +1) (popper-echo-mode +1)
  (setq popper-allow-hiding-last-buffer nil))


(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(dolist (mode-hook '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook vterm-mode-hook
                     dired-mode-hook neotree-mode-hook help-mode-hook Man-mode-hook woman-mode-hook
                     Info-mode-hook compilation-mode-hook))
  (add-hook mode-hook (lambda () (display-line-numbers-mode -1))))

(add-hook 'image-mode-hook #'auto-revert-mode)

(use-package eterm-256color
  :ensure t :hook (term-mode . eterm-256color-mode))

(use-package olivetti :ensure t :commands olivetti-mode)

;; --- Translucency ---
;; Set frame opacity. Values are from 0 (fully transparent) to 100 (fully opaque).
;; You can use a single value for consistent opacity, or a cons cell
;; (ACTIVE-OPACITY . INACTIVE-OPACITY) for different states.
;; This requires your window manager/compositor to support _NET_WM_WINDOW_OPACITY.
;; Example: 90% opaque when active, 75% when inactive.
(add-to-list 'default-frame-alist '(alpha . (97 . 75)))

;; If you want all frames, including the initial one, to have this opacity from the very start,
;; you can also add it to `initial-frame-alist`.
;; (add-to-list 'initial-frame-alist '(alpha . (90 . 75)))
;; Note: Sometimes setting it only in default-frame-alist is sufficient for the initial frame too.

;; To experiment on an existing frame, you can evaluate:
;; (set-frame-parameter nil 'alpha 85)
;; (set-frame-parameter nil 'alpha '(90 . 70))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil evil-want-integration t
        evil-undo-system 'undo-tree evil-respect-visual-line-mode t)
  :config (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-g") 'keyboard-quit)
  (define-key evil-visual-state-map (kbd "C-g") 'keyboard-quit)
  (define-key evil-motion-state-map (kbd "C-g") 'keyboard-quit)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package undo-tree
  :ensure t :init (global-undo-tree-mode))

(use-package general
  :ensure t
  :demand t                             ;
  ;:after (evil popper projectile neotree vterm helm olivetti dashboard ace-window restart-emacs)
  :config
  (general-define-key
    :prefix "SPC"
    :states '(normal visual motion emacs)
    :keymaps 'override
    ""    '(nil :wk "Leader")
    ;; Files & Buffers
    "f f" '(find-file :wk "Find File")
    "f d" '(my-directory-traverse :wk "Traverse Directory")
    "f s" '(save-buffer :wk "Save File")
    "f r" '(helm-recentf :wk "Recent Files (Helm)") ; Changed to helm-recentf
    "b b" '(helm-buffers-list :wk "List Buffers (Helm)")
    "b k" '(kill-this-buffer :wk "Kill Current Buffer")
    "b K" '(kill-buffer-and-window :wk "Kill Buffer & Window")
    "b B" '(ibuffer :wk "Ibuffer")
    "'"   '(helm-bookmarks :wk "Bookmarks (Helm)")

    ;; Projects (Projectile)
    "p p" '(projectile-switch-project :wk "Switch Project")
    "p f" '(helm-projectile-find-file :wk "Find File in Project (Helm)") ; Changed
    "p g" '(helm-projectile-grep :wk "Grep in Project (Helm)") ; Changed
    "p b" '(helm-projectile-buffers-list :wk "Project Buffers (Helm)")
    "p L" '(persp-switch-to-buffer :wk "Switch to buffer in perspective")

    ;; Workspaces / Layouts (Perspective)
    "L s" '(persp-switch :wk "Switch perspective") "L n" '(persp-next :wk "Next perspective")
    "L p" '(persp-prev :wk "Previous perspective") "L c" '(persp-add-new :wk "Create new perspective")
    "L k" '(persp-kill :wk "Kill current perspective") "L r" '(persp-rename :wk "Rename current perspective")
    "L S" '(persp-save-state-to-file :wk "Save perspectives") "L L" '(persp-load-state-from-file :wk "Load perspectives")

    ;; Windows
    "w h" '(evil-window-left :wk "Window Left") "w j" '(evil-window-down :wk "Window Down")
    "w k" '(evil-window-up :wk "Window Up") "w l" '(evil-window-right :wk "Window Right")
    "w s" '(evil-window-split :wk "Split Below") "w v" '(evil-window-vsplit :wk "Split Right")
    "w d" '(delete-window :wk "Delete Window") "w m" '(delete-other-windows :wk "Maximize Window")
    "w w" '(ace-window :wk "Ace Select Window") "w TAB" '(other-window :wk "Next Window")

    ;; Org Mode, Vterm, URL, Olivetti
    "o a" '(org-agenda :wk "Org Agenda") "o c" '(org-capture :wk "Org Capture")
    "o l" '(org-store-link :wk "Org Store Link") "o t" '(my-open-vterm-in-popper :wk "Toggle Vterm (Popper)")
    "o u p" '(my-open-paper-from-url :wk "Open Paper from URL") "o m" '(olivetti-mode :wk "Toggle Olivetti Mode")
    "o p" '(neotree-toggle :wk "Toggle neotree")

    ;; Popper Toggle
    "P t" '(popper-toggle :wk "Toggle Popper Window") "P n" '(popper-cycle :wk "Cycle Popper Windows")
    "P k" '(popper-kill-latest-popup :wk "Kill Latest Popper Popup")

    ;; Magit & Git/Grep
    "g g" '(magit-status :wk "Magit Status") "g s" '(magit-status :wk "Magit Status")
    "g b" '(magit-blame-addition :wk "Magit Blame")
    "g h g" '(helm-grep-do-grep :wk "Grep with Helm")

    ;; Search
    "/ /" '(helm-swoop :wk "Swoop in Buffer (Helm)")

    ;; Help & Emacs Actions
    "h k" '(describe-key :wk "Describe Key") "h v" '(describe-variable :wk "Describe Variable")
    "h f" '(describe-function :wk "Describe Function") "h m" '(describe-mode :wk "Describe Mode")
    "h d" '(dashboard-open :wk "Open Dashboard")
    "x"   '(helm-M-x :wk "Helm M-x")
    "SPC" '(helm-projectile-find-file :wk "Find project file (Helm)") ; Was SPC SPC

    ;; Other useful bindings
    "t n" '(toggle-truncate-lines :wk "Toggle Truncate Lines")
    "q q" '(save-buffers-kill-terminal :wk "Save & Quit Emacs")
    "q r" '(restart-emacs :wk "Restart Emacs")
    "/"   '(comment-line :wk "Comment Line")
    "]"   '(next-buffer :wk "Next Buffer")
    "["   '(previous-buffer :wk "Previous Buffer")

    "t" '(:ignore t :wk "Toggles") ; Create a "Toggles" submenu
    "t l" '(display-line-numbers-mode :wk "Toggle Line Numbers")
    "t t" '(visual-line-mode :wk "Toggle Visual Line Mode"))

  (defun drmoscovium/dont-arrow () (interactive) (message "Arrow keys are discouraged! Use h, j, k, l."))
  (general-define-key
   :keymaps '(evil-normal-state-map evil-motion-state-map evil-visual-state-map)
   "<left>" 'drmoscovium/dont-arrow "<right>" 'drmoscovium/dont-arrow
   "<down>" 'drmoscovium/dont-arrow "<up>" 'drmoscovium/dont-arrow))

(use-package evil-collection
  :ensure t :after evil :config (evil-collection-init))
(use-package evil-org
  :ensure t :after (org evil) :hook (org-mode . evil-org-mode)
  :config (require 'evil-org-agenda) (evil-org-agenda-set-keys))

(use-package ace-window :ensure t :commands ace-window)
(use-package restart-emacs :ensure t :commands restart-emacs)

;; --- Completion Framework (Helm & Company) ---
(use-package helm
  :ensure t
  :init (helm-mode 1)
  :config
  (setq helm-split-window-in-side-p t helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  #'helm-select-action))

(use-package helm-projectile
  :ensure t :after (helm projectile) :config (helm-projectile-on))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2 company-minimum-prefix-length 2)
  (setq company-backends
        (append '((company-capf company-yasnippet company-keywords))
                company-backends)) ; Add to existing backends
  (setq company-tooltip-align-annotations t)
  :bind (:map company-active-map
         ("C-n" . company-select-next) ("C-p" . company-select-previous)
         ("<tab>" . company-complete-common-or-cycle) ("TAB" . company-complete-common-or-cycle)
         ("C-s" . company-filter-candidates)))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets :ensure t :after yasnippet)

;; --- LSP Client (Eglot) ---
(use-package eglot
  :ensure t ; Ensure eglot is installed if not on Emacs 29+
  :commands (eglot eglot-ensure)
  :hook ((prog-mode ) . eglot-ensure) ; Try to start eglot in prog/text modes
  :config
  (setq eglot-autoshutdown t eglot-send-changes-idle-time 0.5)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright")))
  (add-to-list 'eglot-server-programs '((rustic-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))
  ;; (add-to-list 'eglot-server-programs '(java-mode . ("jdtls"))) ; Requires manual setup for jdtls
  )

(use-package eldoc-box
  :ensure t :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))


;; Flycheck for on-the-fly syntax checking (integrates with Eglot)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package tree-sitter
  :ensure t
  :config
  (use-package tree-sitter-langs :ensure t :after tree-sitter)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)) ; Enable highlighting

(defun my-prog-mode-common-hook ()
  "Common setup for programming modes."
  ;; display-line-numbers-mode is global now
  (electric-pair-mode 1))

(use-package cc-mode ; Configurations for built-in cc-mode and its ts-mode variants
  :straight nil
  :hook (((c-mode c++-mode c-ts-mode c++-ts-mode) . my-prog-mode-common-hook) ; General hook
         ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)      ; LSP hook
         ((c-ts-mode c++-ts-mode) . (lambda () (setq-local indent-tabs-mode nil tab-width 4))))
  :config (setq c-basic-offset 4 c-default-style "linux" indent-tabs-mode nil))

(use-package rustic ; Comprehensive Rust mode, handles LSP via rustic-lsp-client
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot) ; Tell rustic to use eglot
  ;; rustic-mode runs its own hook, which should include my-prog-mode-common-hook if needed
  ;; and eglot-ensure will be called via rustic's LSP mechanism
  (add-hook 'rustic-mode-hook #'my-prog-mode-common-hook)
  (add-hook 'rustic-mode-hook (lambda () (setq indent-tabs-mode nil tab-width 4))))

(use-package pyvenv :ensure t :hook ((python-mode python-ts-mode) . pyvenv-mode))
(use-package auto-virtualenv :ensure t :hook ((python-mode python-ts-mode) . auto-virtualenv-set-virtualenv))

(use-package go-ts-mode
  :straight nil :mode "\\.go\\'"
  :hook ((go-ts-mode . my-prog-mode-common-hook) (go-ts-mode . eglot-ensure))
  :config (add-hook 'go-ts-mode-hook (lambda () (setq indent-tabs-mode t tab-width 8)) nil t))
(use-package go-mode ; Fallback
  :ensure t :mode "\\.go\\'"
  :hook ((go-mode . my-prog-mode-common-hook) (go-mode . eglot-ensure))
  :config (add-hook 'go-mode-hook (lambda () (setq indent-tabs-mode t tab-width 8)) nil t))

(use-package typescript-ts-mode
  :straight nil :mode ("\\.ts\\'" . typescript-ts-mode) ("\\.tsx\\'" . tsx-ts-mode)
  :hook (((typescript-ts-mode tsx-ts-mode) . my-prog-mode-common-hook)
         ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))
  :config
  (add-hook 'typescript-ts-mode-hook (lambda () (setq indent-tabs-mode nil tab-width 2)) nil t)
  (add-hook 'tsx-ts-mode-hook (lambda () (setq indent-tabs-mode nil tab-width 2)) nil t))

(use-package tide
  :ensure t
  :after (typescript-ts-mode company flycheck) ; Ensure these are available
  :hook ((typescript-ts-mode . tide-setup) (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode) (tsx-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package vhdl-mode
  :ensure t :mode "\\.vhd\\'"
  :hook ((vhdl-mode . my-prog-mode-common-hook) (vhdl-mode . eglot-ensure)))

(use-package verilog-mode
  :ensure t :mode "\\.v\\'"
  :hook ((verilog-mode . my-prog-mode-common-hook) (verilog-mode . eglot-ensure)))

(use-package org
  :ensure t ; Use a recent version
  :commands (org-agenda org-capture org-store-link)
  ;; evil-org-mode hook is applied in the evil-org use-package block
  :config
  (setq org-ellipsis " ▼" org-log-done 'time org-hide-emphasis-markers t
        org-src-fontify-natively t org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil org-startup-indented t org-startup-folded 'content)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "LOOP(r)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d)" "CANCELLED(c@/!)")
          (sequence "[ ](SPC)" "[-](-)" "[?](?)" "|" "[X](X)"))) ; Checkbox style
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold) ("NEXT" :foreground "blue" :weight bold)
          ("PROJ" :foreground "purple" :weight bold) ("LOOP" :foreground "saddle brown" :weight bold)
          ("WAIT" :foreground "orange" :weight bold) ("HOLD" :foreground "magenta" :weight bold)
          ("DONE" :foreground "forest green" :weight bold) ("CANCELLED" :foreground "gray" :weight bold)
          ("[-]" :foreground "goldenrod" :weight bold) ("[?]" :foreground "magenta" :weight bold)))

  (setq org-directory (expand-file-name "~/org"))
  (unless (file-directory-p org-directory) (make-directory org-directory t))
  (setq org-agenda-files (list org-directory)) ; Scan all .org files in this directory
  (setq org-default-notes-file (concat org-directory "/inbox.org"))

  (setq org-refile-targets `((,org-directory :maxlevel . 4) (nil :maxlevel . 4)))
  (setq org-refile-use-outline-path t org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        `(("t" "Todo (Inbox)" entry (file+headline ,org-default-notes-file "Inbox Tasks")
           "* TODO %?\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  %i\n  %a"
           :prepend t :empty-lines 1 :kill-buffer t)
          ("n" "Note (Inbox)" entry (file+headline ,org-default-notes-file "Quick Notes")
           "* %U %?\n  %i\n  %a" :prepend t :empty-lines 1 :kill-buffer t)
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %<%H:%M> %?\n%i%a" :prepend t :empty-lines 0 :kill-buffer t)
          ("p" "Project" entry (file+headline (concat org-directory "/projects.org") "Active Projects")
           "* PROJ %? \n%i\n%a" :prepend t :empty-lines 1 :kill-buffer t)
          ("m" "Meeting Note" entry (file+headline ,org-default-notes-file "Meetings")
           "* MEETING with %? :MEETING:\n%U\n%a" :clock-in t :clock-resume t :kill-buffer t)))

  (org-babel-do-load-languages 'org-babel-load-languages
   '((python . t) (shell . t) (emacs-lisp . t)))
  (require 'org-tempo)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-modern
  :ensure t 
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-auto-align-tags nil org-tags-column 0 org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t org-insert-heading-respect-content t org-pretty-entities t org-ellipsis "…")
  (setq org-agenda-tags-column 0 org-agenda-block-separator ?─
        org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────"))

(use-package org-super-agenda
  :ensure t :after org :demand t
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(("w" "Workflow Overview"
           ((agenda "" ((org-agenda-span 'week) (org-agenda-start-on-current-day t)
                        (org-agenda-format-date "%Y-%m-%d %a")
                        (org-super-agenda-groups
                         '((:name "⏰ Today" :time-grid t :date today :order 1)
                           (:name "❗ Important (Prio A)" :priority "A" :order 2)
                           (:name "🔥 Due & Overdue" :deadline future :deadline past :order 3)
                           (:name "🗓️ Scheduled Soon" :scheduled future :order 4)
                           (:name "🔁 Recurring/Loops" :todo "LOOP" :tag ("habit" "routine") :order 10)
                           (:name "⏳ Waiting For" :todo "WAIT" :order 12)
                           (:name "🚀 Next Actions" :todo "NEXT" :order 15)))))
            (alltodo "" ((org-agenda-overriding-header "\n✅ All Tasks by Status")
                         (org-super-agenda-groups
                          '((:name "🏗️ Projects" :todo "PROJ" :order 1)
                            (:name "🚀 Next Actions" :todo "NEXT" :order 2)
                            (:name "📋 Active TODOs" :todo "TODO" :order 3)
                            (:name "⏳ Waiting For" :todo "WAIT" :order 4)
                            (:name "🔁 Loops" :todo "LOOP" :order 5)
                            (:name "📝 Notes" :todo "NOTE" :order 20)))))))
          ("P" "All Projects List"
           ((alltodo "" ((org-agenda-overriding-header "All Projects")
                         (org-super-agenda-groups '(("Active Projects" :todo "PROJ"))))))))))


(use-package dslide :ensure t :commands dslide-mode)
(use-package ox-hugo :ensure t :after ox)
(use-package latex-preview-pane :ensure t :commands latex-preview-pane-enable)
;;(use-package org-auctex :ensure (:host github :repo "karthink/org-auctex") :after (org tex) :hook (org-mode . org-auctex-mode))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  ;:bind
  ; Don't bind anything here, using general to bind everything
  :config
  (setq denote-directory (expand-file-name "~/org/"))
  (denote-rename-buffer-mode 1)
  )

(use-package ox-typst
  :after org
  :ensure t)


  (require 'ob)

(defvar org-babel-default-header-args:typst
  '((:results . "typst") (:exports . "results"))
  "Default arguments to use when evaluating a typst source block.")

(defun org-babel-expand-body:typst (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:typst (body params)
  "Execute a block of Dot code with org-babel.
This function is called by `org-babel-execute-src-block'."
  body)

(defun org-babel-prep-session:typst (_session _params)
  "Return an error because Dot does not support sessions."
  (error "Dot does not support sessions"))

(use-package pdf-tools
  :ensure t :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "<mouse-4>") #'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "<mouse-5>") #'pdf-view-scroll-up-or-next-page))

(defun my-directory-traverse ()
  "Traverse directories using Helm or Dired, project-aware."
  (interactive)
  (let ((initial-dir (or (ignore-errors (projectile-project-root)) default-directory)))
    (if (fboundp 'helm-find-files) (helm-find-files initial-dir) (dired initial-dir))))

(defun my-open-paper-from-url (url)
  "Download and open a research paper PDF from URL."
  (interactive (list (read-string "Paper URL: ")))
  (let* ((processed-url url) (is-arxiv-abs (string-match-p "arxiv\\.org/abs/\\(.+\\)" url))
         (is-pdf (string-match-p "\\.pdf\\'" (downcase url)))
         (download-dir (expand-file-name "~/Downloads/papers/")) file-name local-file-path)
    (unless (file-directory-p download-dir) (make-directory download-dir t))
    (when is-arxiv-abs (setq processed-url (replace-match "https://arxiv.org/pdf/\\1.pdf" t t url)))
    (setq is-pdf (string-match-p "\\.pdf\\'" (downcase processed-url)))
    (if is-pdf
        (progn (setq file-name (file-name-nondirectory (car (url-parse-by-used-backend processed-url 'path))))
               (unless (string-match-p "\\.pdf\\'" (downcase file-name)) (setq file-name (concat file-name ".pdf")))
               (setq local-file-path (concat download-dir (replace-regexp-in-string "/" "_" file-name)))
               (message "Attempting to download %s to %s..." processed-url local-file-path)
               (condition-case err (url-copy-file processed-url local-file-path t)
                 (error (message "Failed to download PDF: %s. Opening URL." err) (browse-url url) (setq local-file-path nil)))
               (when local-file-path (message "Downloaded. Opening %s..." local-file-path) (find-file local-file-path)))
      (message "URL not a direct PDF/arXiv abstract. Opening in browser/eww...")
      (if (fboundp 'eww) (eww url) (browse-url url)))))

(defun my-open-vterm-in-popper ()
  "Ensure a vterm buffer exists and display it via popper."
  (interactive)
  (let ((vterm-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (eq major-mode 'vterm-mode))) (buffer-list))))
    (if vterm-buffer (switch-to-buffer vterm-buffer) (vterm)))
  (when (eq major-mode 'vterm-mode) (popper-toggle-type-this)))

;; Final GC threshold is set by the hook in early-init.el
(setq read-process-output-max (* 1024 1024)) ; 1MB for process output

(dashboard-open)

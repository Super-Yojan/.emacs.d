;; -*- lexical-binding: t -*-

;; ============================================================
;; Ensure seq-empty-p fix (belt and suspenders for Emacs 30)
;; ============================================================
(require 'cl-lib)
(require 'seq)
(unless (ignore-errors (seq-empty-p 'test-symbol))
  (cl-defmethod seq-empty-p ((_ symbol)) nil))

;; ============================================================
;; Bootstrap straight.el
;; ============================================================
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package with straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-ensure t)

;; Prevent straight from managing built-in packages INCLUDING org and seq
(setq straight-built-in-pseudo-packages 
      '(emacs nadvice python eldoc project org seq cl-lib))

;; Use built-in org-mode (stable and always works)
(require 'org)
(require 'org-tempo)

;; Basic settings
(setq-default
 ring-bell-function 'ignore
 column-number-mode t
 indent-tabs-mode nil
 tab-width 4
 fill-column 80)

(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;; Recent files tracking
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; Backup settings with error handling
(let ((backup-dir (expand-file-name ".cache/backups" user-emacs-directory))
      (auto-save-dir (expand-file-name ".cache/auto-save" user-emacs-directory)))
  (condition-case nil
      (progn
        (make-directory backup-dir t)
        (make-directory auto-save-dir t)
        (setq backup-directory-alist `(("." . ,backup-dir))
              auto-save-file-name-transforms `((".*" ,auto-save-dir t))
              backup-by-copying t
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2
              version-control t
              create-lockfiles nil))
    (error nil)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(defun my-reload-init-file ()
  "Reloads the user's Emacs initialization file."
  (interactive)
  (load-file user-init-file)
  (message "Emacs init file reloaded!"))

;; Set font with error handling
(when (display-graphic-p)
  (condition-case nil
      (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono-18")
    (error (message "FiraCode font not found"))))


(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode)
  :config
  ;; Change "Latin Modern Roman" to "ET Book" or "Times New Roman" if you prefer
  (set-face-attribute 'variable-pitch nil :family "Georgia"  :height 130)
  ;; Keep your code blocks monospaced!
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font Mono" :height 130))

(use-package visual-fill-column
  :defer t)


(defun my/sync-theme-with-system (appearance)
  "Switch timu-macos flavour based on system APPEARANCE (light or dark)."
  ;; 1. Disable active themes to prevent conflict/overlap
  (mapc #'disable-theme custom-enabled-themes)
  
  ;; 2. Set the flavour variable based on the appearance argument
  ;; Note: appearance is provided as the symbol 'light or 'dark
  (customize-set-variable 'timu-macos-flavour 
                          (if (eq appearance 'dark) "dark" "light"))
  
  ;; 3. Reload the theme to apply the new flavour
  (load-theme 'timu-macos t))

;; 4. Add the hook (only works on macOS Emacs ports capable of detecting system theme)
(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions #'my/sync-theme-with-system))

;; Load theme with error handling
(use-package timu-macos-theme
  :ensure t
  :config
  (load-theme 'timu-macos t))

(customize-set-variable 'timu-macos-scale-org-document-title 1.2)
(customize-set-variable 'timu-macos-scale-org-document-info 1.2)
(customize-set-variable 'timu-macos-scale-org-level-1 1.2)
(customize-set-variable 'timu-macos-scale-org-level-2 1.2)
(customize-set-variable 'timu-macos-scale-org-level-3 1.1)

;; Undo-tree configuration with history directory
(use-package undo-tree
  :demand t
  :config
  (let ((undo-dir (concat user-emacs-directory "undo-tree-hist/")))
    (unless (file-exists-p undo-dir)
      (make-directory undo-dir t))
    (setq undo-tree-history-directory-alist `(("." . ,undo-dir))))
  (global-undo-tree-mode))

;; Icons for dashboard (optional, only if display supports it)
(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;; Dashboard for quick access
(use-package dashboard
  :demand t
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 10)
                          (bookmarks . 5))
        dashboard-set-init-info t
        dashboard-set-footer nil)
  (dashboard-setup-startup-hook)

  (defun dashboard-goto-projects ()
    "Jump to projects section in dashboard."
    (interactive)
    (dashboard-refresh-buffer)
    (goto-char (point-min))
    (search-forward "Projects:" nil t))
  
  (defun dashboard-goto-recent-files ()
    "Jump to recent files section in dashboard."
    (interactive)
    (dashboard-refresh-buffer)
    (goto-char (point-min))
    (search-forward "Recent Files:" nil t)))

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook vterm-mode-hook term-mode-hook shell-mode-hook
                              pdf-view-mode-hook eshell-mode-hook
                              compilation-mode-hook dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Fullscreen support
(defun toggle-fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; Zen Mode - Distraction-free writing
(use-package writeroom-mode
  :commands writeroom-mode
  :config
  (setq writeroom-width 100
        writeroom-mode-line t
        writeroom-bottom-divider-width 0
        writeroom-fullscreen-effect 'maximized
        writeroom-maximize-window t
        writeroom-extra-line-spacing 0.2))

;;; 2. SET THE MARGINS (The "Page" Feel)
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 100)  ;; Width of the "paper"
  (setq olivetti-style 'fancy))   ;; Smooth margins


;; Function to toggle zen mode with multiple options
(defun my-zen-mode ()
  "Toggle zen mode for distraction-free writing."
  (interactive)
  (if (bound-and-true-p writeroom-mode)
      (writeroom-mode -1)
    (writeroom-mode 1)))

;; Alternative lightweight zen mode
(defun my-zen-mode-light ()
  "Toggle lightweight zen mode with olivetti."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (display-line-numbers-mode 1))
    (progn
      (olivetti-mode 1)
      (display-line-numbers-mode -1))))

;; Ultra zen mode - everything hidden
(defun my-zen-mode-ultra ()
  "Toggle ultra zen mode - hide everything."
  (interactive)
  (if (bound-and-true-p writeroom-mode)
      (progn
        (writeroom-mode -1)
        (display-line-numbers-mode 1)
        (when (fboundp 'doom-modeline-mode) (doom-modeline-mode 1)))
    (progn
      (writeroom-mode 1)
      (display-line-numbers-mode -1)
      (when (fboundp 'doom-modeline-mode) (doom-modeline-mode -1)))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3))

;; Which-key first (needed by general)
(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; Ivy - completion framework
(use-package ivy
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil))

;; Counsel
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

;; Swiper
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

;; Evil mode configuration
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-tree
        evil-respect-visual-line-mode t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Projectile (needed before counsel-projectile)
(use-package projectile
  :demand t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-project-search-path '("~/git/" "~/work/" "~/Research/")
        projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld" user-emacs-directory)
        projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory))
  
  (let ((cache-dir (expand-file-name ".cache" user-emacs-directory)))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))))

;; Counsel-projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

;; Ivy-rich
(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; General keybindings
(use-package general
  :demand t
  :config
  (general-define-key
   :prefix "SPC"
   :states '(normal visual motion)
   :keymaps 'override

   "f n" '(make-frame-command :wk "make frame")

   ;; Files & Buffers
   "f f" '(counsel-find-file :wk "Find file")
   "f s" '(save-buffer :wk "Save file")
   "f r" '(counsel-recentf :wk "Recent files")
   "b b" '(ivy-switch-buffer :wk "Switch buffer")
   "b k" '(kill-this-buffer :wk "Kill buffer")

   ;; Projects
   "p" '(:ignore t :wk "Projects")
   "p p" '(counsel-projectile-switch-project :wk "Switch project")
   "p f" '(counsel-projectile-find-file :wk "Find file in project")
   "p d" '(counsel-projectile-find-dir :wk "Find dir in project")
   "p b" '(counsel-projectile-switch-to-buffer :wk "Switch project buffer")
   "p s" '(counsel-projectile-rg :wk "Search in project")
   "p r" '(projectile-recentf :wk "Recent project files")
   "p i" '(projectile-invalidate-cache :wk "Invalidate cache")
   "p k" '(projectile-kill-buffers :wk "Kill project buffers")
   "p S" '(projectile-save-project-buffers :wk "Save project buffers")

   ;; Search
   "s" '(:ignore t :wk "Search")
   "s s" '(swiper :wk "Search buffer")
   "s p" '(counsel-projectile-rg :wk "Search project")
   "s d" '(counsel-ag :wk "Search directory")
   "s f" '(counsel-find-file :wk "Find files")
   "s g" '(counsel-git-grep :wk "Git grep")

   ;; Dashboard
   "d" '(:ignore t :wk "Dashboard")
   "d d" '(dashboard-refresh-buffer :wk "Open dashboard")
   "d p" '(dashboard-goto-projects :wk "Go to projects")
   "d r" '(dashboard-goto-recent-files :wk "Go to recent files")

   ;; Windows
   "w h" '(evil-window-left :wk "Left")
   "w j" '(evil-window-down :wk "Down")
   "w k" '(evil-window-up :wk "Up")
   "w l" '(evil-window-right :wk "Right")
   "w s" '(evil-window-split :wk "Split below")
   "w v" '(evil-window-vsplit :wk "Split right")
   "w d" '(delete-window :wk "Delete window")
   "w m" '(delete-other-windows :wk "Maximize")

   ;; Org Mode
   "o a" '(org-agenda :wk "Agenda")
   "o c" '(org-capture :wk "Capture")
   "o t" '(org-babel-tangle :wk "Tangle")

   ;; Eglot / Code
   "c" '(:ignore t :wk "Code/Eglot")
   "c r" '(eglot-rename :wk "Rename")
   "c a" '(eglot-code-actions :wk "Code action")
   "c f" '(eglot-format-buffer :wk "Format buffer")
   "c g" '(:ignore t :wk "Go to")
   "c g d" '(xref-find-definitions :wk "Definition")
   "c g r" '(xref-find-references :wk "References")
   "c g i" '(eglot-find-implementation :wk "Implementation")
   "c g t" '(eglot-find-typeDefinition :wk "Type definition")
   "c s" '(consult-eglot-symbols :wk "Workspace symbol")
   "c d" '(eldoc-box-help-at-point :wk "Describe at point")
   "c e" '(flymake-show-buffer-diagnostics :wk "Show errors")
   "c E" '(flymake-show-project-diagnostics :wk "Project errors")
   "c R" '(eglot-reconnect :wk "Restart server")
   "c q" '(eglot-shutdown :wk "Shutdown server")

   ;; Reload
   "h r r" '(my-reload-init-file :wk "Reload")

   ;; Terminal
   "t t" '(vterm :wk "Terminal")

   ;; Git
   "g g" '(magit-status :wk "Magit")

   ;; File tree
   "e" '(neotree-toggle :wk "neotree")

   ;; Typst
   "m c" '(my-typst-compile-and-view :wk "Compile & view Typst")
   "m w" '(my-typst-watch-toggle :wk "Toggle watch mode (live preview)")
   "m v" '(my-typst-view-pdf :wk "View PDF")

   ;; LaTeX Preview (any buffer)
   "l" '(:ignore t :wk "LaTeX")
   "l p" '(my-latex-preview-toggle :wk "Preview LaTeX")
   "l r" '(texfrag-document :wk "Render all (texfrag)")
   "l c" '(texfrag-clear-document :wk "Clear previews")

   ;; Help
   "h k" '(describe-key :wk "Describe key")
   "h v" '(describe-variable :wk "Describe variable")
   "h f" '(describe-function :wk "Describe function")

   ;; Buffer navigation
   "[" '(previous-buffer :wk "Previous buffer")
   "]" '(next-buffer :wk "Next buffer")

   ;; View & Zen Mode
   "v" '(:ignore t :wk "View/Zen")
   "v f" '(toggle-fullscreen :wk "Fullscreen (F11)")
   "v z" '(my-zen-mode :wk "Zen mode (writeroom)")
   "v l" '(my-zen-mode-light :wk "Zen mode light (olivetti)")
   "v u" '(my-zen-mode-ultra :wk "Ultra zen (hide all)")
   "v n" '(display-line-numbers-mode :wk "Toggle line numbers")

   ;; Other
   "SPC" '(counsel-projectile :wk "Find in project")
   "q q" '(save-buffers-kill-terminal :wk "Quit")
   "/" '(comment-line :wk "Comment")

   ;; Denote
   "n n" '(denote-open-or-create :wk "Denote")
   "n l" '(denote-link :wk "Denote link")
   "n L" '(denote-add-links :wk "Denote add links")))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm: %s"))

;; Make vterm open as a popup at the bottom
(add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
               (display-buffer-at-bottom)
               (window-height . 0.3)
               (dedicated . t)))

(use-package csv-mode
  :mode "\\.csv\\'")

;; Persist history
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle)))

;; Company-box
(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-doc-enable t))

;; Eldoc-box for nicer documentation display
(use-package eldoc-box
  :commands eldoc-box-help-at-point
  :config
  (setq eldoc-box-max-pixel-width 800
        eldoc-box-max-pixel-height 600
        eldoc-box-clear-with-C-g t))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (let ((snippet-dir "~/.emacs.d/snippets"))
    (when (file-directory-p snippet-dir)
      (setq yas-snippet-dirs (list snippet-dir))))
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================
;; Eglot - Built-in LSP client (simpler and lighter than lsp-mode)
;; ============================================================
(use-package eglot
  :straight (:type built-in)
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (typst-ts-mode . eglot-ensure))
  :config
  ;; --- START OF FIX ---
  ;; Patch: Prevent crash when server returns nil markup (The "char-or-string-p" error)
  (advice-add 'eglot--format-markup :filter-args
    (lambda (args)
      ;; If the argument (content) is nil, replace it with an empty string
      (if (car args) args '(""))))
  ;; --- END OF FIX ---

  ;; Performance settings
  (setq eglot-autoshutdown t  ; Shutdown server when last buffer closes
        eglot-events-buffer-size 0  ; Disable events logging for performance
        eglot-sync-connect nil  ; Don't block on connection
        eglot-connect-timeout 60)  ; Longer timeout for slow servers

  ;; Configure servers
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("typst-lsp")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '(rustic-mode . ("rust-analyzer")))

  ;; Eldoc settings for Eglot
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Improve performance by limiting capabilities
  (fset #'jsonrpc--log-event #'ignore))


;; Flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Magit
(use-package magit
  :commands magit-status)

;; Treemacs (optional file explorer)
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 30))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; Prettier Formatting (Async - won't freeze Emacs)
(use-package apheleia
  :hook (prog-mode . apheleia-mode))

;; ============================================================
;; Native Tree-Sitter (The "Emacs 30 Way")
;; ============================================================

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; Markdown support (often missing by default)
(use-package markdown-mode
  :mode ("README\\.md\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown"))

;; ============================================================
;; Typst
;; ============================================================

(defvar my-typst-watch-process nil
  "Process for typst watch mode.")

(defun my-typst-compile-and-view ()
  "Compile current Typst file and open the PDF."
  (interactive)
  (when buffer-file-name
    (let* ((input-file buffer-file-name)
           (output-file (concat (file-name-sans-extension input-file) ".pdf")))
      (save-buffer)
      (call-process "typst" nil nil nil "compile" input-file)
      (when (file-exists-p output-file)
        (if (eq system-type 'darwin)
            (call-process "open" nil nil nil output-file)
          (call-process "xdg-open" nil nil nil output-file))))))

(defun my-typst-watch-toggle ()
  "Toggle typst watch mode for live preview."
  (interactive)
  (if (and my-typst-watch-process (process-live-p my-typst-watch-process))
      (progn
        (kill-process my-typst-watch-process)
        (setq my-typst-watch-process nil)
        (message "Typst watch mode stopped"))
    (when buffer-file-name
      (save-buffer)
      (setq my-typst-watch-process
            (start-process "typst-watch" "*typst-watch*"
                           "typst" "watch" "--open" buffer-file-name))
      (message "Typst watch mode started"))))

(defun my-typst-view-pdf ()
  "View the PDF for the current Typst file."
  (interactive)
  (when buffer-file-name
    (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
      (if (file-exists-p pdf-file)
          (if (eq system-type 'darwin)
              (call-process "open" nil nil nil pdf-file)
            (call-process "xdg-open" nil nil nil pdf-file))
        (message "PDF file not found. Compile first with SPC m c")))))

(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :mode "\\.typ\\'"
  :config
  (setq typst-ts-mode-watch-options "--open"
        typst-ts-mode-compile-options ""))

;; ============================================================
;; Python
;; ============================================================
(use-package pyvenv
  :hook (python-ts-mode . pyvenv-mode)) ;; Hook to the TS mode specifically

;; ============================================================
;; Rust
;; ============================================================

;; Set the LSP client BEFORE loading rustic
(setq rustic-lsp-client 'eglot)

(use-package rustic
  :straight t
  :demand t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . eglot-ensure)
  :config
  (setq rustic-format-on-save nil
        rustic-analyzer-command '("rust-analyzer"))
  ;; Force rustic to use the native tree-sitter mode if available
  (setq rustic-mode-use-ts-mode t))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Ensure nerd-icons for themes
(use-package nerd-icons
  :if (display-graphic-p))

;; Htmlize for org export
(use-package htmlize
  :defer t)

;; Org-fragtog for LaTeX preview
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))


;; Ox-typst for Typst export
(use-package ox-typst
  :after org
  :config
  (setq org-typst-from-latex-environment #'org-typst-from-latex-with-naive
        org-typst-from-latex-fragment #'org-typst-from-latex-with-naive))

;;; 4. CLEAN UP THE UI
;; Hide the *bold* and /italic/ markers so you just see the style
(setq org-hide-emphasis-markers t)
;; Make LaTeX previews a bit larger so they match the text
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; Org configuration
(with-eval-after-load 'org
  (setq org-directory (expand-file-name "~/org")
        org-default-notes-file (concat org-directory "/inbox.org")
        org-agenda-files (list org-directory))

  ;; Create org directory if it doesn't exist
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  (setq org-display-inline-images t
        org-redisplay-inline-images t
        org-ellipsis "..."
        org-log-done 'time
        org-hide-emphasis-markers t
        org-startup-indented t
        org-startup-folded 'content)

  (use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; 2. Replace the bullets with a simple space string
  (setq org-superstar-headline-bullets-list '(" " " " " " " ")))
  
  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n  %i\n  %a")))

  ;; LaTeX preview settings
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))

  ;; Source code blocks
  (setq org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)

  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ditaa . t)
     (shell . t))))

(setq org-ditaa-jar-path "/opt/homebrew/bin/ditaa")
(setq org-babel-python-command "/opt/homebrew/bin/python3")

;; Evil-org integration
(use-package evil-org
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package denote
  :commands (denote denote-link denote-backlinks)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-rename-buffer-mode 1))

(use-package ox-hugo
  :after ox)

(with-eval-after-load 'ox-latex
  ;; Use minted for code blocks
  (setq org-latex-listings 'minted)

  ;; Add the minted package to all LaTeX exports
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  ;; Configure the PDF generation command to allow shell escape
  ;; (REQUIRED for minted to call Python)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))


  
  (add-to-list 'org-latex-classes
               '("ieee"
                 "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package jupyter
  :defer t
  :config
  (setq jupyter-server-use-environment-path t)
  
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
             '((jupyter . t))))))

;; texfrag - Inline LaTeX preview for any buffer
(use-package texfrag
  :commands (texfrag-mode texfrag-document texfrag-clear-document)
  :config
  (define-key texfrag-mode-map (kbd "C-c C-x C-l") 'texfrag-document))

;; Track texfrag preview state
(defvar-local my-texfrag-previews-active nil
  "Whether texfrag previews are currently shown.")

;; Function to toggle LaTeX preview in any buffer
(defun my-latex-preview-toggle ()
  "Toggle LaTeX preview in current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    ;; Use org's built-in preview for org-mode
    (call-interactively 'org-latex-preview))
   ((bound-and-true-p texfrag-mode)
    (if my-texfrag-previews-active
        (progn
          (texfrag-clear-document)
          (setq my-texfrag-previews-active nil)
          (message "LaTeX previews cleared"))
      (progn
        (texfrag-document)
        (setq my-texfrag-previews-active t)
        (message "LaTeX previews rendered"))))
   (t
    (message "Enable texfrag-mode first (M-x texfrag-mode)"))))

;; Global keybinding for LaTeX preview
(global-set-key (kbd "C-c l p") 'my-latex-preview-toggle)

(use-package auctex
  :ensure t
  :defer t
  :hook 
  (LaTeX-mode . (lambda ()
                  (reftex-mode t)       ; Turn on RefTeX for bibliographies
                  (flyspell-mode t)     ; Turn on on-the-fly spell checking
                  (LaTeX-math-mode t))) ; Enable easier math symbol insertion
  :config
  ;; Enable saving of parsed information (essential for referencing)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Default to PDF generation instead of DVI
  (setq TeX-PDF-mode t)

  ;; Ask for the "Master" file. 
  ;; Useful for multi-file documents (like a thesis) where you edit a chapter
  ;; but want to compile the main.tex file.
  (setq TeX-master nil)

  ;; correlate the source and the output (SyncTeX)
  ;; Allows you to Ctrl+Click in PDF to jump to code, and vice versa.
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex))

;; Optional: Configure RefTeX to play nice with AUCTeX
(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package mixed-pitch
  :ensure t
  :hook
  ;; Use mixed-pitch-mode in LaTeX-mode (AUCTeX) and standard latex-mode
  ((LaTeX-mode . mixed-pitch-mode)
   (latex-mode . mixed-pitch-mode))
  :config
  ;; Add LaTeX-specific faces to the list of faces that should REMAIN fixed-pitch
  ;; This prevents math and code macros from becoming variable-pitch.
  (dolist (face '(font-latex-math-face
                  font-latex-sedate-face
                  font-latex-warning-face
                  font-latex-verbatim-face
                  font-latex-script-char-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(use-package citar
  :custom
  (citar-bibliography '("~/Documents/Research Projects/bib/references.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-width)

  (setq pdf-view-midnight-colors 
        `(,(face-foreground 'default) . ,(face-background 'default)))

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq pdf-view-midnight-colors
                    `(,(face-foreground 'default) . ,(face-background 'default)))
              (pdf-view-midnight-minor-mode))))

;; Increase the amount of data Emacs reads from processes
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Eglot performance (already configured above, but good to document)
;; - eglot-events-buffer-size set to 0
;; - eglot-autoshutdown enabled
;; - jsonrpc logging disabled

;; Faster project indexing
(setq projectile-enable-caching t)

(message "Init loaded successfully!")

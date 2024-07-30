(setenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/snap/bin:/usr/local/go/bin:/usr/share/:/Library/TeX/texbin/:/Users/DTR/Library/Python/3.9/bin/")
(setq exec-path (append '("/usr/local/go/bin" "/usr/bin/jdt-language-server/bin" "/Library/TeX/texbin/" "/Users/DTR/Library/Python/3.9/bin/")
                        exec-path))

(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
                ("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

  (setq projectile-project-search-path '("~/Blimp/" "~/Blimp-Senior-Design/" "~/RDC/" ("~/git" . 1)))
  
(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package general
          :ensure t)
        (use-package evil
          :ensure t
          :init
          (setq evil-want-keybinding nil)
          (setq evil-want-integration t)
          :config
          (evil-mode 1))

        (use-package evil-collection
          :after evil
          :ensure t
          :config
          (evil-collection-init))
(require 'org-tempo)

  (use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))
    
        (require 'general)

        (general-create-definer my-leader-def
          :prefix "SPC")
      ;; ** Global Keybindings
      (my-leader-def
        :keymaps 'normal
        ;; bind "SPC a"
        "a" 'org-agenda
      ;;  "b" 'counsel-bookmark
        "c" 'org-capture
        "SPC" 'projectile-find-file
        "fp" 'projectile-switch-project
        "gp" 'projectile-grep
        "ff" 'find-file
        "wl" 'evil-window-right
        "wh" 'evil-window-left
        "wk" 'evil-window-up
        "wj" 'evil-window-down
        "wv" 'evil-window-vnew
        "ws" 'evil-window-new
        "wq" 'evil-quit
        "eb" 'eval-buffer
        "op" 'treemacs
        "ot" 'vterm
        "bb" 'ibuffer-jump
        "]" 'evil-next-buffer
        "[" 'evil-prev-buffer
        "ti" 'org-clock-in
        "to" 'org-clock-out
        "/" 'comment-line
        "x" 'helm-M-x
        "rf" 'org-roam-node-find
        "bk" 'kill-buffer
        )

    ;;  (define-key evil-normal-state-map (kbd "RET") 'org-toggle-todo-and-fold)
    ;;(define-key evil-normal-state-map (kbd "S") 'comment-line)
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

(setq inhibit-startup-screen t)
          (setq inhibit-startup-echo-area-message t)
          (setq inhibit-startup-message t)
          (setq initial-scratch-message nil)
          (setq initial-major-mode 'org-mode)
          (menu-bar-mode 0)
          (setq line-number-mode t)
          (setq display-line-numbers-mode 1)
          (setq-default indent-tabs-mode nil)
          (setq pop-up-windows nil)
          (tool-bar-mode 0)
          (tooltip-mode  0)
          (scroll-bar-mode 0)

;; use-package with package.el:
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

          (use-package which-key
          :straight t
            :init (which-key-mode)
            :diminish which-key-mode
            :config
            (setq which-key-idle-delay 0.3))

          (use-package eterm-256color
            :hook (term-mode . eterm-256color-mode))

          (use-package all-the-icons-dired
      :straight t
            :hook (dired-mode . all-the-icons-dired-mode))



  (set-frame-font "JetBrainsMono Nerd Font Mono 12" nil t)


          (use-package ido-vertical-mode
        :straight t
        )
          (require 'ido-vertical-mode)
          (ido-mode 1)
          (ido-vertical-mode 1)

          (use-package helm :straight t)

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


          (use-package dired
            :ensure nil
            :commands (dired dired-jump)
            :bind (("C-x C-j" . dired-jump))
            :config
            (evil-collection-define-key 'normal 'dired-mode-map
              "h" 'dired-up-directory
              "l" 'dired-find-file))

          (use-package dired-single
    :ensure t)


          (use-package tree-sitter
      :straight t
      )
          (use-package tree-sitter-langs
  :straight t
  )
          (require 'tree-sitter)
          (require 'tree-sitter-langs)
          ;; (use-package evil-nerd-commenter
          ;;   :bind ("gcc" . evilnc-comment-or-uncomment-lines))

          (setq backup-directory-alist            '((".*" . "~/.Trash")))

;; (straight-use-package
        ;; '(nano :type git :host github :repo "rougier/nano-emacs"))
      (straight-use-package
        '(org-margin :type git :host github :repo "rougier/org-margin"))
      (require 'org-margin)

      (straight-use-package
       '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))
      (require 'svg-tag-mode)
    (svg-tag-mode 1)

      (setq svg-tag-tags
            '(("TODO" . ((lambda (tag) (svg-tag-make tag))))))

    (setq svg-tag-tags
          '(("DONE" . ((lambda (tag) (svg-tag-make tag))))))

    (setq svg-tag-tags
          '(("CANCLED" . ((lambda (tag) (svg-tag-make tag))))))


      (straight-use-package
       '(notebook-mode :type git :host github :repo "rougier/notebook-mode"))
      (require 'notebook)

(straight-use-package
 '(pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))
(straight-use-package
 '(org-bib-mode :type git :host github :repo "rougier/org-bib-mode"))

(use-package olivetti
  :straight t
  )
(require 'olivetti)

(use-package modus-themes
    :straight t
    )
  (require 'modus-themes)
(modus-themes-select 'modus-operandi )            ; Light theme

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(defun ibuffer-advice (format)
  (with-current-buffer "*Ibuffer*"
    (save-excursion
    (let ((inhibit-read-only t))

      ;; Remove header and insert ours
      (goto-char (point-min))
      (search-forward "-\n" nil t)
      (delete-region 1 (point))
      (goto-char (point-min))
      (insert (concat
               (propertize "\n" 'face '(:height 1.2))
               (propertize " "  'display `(raise +0.25))
               (propertize "  Buffers list (ibuffer)"
                           'face 'nano-faded)
               (propertize " "  'display `(raise -0.35))
               "\n"))
      (insert "")

      ;; Transform titles
      (goto-char (point-min))
      (while (re-search-forward "\\[ \\(.*\\) \\]" nil t)
        (let* ((title (match-string 0))
               (property (get-text-property 0 'ibuffer-filter-group-name title)))
          (replace-match "\n")
          (insert (concat
                   (propertize
                    (format "   %s " (substring title 2 -2))
                    'ibuffer-filter-group-name property)
                   (propertize
                    (make-string (- 30 (length title)) ?—)
                    'face 'nano-faded)
                   "\n"))))))))


(setq ibuffer-saved-filter-groups
       '(("home"
              ("Configuration" (or (filename . ".emacs.d")
                                           (filename . "emacs-config")))
              ("Org" (or (mode . org-mode)
                             (filename . "OrgMode")))
          ("Code" (or  (derived-mode . prog-mode)
                       (mode . ess-mode)
                       (mode . compilation-mode)))
          ("Text" (and (derived-mode . text-mode)
                       (not  (starred-name))))
          ("TeX"  (or (derived-mode . tex-mode)
                      (mode . latex-mode)
                      (mode . context-mode)
                      (mode . ams-tex-mode)
                      (mode . bibtex-mode)))
              ("Help" (or (name . "\*Help\*")
                              (name . "\*Apropos\*")
                              (name . "\*info\*"))))))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-header-line nil)
(setq ibuffer-eliding-string (propertize "…" 'face 'nano-salient))
(setq ibuffer-fontification-alist '((0 t nano-salient)))
(setq ibuffer-formats
      '(("  "  mark " "(name 24 24 :left :elide) "  " modified)
        (mark " " (name 16 -1) " " filename)))

(defun ibuffer-setup ()
  (ibuffer-switch-to-saved-filter-groups "home")
  (ibuffer-auto-mode 1))

(defun nano-sidebar-init-ibuffer (frame sidebar)
  "Default sidebar initialization"

  (select-frame frame)
  (let ((buffer (current-buffer)))
    (ibuffer)
    (switch-to-buffer buffer))
  (select-frame sidebar)
  (switch-to-buffer "*Ibuffer*")
  (set-window-dedicated-p (get-buffer-window "*Ibuffer*") t)
  (hl-line-mode)
  (setq header-line-format nil)
  (setq mode-line-format nil))


(setq nano-sidebar-default-init 'nano-sidebar-init-ibuffer)
(advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-advice)
(add-hook 'ibuffer-mode-hook #'ibuffer-setup)

(require 'eglot)
    (use-package company
      :ensure t
    :init (global-company-mode)
    )
      (require 'company)
      (straight-use-package
       '(yasnippet :type git :host github :repo "joaotavora/yasnippet"))

(straight-use-package
       '(origami :type git :host github :repo "gregsexton/origami.el"))
(require 'origami)

    (require 'yasnippet)
  (use-package yasnippet-snippets
    :straight t)

(yas-reload-all)
    (yas-global-mode 1)

(use-package magit
  :straight t
  )

(use-package eldoc-box
  :straight t
  )
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package go-mode
      :straight t)
    (require 'go-mode)
          (require 'project)

        (defun project-find-go-module (dir)
          (when-let ((root (locate-dominating-file dir "go.mod")))
            (cons 'go-module root)))
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

        (cl-defmethod project-root ((project (head go-module)))
          (cdr project))

        (add-hook 'project-find-functions #'project-find-go-module)

  (setq-default eglot-workspace-configuration
      '((:gopls .
          ((staticcheck . t)
           (matcher . "CaseSensitive")))))


    (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook
      (lambda ()
          (call-interactively 'eglot-code-action-organize-imports))
      nil t)

(use-package python-mode
  :straight t
    :custom
  (python-shell-interpreter "python3")
  :hook (python-mode . eglot-ensure))

  (use-package auto-virtualenv
:straight t)
  (require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

(use-package eglot-java
  :straight t
  )
(add-hook 'java-mode-hook 'eglot-java-mode)

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(use-package tide
  :straight t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(setq org-startup-folded t)

  (use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  )
  (require 'org-roam)
  (setq org-roam-directory (file-truename "~/RoamNotes"))
  (org-roam-db-autosync-mode)

(setq org-agenda-files (quote ("~/org/todo.org"
                                   "~/org/inbox.org"
                                )))

      (add-hook 'org-mode-hook 'notebook-mode)
      (add-hook 'org-mode-hook 'org-modern-mode)
(setq org-default-notes-file "~/org/inbox.org")
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                  ("PHONE" :foreground "forest green" :weight bold))))

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

(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (defvar my/variable-width-font "JetBrainsMono Nerd Font Mono")
  (defvar my/fixed-width-font "JetBrainsMono Nerd Font Mono")
    ;; Org Mode Appearance ------------------------------------

    ;; Load org-faces to make sure we can set appropriate faces
    (require 'org-faces)

    ;; Hide emphasis markers on formatted text
    (setq org-hide-emphasis-markers t)

    ;; Resize Org headings
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

    ;; Make the document title a bit bigger
    (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

    ;;; Centering Org Documents --------------------------------

    ;; Install visual-fill-column
    (unless (package-installed-p 'visual-fill-column)
      (package-install 'visual-fill-column))

    ;; Configure fill width
    (setq visual-fill-column-width 110
          visual-fill-column-center-text t)

    ;;; Org Present --------------------------------------------

    ;; Install org-present if needed
    (unless (package-installed-p 'org-present)
      (package-install 'org-present))

    (defun my/org-present-prepare-slide (buffer-name heading)
      ;; Show only top-level headlines
      (org-overview)

      ;; Unfold the current entry
      (org-show-entry)

      ;; Show only direct subheadings of the slide but don't expand them
      (org-show-children))

    (defun my/org-present-start ()
      ;; Tweak font sizes
      (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                         (header-line (:height 4.0) variable-pitch)
                                         (org-document-title (:height 1.75) org-document-title)
                                         (org-code (:height 1.55) org-code)
                                         (org-verbatim (:height 1.55) org-verbatim)
                                         (org-block (:height 1.25) org-block)
                                         (org-block-begin-line (:height 0.7) org-block)))

      ;; Set a blank header line string to create blank space at the top
      (setq header-line-format " ")

      ;; Display inline images automatically
      (org-display-inline-images)

      ;; Center the presentation and wrap lines
      (visual-fill-column-mode 1)
      (visual-line-mode 1))

    (defun my/org-present-end ()
      ;; Reset font customizations
      (setq-local face-remapping-alist '((default variable-pitch default)))

      ;; Clear the header line string so that it isn't displayed
      (setq header-line-format nil)

      ;; Stop displaying inline images
      (org-remove-inline-images)

      ;; Stop centering the document
      (visual-fill-column-mode 0)
      (visual-line-mode 0))

    ;; Turn on variable pitch fonts in Org Mode buffers
    (add-hook 'org-mode-hook 'variable-pitch-mode)

    ;; Register hooks with org-present
    (add-hook 'org-present-mode-hook 'my/org-present-start)
    (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
    (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(use-package ox-hugo
  :straight t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(use-package latex-preview-pane
  :straight t)

(latex-preview-pane-enable)

 (straight-use-package
            '(org-auctex :type git :host github :repo "karthink/org-auctex"))
          (require 'org-auctex)

(straight-use-package
            '(org-modern :type git :host github :repo "minad/org-modern"))
          (require 'org-modern)


  
    ;; Minimal UI
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (modus-themes-load-operandi)

  ;; Choose some fonts
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font Mono 12")
  (set-face-attribute 'variable-pitch nil :family "JetBrainsMono Nerd Font Mono 12")
  (set-face-attribute 'org-modern-symbol nil :family "JetBrainsMono Nerd Font Mono 12")

  ;; Add frame borders and window dividers
  ;; (modify-all-frames-parameters
   ;; '((right-divider-width . 40)
     ;; (internal-border-width . 40)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode)

(use-package mu4e
  :straight t
  )

(setenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/snap/bin:/usr/local/go/bin:/usr/share/:/Library/TeX/texbin/:/Users/DTR/Library/Python/3.9/bin/:/opt/homebrew/bin/")
(setq exec-path (append '("/usr/local/go/bin" "/usr/bin/jdt-language-server/bin" "/Library/TeX/texbin/" "/Users/DTR/Library/Python/3.9/bin/" "/opt/homebrew/bin/")
                        exec-path))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(setq projectile-project-search-path '("~/Blimp/" "~/Blimp-Senior-Design/" "~/RDC/" ("~/git" . 1)))

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
        "op" 'neotree
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

;; Enable relative line numbers globally
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Optionally disable line numbers in certain modes
(dolist (mode '(
             term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

              (setq inhibit-startup-screen t)
              (setq inhibit-startup-echo-area-message t)
              (setq inhibit-startup-message t)
              (setq initial-scratch-message nil)
              (setq initial-major-mode 'org-mode)
              (menu-bar-mode 0)
              (setq line-number-mode t)
              (setq-default indent-tabs-mode nil)
              (setq pop-up-windows nil)
              (tool-bar-mode 0)
              (tooltip-mode  0)
              (scroll-bar-mode 0)
  (add-hook 'image-mode-hook
    (lambda ()
      (auto-revert-mode)
      (auto-image-file-mode)))

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



      (set-frame-font "JetBrainsMono Nerd Font Mono 14" nil t)


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
                    "\\*Warnings\\*"
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



              (use-package tree-sitter
          :straight t
          )
              (use-package tree-sitter-langs
      :straight t
      )
              (require 'tree-sitter)
              (require 'tree-sitter-langs)

              (setq backup-directory-alist            '((".*" . "~/.Trash")))

;;(straight-use-package
  ;;'(nano :type git :host github :repo "rougier/nano-emacs"))
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

(use-package olivetti
  :straight t
  )
(require 'olivetti)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(require 'doom-modeline)
(doom-modeline-mode 1)

(use-package neotree
  :straight t)
(require 'neotree)

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

(straight-use-package 'rustic)
(setq rustic-lsp-client 'eglot)

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

(use-package dslide
    :straight (dslide :type git :host github
                      :repo "positron-solutions/dslide"))

(use-package latex-preview-pane
  :straight t)

(latex-preview-pane-enable)

 (straight-use-package
            '(org-auctex :type git :host github :repo "karthink/org-auctex"))
          (require 'org-auctex)

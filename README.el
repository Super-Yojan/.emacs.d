(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
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

;;(setq package-enable-at-startup nil)
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(straight-use-package 'org)

(use-package general
      :ensure t)
    (use-package evil
      :ensure t
      :init
      (setq evil-want-integration t)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after evil
      :ensure t
      :config
      (evil-collection-init))

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
    "ns" 'elscreen-create
    "]" 'evil-next-buffer
    "[" 'evil-prev-buffer
    "ti" 'org-clock-in
    "to" 'org-clock-out
    "/" 'comment-line
    "x" 'helm-M-x
    )

  (define-key evil-normal-state-map (kbd "RET") 'org-toggle-todo-and-fold)
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
(setq line-number-mode t)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package ido-vertical-mode)
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

(use-package dired-single)


(use-package tree-sitter)
(use-package tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
;; (use-package evil-nerd-commenter
;;   :bind ("gcc" . evilnc-comment-or-uncomment-lines))

(setq backup-directory-alist            '((".*" . "~/.Trash")))

(straight-use-package
          '(nano :type git :host github :repo "rougier/nano-emacs"))
        (straight-use-package
          '(org-margin :type git :host github :repo "rougier/org-margin"))
        (require 'org-margin)
      ;; TODO Add hook to enable org-margin
        (straight-use-package 'mini-frame)
      (require 'mini-frame)
        (require 'nano)
(require 'nano-faces)

        (straight-use-package
         '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))
        (require 'svg-tag-mode)
      (svg-tag-mode 1)

        (setq svg-tag-tags
              '(("TODO" . ((lambda (tag) (svg-tag-make "TODO"))))))

      (setq svg-tag-tags
            '(("DONE" . ((lambda (tag) (svg-tag-make "DONE"))))))

        (straight-use-package
         '(notebook-mode :type git :host github :repo "rougier/notebook-mode"))
        (require 'notebook)

      (straight-use-package
       '(nano-vertico :type git :host github :repo "rougier/nano-vertico"))
      (require 'nano-vertico)
      (nano-vertico-mode 1)

    ;;(straight-use-package
     ;;  '(svg-lib :type git :host github :repo "rougier/svg-lib"))
      ;;(require 'svg-lib)


  (straight-use-package
   '(org-imenu :type git :host github :repo "rougier/org-imenu"))

  (straight-use-package
   '(pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))
  (straight-use-package
   '(org-bib-mode :type git :host github :repo "rougier/org-bib-mode"))

  (straight-use-package
   '(nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer"))

  (require 'nano-minibuffer)

(straight-use-package '(nano-sidebar :type git :host github
                                     :repo "rougier/nano-sidebar"))
(require 'nano-sidebar)

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

    (require 'yasnippet)
  (use-package yasnippet-snippets
    :straight t)

(yas-reload-all)
    (yas-global-mode 1)

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

(use-package org-roam
  :straight t
  )

(require 'org-roam)
(setq org-roam-directory (file-truename "~/org"))
(org-roam-db-autosync-mode)

;; Don't know what it does but it's here
(setq org-roam-node-display-template
    (concat "${title:*} "
            (propertize "${tags:10}" 'face 'org-tag)))

 (("d" "default" plain "%?"
:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                   "#+title: ${title}\n")
:unnarrowed t))

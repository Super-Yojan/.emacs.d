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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(setq projectile-project-search-path '("~/Blimp/" "~/Blimp-Senior-Design/" "~/RDC/" ("~/github" . 1)))

(straight-use-package
  '(nano :type git :host github :repo "rougier/nano-emacs"))
(straight-use-package 'mini-frame)
(require 'nano)

(straight-use-package

 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

(require 'svg-tag-mode)

(setq svg-tag-tags
      '(("TODO" . ((lambda (tag) (svg-tag-make "TODO"))))))

(straight-use-package
 '(notebook-mode :type git :host github :repo "rougier/notebook-mode"))

(require 'notebook)

(use-package perspective
  :straight t  ; use `:straight t` if using straight.el!
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode))

;;(require 'nano-splash)
;;(require 'nano-modeline)
;;(require 'nano-mu4e)
;;(require 'nano-layout)
;;(require 'nano-minibuffer)
;;(require 'nano-color)
;;(require 'notebook-mode)
;;(format "[[https://www.gnu.org/software/emacs/][Org mode]] %s"
 ;;       (org-version nil nil))

(getenv "PATH")

(load-file "~/.emacs.d/01_config_keybinds.el")
(load-file "~/.emacs.d/00_config_agenda.el")
(load-file "~/.emacs.d/coding/00_lsp_setup.el")
(load-file "~/.emacs.d/coding/02_config_coding_rust.el")
(load-file "~/.emacs.d/coding/03_config_cpp.el")
(load-file "~/.emacs.d/coding/04_config_vhdl.el")
(load-file "~/.emacs.d/coding/01_python.el")
;;(load-file "~/.emacs.d/ui/elegance.el")
(load-file "~/.emacs.d/ui/sanity.el")
;;(load-file "~/.emacs.d/notebook-mode/notebook.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" default))
 '(helm-minibuffer-history-key "M-p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

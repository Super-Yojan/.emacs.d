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


(load-file "~/.emacs.d/00_config_agenda.el")
(load-file "~/.emacs.d/01_config_keybinds.el")
(load-file "~/.emacs.d/coding/00_lsp_setup.el")
(load-file "~/.emacs.d/coding/02_config_coding_rust.el")
(load-file "~/.emacs.d/coding/03_config_cpp.el")
(load-file "~/.emacs.d/coding/04_config_vhdl.el")
(load-file "~/.emacs.d/ui/elegance.el")
(load-file "~/.emacs.d/ui/sanity.el")



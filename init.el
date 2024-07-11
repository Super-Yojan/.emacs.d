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

  (straight-use-package 'org)
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


(org-babel-load-file "~/.emacs.d/README.org")

;;(server-start) 

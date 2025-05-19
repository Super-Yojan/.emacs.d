;; -*- lexical-binding: t; -*-

;; --- Garbage Collection Optimization ---
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))))

;; --- Disable UI Elements ---
(setq default-frame-alist (append '((tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (vertical-scroll-bars . nil))
                                  default-frame-alist))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; --- Inhibit Startup Screen ---
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(message "Early-init.el loaded at %s." (current-time-string))

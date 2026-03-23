;; -*- lexical-binding: t; -*-

;; Fix seq-empty-p for Emacs 30 + Evil compatibility
(require 'cl-lib)
(require 'seq)
(cl-defmethod seq-empty-p ((_ symbol)) nil)

;; Prevent package.el (we use straight.el)
(setq package-enable-at-startup nil)

;; Native compilation
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-compile-prune-cache t))

;; GC: high during init, reasonable after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Disable UI early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      frame-inhibit-implied-resize t)

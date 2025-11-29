;; -*- lexical-binding: t; -*-
;; early-init.el for Emacs 30

;; ============================================================
;; CRITICAL: Fix seq-empty-p for Emacs 30 + Evil compatibility
;; Must be done BEFORE anything else loads
;; ============================================================
(require 'cl-lib)
(require 'seq)

;; Add method for symbol type (Evil passes 'normal, 'insert, etc.)
(cl-defmethod seq-empty-p ((_ symbol)) nil)

;; ============================================================
;; Standard early-init settings
;; ============================================================

;; Prevent package.el from loading (we use straight.el)
(setq package-enable-at-startup nil)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (setq native-compile-prune-cache t))

;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Disable UI early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Enable the transparent title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Set the appearance to dark (so the text is white)
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; Prevent glimpse of un-styled Emacs
(setq frame-inhibit-implied-resize t)

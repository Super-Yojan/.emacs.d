(use-package general)
(require 'general)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))



(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  ;; bind "SPC a"
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture
  "SPC" 'dired
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
  "]" 'elscreen-next
  "[" 'elscreen-previous
  "ti" 'org-clock-in
  "to" 'org-clock-out
 )


(define-key evil-normal-state-map (kbd "RET") 'org-toggle-todo-and-fold)

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



(use-package general)
(require 'general)

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
  "wj" 'evil-window-bottom
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


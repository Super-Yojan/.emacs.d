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
  "l" 'evil-window-right
  "h" 'evil-window-left
  "k" 'evil-window-up
  "j" 'evil-window-bottom
  "[" 'evil-buffer
  "wv" 'evil-window-vnew
  "ws" 'evil-window-new
  "wq" 'evil-quit
  "eb" 'eval-buffer
  "op" 'treemacs
  "ot" 'vterm
  )

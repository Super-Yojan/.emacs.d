;;; human-aug-theme.el --- Human Augmentation Dark Theme

(deftheme human-aug
  "A Dark Mode theme for Human Augmentation (Midnight Blue variant).")

(let ((class '((class color) (min-colors 89)))
      ;; UPDATED: Background is now Slate 900 (#0f172a) for lighter blue feel
      (bg      "#0f172a")  
      (bg-alt  "#1e293b")  ; Slate 800
      (bg-hl   "#334155")  ; Slate 700
      (fg      "#e2e8f0")  ; Slate 200
      (fg-dim  "#94a3b8")  ; Slate 400
      (comment "#64748b")  ; Slate 500
      
      ;; Accents
      (cyan    "#a3d5ff")  ; Engineer Blue
      (purple  "#c4a6e0")  ; Artist Purple
      (yellow  "#fbfda6")  ; Athlete Yellow
      (red     "#fda4af")  
      (green   "#86efac"))

  (custom-theme-set-faces
   'human-aug
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,cyan))))
   `(region ((t (:background ,bg-hl :extend t))))
   `(fringe ((t (:background ,bg :foreground ,comment))))
   `(minibuffer-prompt ((t (:foreground ,cyan :weight bold))))
   `(mode-line ((t (:background ,bg-alt :foreground ,cyan :box (:line-width 1 :color ,bg-alt)))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-string-face ((t (:foreground ,yellow))))
   `(font-lock-keyword-face ((t (:foreground ,cyan :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,purple :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,cyan))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(org-level-1 ((t (:foreground ,cyan :height 1.2 :weight bold))))
   `(org-level-2 ((t (:foreground ,purple :height 1.1 :weight bold))))
   `(org-level-3 ((t (:foreground ,yellow :weight bold))))
   `(org-block ((t (:background ,bg-alt))))
   ))

(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'human-aug)

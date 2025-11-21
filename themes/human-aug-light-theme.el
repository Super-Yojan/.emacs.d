;;; human-aug-light-theme.el --- Human Augmentation Light Theme

(deftheme human-aug-light
  "A Light Mode theme for Human Augmentation.")

(let ((class '((class color) (min-colors 89)))
      ;; Light Mode Palette
      (bg      "#ffffff")  ; White
      (bg-alt  "#f1f5f9")  ; Slate 100
      (bg-hl   "#e2e8f0")  ; Slate 200
      (fg      "#0f172a")  ; Slate 900
      (fg-dim  "#475569")  ; Slate 600
      (comment "#64748b")  ; Slate 500
      
      ;; Accents (Darkened for white background)
      (cyan    "#0369a1")  ; Sky 700
      (purple  "#7e22ce")  ; Purple 700
      (yellow  "#b45309")  ; Amber 700
      (red     "#be123c")  
      (green   "#15803d"))

  (custom-theme-set-faces
   'human-aug-light
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,cyan))))
   `(region ((t (:background ,bg-hl :extend t))))
   `(fringe ((t (:background ,bg :foreground ,comment))))
   `(minibuffer-prompt ((t (:foreground ,cyan :weight bold))))
   `(mode-line ((t (:background ,bg-alt :foreground ,cyan :box (:line-width 1 :color ,bg-hl)))))
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

(provide-theme 'human-aug-light)

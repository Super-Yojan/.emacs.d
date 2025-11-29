;;; human-aug-theme.el --- Human Augmentation Dark Theme (GitHub Edition)

(deftheme human-aug
  "A GitHub-inspired Dark Mode theme for Human Augmentation.")

(let ((class '((class color) (min-colors 89)))
      ;; GITHUB DARK PALETTE
      (bg      "#0d1117")  ; Canvas Main
      (bg-alt  "#161b22")  ; Canvas Subtler (Blocks/Modeline)
      (bg-hl   "#264f78")  ; Selection Blue
      (border  "#30363d")  ; UI Borders
      
      (fg      "#c9d1d9")  ; FG Default
      (fg-dim  "#8b949e")  ; FG Muted (Comments/Docstrings)
      
      ;; SYNTAX ACCENTS
      (keyword "#ff7b72")  ; Red/Pink (The classic GitHub keyword)
      (func    "#d2a8ff")  ; Purple (Functions)
      (string  "#a5d6ff")  ; Light Blue (Strings)
      (const   "#79c0ff")  ; Cyan-Blue (Constants)
      (var     "#c9d1d9")  ; Default FG (Variables usually plain in GH)
      (type    "#ff7b72")  ; Pink (Types)
      (warning "#d29922")  ; Yellow/Orange
      (success "#3fb950")) ; Green

  (custom-theme-set-faces
   'human-aug
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,fg))))
   `(region ((t (:background ,bg-hl :extend t))))
   `(fringe ((t (:background ,bg :foreground ,border))))
   `(vertical-border ((t (:foreground ,border))))
   
   ;; UI ELEMENTS: Mimic GitHub's bordered containers
   `(minibuffer-prompt ((t (:foreground ,const :weight bold))))
   `(mode-line ((t (:background ,bg-alt :foreground ,fg :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,fg-dim :box (:line-width 1 :color ,border)))))
   `(line-number ((t (:foreground ,fg-dim :background ,bg))))
   `(line-number-current-line ((t (:foreground ,fg :background ,bg-alt))))

   ;; SYNTAX HIGHLIGHTING
   `(font-lock-comment-face ((t (:foreground ,fg-dim))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-keyword-face ((t (:foreground ,keyword :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,func :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,var))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-constant-face ((t (:foreground ,const))))
   `(font-lock-warning-face ((t (:foreground ,warning :weight bold))))

   ;; ORG MODE: Clean, flat, with distinct header colors
   `(org-level-1 ((t (:foreground ,fg :weight bold :height 1.0))))
   `(org-level-2 ((t (:foreground ,func :weight bold :height 1.0))))
   `(org-level-3 ((t (:foreground ,const :weight bold :height 1.0))))
   `(org-level-4 ((t (:foreground ,fg-dim :weight bold :height 1.0))))
   `(org-document-title ((t (:foreground ,fg :weight bold :height 1.2))))
   
   ;; Code blocks that look like embedded Gists
   `(org-code ((t (:foreground ,string :background ,bg-alt))))
   `(org-block ((t (:background ,bg-alt :foreground ,fg))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-alt))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-alt))))
   ))

(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'human-aug)

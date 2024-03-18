
;; Org Mode Configuration for LaTeX Export with Assignment Template

;; Load org-mode and org-latex
(use-package latex-preview-pane)
(require 'org)
(require 'ox-latex)


;; Set default LaTeX class for export to article
(setq org-latex-default-class "article")

;; Define a template for assignments
(setq org-latex-assignment-template
      "\\documentclass[12pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath,amsfonts,amssymb}
\\usepackage{graphicx}

\\title{<<title>>}
\\author{<<author>>}
\\date{\\today}

\\begin{document}

\\maketitle

\\section*{Instructions}
<<instructions>>

\\section*{Questions}
\\begin{enumerate}
<<questions>>
\\end{enumerate}

\\end{document}")

;; Function to insert a new assignment
(defun insert-assignment ()
  (interactive)
  (let ((title (read-string "Title: "))
        (author (read-string "Author: "))
        (instructions (read-string "Instructions: "))
        (questions (read-string "Questions (separated by comma): ")))
    (insert (replace-regexp-in-string "<<title>>" title
              (replace-regexp-in-string "<<author>>" author
                (replace-regexp-in-string "<<instructions>>" instructions
                  (replace-regexp-in-string "<<questions>>" (mapconcat 'identity (split-string questions ",") "\n") org-latex-assignment-template)))))))

;; Key binding for inserting new assignments
(global-set-key (kbd "C-c a") 'insert-assignment)



;;; brand-export.el --- Human Augmentation HTML Export -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'htmlize)

;; The JavaScript to inject: Theme Toggle + Copy Button Logic
(defconst human-aug/theme-toggle-js
  "<script>
    document.addEventListener('DOMContentLoaded', () => {
        // --- 1. Theme Toggle Logic ---
        const body = document.body;
        const toggleBtn = document.createElement('button');
        toggleBtn.id = 'theme-toggle';
        
        const savedTheme = localStorage.getItem('theme');
        if (savedTheme === 'light') {
            body.classList.add('light-mode');
            toggleBtn.textContent = '🌙 Dark Mode';
        } else {
            toggleBtn.textContent = '☀️ Light Mode';
        }

        toggleBtn.onclick = () => {
            body.classList.toggle('light-mode');
            const isLight = body.classList.contains('light-mode');
            localStorage.setItem('theme', isLight ? 'light' : 'dark');
            toggleBtn.textContent = isLight ? '🌙 Dark Mode' : '☀️ Light Mode';
        };
        document.body.appendChild(toggleBtn);

        // --- 2. Code Block Copy Buttons ---
        // Target the container for code blocks
        const codeBlocks = document.querySelectorAll('.org-src-container');

        codeBlocks.forEach(container => {
            // Find the pre tag containing the code
            const pre = container.querySelector('pre.src');
            if (!pre) return;

            // Create the copy button
            const copyBtn = document.createElement('button');
            copyBtn.className = 'copy-btn';
            copyBtn.textContent = 'Copy';

            // Add copy functionality
            copyBtn.onclick = () => {
                // Get the text, ignoring the 'OUTPUT:' labels if any
                const codeText = pre.textContent;
                navigator.clipboard.writeText(codeText).then(() => {
                    const originalText = copyBtn.textContent;
                    copyBtn.textContent = 'Copied!';
                    setTimeout(() => {
                        copyBtn.textContent = originalText;
                    }, 2000);
                });
            };

            // Append button to the container (it will be positioned absolute top-right)
            container.appendChild(copyBtn);
        });
    });
  </script>")



;;;###autoload
(defun human-aug/export-html ()
  "Export the current Org buffer to an HTML file using the Brand Notebook CSS.
This function ensures that:
1. Code blocks use clean CSS classes instead of inline styles (fixing the 'blocky' artifacts).
2. The 'brand_notebook.css' file is linked automatically.
3. Default Org Mode styles are suppressed to prevent conflicts."
  (interactive)
  (let* ((curr-file (concat buffer-file-name ))
           
         ;; 1. Force htmlize to use CSS classes (.org-keyword) instead of 
         ;;    hardcoded inline styles (style="color: #..."). 
         ;;    This is what fixes the 'screenshot' look.
         ;;(org-html-htmlize-output-type 'css)
         (htmlize-output-type 'css)
         
         ;; 2. Disable default Org CSS bloat
         (org-html-head-include-default-style nil)
         ;;(org-html-head-include-scripts nil) 
         
         ;; 3. Inject your Brand CSS and Fonts

         
         ;; 4. Define output filename
         (outfile (concat (file-name-sans-extension curr-file) ".html")))
    
    ;; Perform the export
    (htmlize-file outfile)
    (message "✅ Exported to %s using Human Augmentation Theme" outfile)))

(provide 'brand-export)

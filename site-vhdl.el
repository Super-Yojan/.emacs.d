;
; Site-specific lisp for VHDL-mode.
;
(custom-set-variables
 '(vhdl-underscore-is-part-of-word t)
 '(vhdl-upper-case-constants t)
 '(vhdl-hideshow-menu nil)
 '(vhdl-index-menu nil)
 '(vhdl-indent-tabs-mode nil)
 '(vhdl-reset-name "rst")
 '(vhdl-clock-name "clk")
 '(vhdl-speedbar-save-cache nil)
 '(vhdl-date-format "%d/%m/%Y %r")
 '(vhdl-testbench-create-files (quote separate))
 '(vhdl-testbench-entity-name (quote (".*" . "tb_\\&")))
 '(vhdl-testbench-architecture-name (quote (".*" . "tb_\\&_rtl")))
 '(vhdl-testbench-configuration-name (quote ("\\(.*\\) \\(.*\\)" . "\\2_cfg")))
 '(vhdl-testbench-architecture-file-name (quote ("\\(.*\\) \\(.*\\)" . "\\2")))
 '(vhdl-testbench-initialize-signals t)
 '(vhdl-standard (quote (93 nil)))
 '(vhdl-include-direction-comments nil)
 '(vhdl-include-group-comments (quote always))
 '(vhdl-include-type-comments nil)
 '(vhdl-include-port-comments nil)
 )


; Load other sections of the VHDL configuration
(load "~/.emacs.d/site-vhdl-templates.el") ; Load my VHDL defaults
(load "~/.emacs.d/site-vhdl-projects.el") ; Load my VHDL defaults
(load "~/.emacs.d/site-vhdl-compilers.el") ; Load my VHDL defaults


; Permanantly enable syntax highlighting
;(global-font-lock-mode t)


; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)


; Set up a highlighter for code that is not finished or needs to be looked at
(defun my-highlight-fixme ()
  (interactive)
  (font-lock-mode 1)
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|XXX\\|!!!\\)" 1 font-lock-warning-face prepend))))
(add-hook 'vhdl-mode-hook 'my-highlight-fixme)

;
; make-vhdl-project function
;
;(defun make-vhdl-makefile (projname)
(defun make-vhdl-makefile ()
  "Generate the makefile and make the specified VHDL project"
;  (interactive "s")
  (vhdl-mode)
  (vhdl-set-project vhdl-project)
  (vhdl-generate-makefile)
  )


(use-package rust-mode
  :ensure t
  :defer t
  :init
  (general-define-key
   :states '(normal motion)
   :prefix "r"
   "ct" 'cargo-process-test
   "cb" 'cargo-process-build
   "cr" 'cargo-process-run
   "ce" 'cargo-process-bench
   (kbd "c RET") 'cargo-process-fmt
   )
  )

(use-package cargo
  :ensure t
  :after rust-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  )

(use-package flycheck-rust
  :ensure t
  :defer t
  :init
  
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  )

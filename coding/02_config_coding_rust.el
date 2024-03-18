(use-package rust-mode
  :ensure t
  :defer t
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

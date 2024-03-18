(use-package python-mode
  :ensure nil
  :custom
  (python-shell-interpreter "python3")
  :hook (python-mode . lsp-deferred))
)

(setq lsp-python-ms-executable "/Users/DTR/Library/Python/3.8/bin/pylsp")


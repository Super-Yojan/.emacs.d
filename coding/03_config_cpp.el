;; === General Configuration
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package ccls)
(require 'ccls)
(setq ccls-executable "/opt/homebrew/bin/ccls")

(use-package platformio-mode)
(require 'platformio-mode)

;; Enable ccls for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook (lambda ()
                           (lsp-deferred)
                           (platformio-conditionally-enable)))

;; === CMake ===
(use-package cmake-mode
  :ensure t
  :defer t
  :init
  ; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
	(append
	 '(("CMakeLists\\.txt\\'" . cmake-mode))
	 '(("\\.cmake\\'" . cmake-mode))
	 auto-mode-alist))
  )


(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
      '("melpa-stable" . "https://stable.melpa.org/packages/"))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; cider
(use-package cider
  :ensure t
  :pin melpa-stable)

;; try packagaes without installing them
(use-package try
  :ensure t)

;; show ido files vertically
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

;; fuzzy search like sublime using flx-ido
(use-package flx-ido
  :ensure t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil))
  :config
  (flx-ido-mode 1))

;; paredit
(use-package paredit
  :ensure t)

;; the almighty parinifer
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

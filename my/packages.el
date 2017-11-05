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
  :pin melpa-stable
  :config (setq cider-repl-display-help-banner nil))

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

;; use projectile for better search and project management
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;; goto a char on avy
(use-package avy
  :ensure t
  :bind ("C-z" . avy-goto-char))

;; let emacs help you
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; color code parenthesis
(use-package rainbow-delimiters
  :ensure t
  :config
  (setq global-rainbow-delimiters-mode 1))

;; make sure the indentation in fine with clojure and clojurescript 
(use-package clojure-mode
  :ensure t)

;; load .graphql files gracefully
(use-package graphql-mode
  :ensure t)

;; feel a little more at home with neotree
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

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
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

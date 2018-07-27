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
  :defer t
  :pin melpa-stable
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))


;; try packagaes without installing them
(use-package try
  :defer t
  :ensure t)

;; show ido files vertically
(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init (ido-vertical-mode 1))

;; fuzzy search like sublime using flx-ido
(use-package flx-ido
  :ensure t
  :defer t
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
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
        (append '(
                  "elpa/"))
    projectile-globally-ignored-directories)
  (setq projectile-globally-ignored-files)
  (append '()
      ".#*"
    projectile-globally-ignored-files))

;; goto a char on avy
(use-package avy
  :ensure t
  :defer t
  :bind ("C-z" . avy-goto-char))

;; let emacs help you
(use-package which-key
  :ensure t
  :defer t
  :init (which-key-mode))

;; color code parenthesis
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; make sure the indentation in fine with clojure and clojurescript
(use-package clojure-mode
  :defer t
  :ensure t)

;; load .graphql files gracefully
(use-package graphql-mode
  :defer t
  :ensure t)

;; feel a little more at home with neotree
(use-package neotree
  :ensure t
  :defer t
  :config
  (set-face-attribute 'neo-file-link-face nil :foreground "white")
  (set-face-attribute 'neo-dir-link-face nil :foreground "cyan")
  (global-set-key [f8] 'neotree-toggle))

;; complete anything mode
(use-package company
  :ensure t
  :defer t
  :pin melpa-stable
  :init
  (global-company-mode)
  (setq company-begin-commands
        '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash cljr-slash)))

;; The almighty paredit
(use-package paredit
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

;; hope that there will be a complete config for org-mode someday
(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (progn))

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))





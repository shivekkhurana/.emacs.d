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
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
        (append '(
                  "elpa/"
		  )
		projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
	(append '(
		  ".#*"
		  )
		projectile-globally-ignored-files)))
      

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
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; make sure the indentation in fine with clojure and clojurescript
(use-package clojure-mode
 :ensure t)

;; load .graphql files gracefully
(use-package graphql-mode
  :ensure t)

;; edit js files while maintaining sanity
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.json\\'" . web-mode)
	 ("\\.jsx?\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js\\'")))
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

;; lint all the js
(use-package flycheck
  :ensure t
  :defer t
  :init
  (progn
    (global-flycheck-mode)
    (add-hook 'flycheck-mode-hook (lambda ()
				    (let* ((root (locate-dominating-file
						  (or (buffer-file-name) default-directory)
						  "node_modules"))
					   (eslint (and root
							(expand-file-name "node_modules/eslint/bin/eslint.js"
									  root))))
				      (when (and eslint (file-executable-p eslint))
					(setq-local flycheck-javascript-eslint-executable eslint))))))
  :config
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; feel a little more at home with neotree
(use-package neotree
  :ensure t
  :config
  (set-face-attribute 'neo-file-link-face nil :foreground "white")
  (set-face-attribute 'neo-dir-link-face nil :foreground "cyan")
  (global-set-key [f8] 'neotree-toggle))

;; paredit
(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

;; hope that there will be a complete config for org-mode someday
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (progn
    ;; config stuff
    ))

;; the good old monokai
(use-package monokai-theme
  :ensure t
  :defer t
  :init
  (load-theme 'monokai t))

(provide 'packages)
;;; packages.el ends here



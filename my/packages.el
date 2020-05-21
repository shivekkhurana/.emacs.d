;;; package --- Bring all the packages

;;; Commentary:

;;; Code:

;; Bootstrap `straight'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;; cider
(use-package cider
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))


;; show ido files vertically
(use-package ido-vertical-mode
  :defer t
  :init (ido-vertical-mode 1))

;; use projectile for better search and project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-directories
        (append '("elpa/") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-directories
        (append '(".shadow-cljs/*") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
  	(append '() ".#*" projectile-globally-ignored-files)))

;; https://www.reddit.com/r/emacs/comments/6xc0im/ivy_counsel_swiper_company_helm_smex_and_evil/
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "%d/%d ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

;; let emacs help you
(use-package which-key
  :defer t
  :init (which-key-mode))

;; color code parenthesis
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :init (global-flycheck-mode))

;; First install the package, then install the checker as soon as `clojure-mode' is loaded
(use-package flycheck-clj-kondo)

;; make sure the indentation in fine with clojure and clojurescript
(use-package clojure-mode
  :defer t
  :config
  (require 'flycheck-clj-kondo))

;; load .graphql files gracefully
(use-package graphql-mode
  :defer t
  :ensure t)

;; feel a little more at home with neotree
(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle))

;; complete anything mode
(use-package company
  :defer t
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

(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  ;;(setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

;; dracula theme
(use-package dracula-theme)

;; Highlight current line
(defvar hl-line-face)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1a1b22")
(set-face-foreground 'highlight nil)

;; joining the evil side
(use-package evil
  :straight t
  :demand t
  :config
  (evil-mode 1))

;; eval without going to the very end
(use-package evil-adjust
  :straight (evil-adjust :type git :host github :repo "troyp/evil-adjust"))

(use-package darkroom
  :straight (darkroom :type git :host github :repo "joaotavora/darkroom")
  :config
  (global-set-key [f1] 'darkroom-mode))

;; beautiful powerline
(use-package powerline-evil
  :straight (powerline-evil :type git :host github :repo "johnson-christopher/powerline-evil")
  :init
  (powerline-evil-center-color-theme))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package yaml-mode
  :ensure t)

;;; packages.el ends here

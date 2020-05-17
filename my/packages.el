;;; package --- Bring all the packages

;;; Commentary:

;;; Code:

;; Bootstrap `straight'
;; The plan is to eventually get rid of use-package
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

;; fuzzy search like sublime using flx-ido
(use-package flx-ido
  :defer t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil))
  :config
   (flx-ido-mode 1))

;; use projectile for better search and project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-directories
        (append '("elpa/") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-directories
        (append '(".shadow-cljs/*") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
  	(append '() ".#*" projectile-globally-ignored-files)))

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

;; remove personal dependance on sublime to work on js projects
(use-package js2-mode
  :init
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))

  (add-to-list 'auto-mode-alist '("\\.[j|t]s$" . js2-mode)))

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

;; joining the evil side
(use-package evil
  :straight t
  :demand t
  :config
  (evil-mode 1))


;;; packages.el ends here

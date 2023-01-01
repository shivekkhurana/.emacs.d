;; package --- Bring all the packages

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

;;  Effectively replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;; LSP superpowers
(use-package lsp-mode
  :commands lsp
  :straight t
  :defer 1)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :ensure t
  :defer 1
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package company-lsp
  :straight t
  :defer 1)

;; cider
(use-package cider
  :defer t
  :straight t
  :hook ((before-save . cider-format-buffer))
  :hook ((clojure-mode . cider-mode))
  :hook ((clojurescript-mode . cider-mode))
  :hook ((cider-repl-mode . cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))

;; show ido files vertically
(use-package ido-vertical-mode
  :defer 1
  :straight t
  :init (ido-vertical-mode 1))

;; use projectile for better search and project management
(use-package projectile
  :defer 1
  :straight t
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
  :straight t
  :defer 1
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "%d/%d ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :straight t
  :defer 1)

(use-package swiper
  :ensure t
  :straight t
  :defer 1
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c j") 'counsel-git-grep))

;; let emacs help you
(use-package which-key
  :straight t
  :defer 5
  :init (which-key-mode))

;; color code parenthesis
(use-package rainbow-delimiters
  :straight t
  :defer 5
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; lint all the things boy
(use-package flycheck
  :defer 5
  :straight t
  :init (add-hook 'prog-mode-hook #'flycheck-mode))

;; First install the package, then install the checker as soon as `clojure-mode' is loaded
(use-package flycheck-clj-kondo
  :straight t
  :defer 5)

;; make sure the indentation in fine with clojure and clojurescript
(use-package clojure-mode
  :straight t
  :defer 5
  :hook ((clojure-mode . lsp-mode))
  :hook ((clojurescript-mode . lsp-mode))
  :config
  (require 'flycheck-clj-kondo))

(use-package clj-refactor
  :ensure t
  :straight t
  :defer 5
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

;; status app specific lint config
(defun hs-clojure-mode-hook ()
  (interactive)
  (put-clojure-indent 'letsubs 1)
  (put-clojure-indent 'create-class 0)
  (put-clojure-indent 'register-handler-db -1)
  (put-clojure-indent 'create-class -1)
  (put-clojure-indent 'register-handler-fx -1)
  (put-clojure-indent 'register-handler -1)
  (put-clojure-indent 'reg-fx -1)
  (put-clojure-indent 'reg-cofx -1)
  (put-clojure-indent 'reg-sub -1)
  (put-clojure-indent 'allowed-keys -1)
  (put-clojure-indent 'start 0)
  (put-clojure-indent 'list-item 0)
  (put-clojure-indent 'setTimeout 0)
  (put-clojure-indent 'set-timeout 0)
  (put-clojure-indent 'run-test-sync 0)
  (put-clojure-indent 'keep 0)
  (put-clojure-indent 'status/move-to-internal-storage 0)
  (put-clojure-indent 'status/should-move-to-internal-storage? 0)
  (put-clojure-indent 'utils/show-popup 0)
  (put-clojure-indent '.watchPosition 0)
  (put-clojure-indent '.clearWatch 0)
  (put-clojure-indent 'ra/start-figwheel! 0)
  (put-clojure-indent '.getCurrentPosition 0)
  (put-clojure-indent 'crypt/gen-random-bytes 0)
  (put-clojure-indent 'assoc 0)
  (put-clojure-indent 'figwheel/watch-and-reload 0)
  (put-clojure-indent 'leval/eval-in-project 0)
  (hs-minor-mode 1))

(add-hook 'clojure-mode-hook 'hs-clojure-mode-hook)

;; load .graphql files gracefully
(use-package graphql-mode
  :defer t
  :straight t
  :ensure t)

 ;; feel a little more at home with neotree
(use-package neotree
  :defer 5
  :ensure t
  :straight t
  :config
  (global-set-key [f8] 'neotree-toggle))

;; complete anything mode
(use-package company
  :defer 5
  :straight t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (setq company-begin-commands
        '(self-insert-command
	  org-self-insert-command
	  orgtbl-self-insert-command
	  c-scope-operator
	  c-electric-colon
	  c-electric-lt-gt
	  c-electric-slash
	  cljr-slash)))

;; The almighty paredit
(use-package paredit
  :defer t
  :straight t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

;; (use-package emmet-mode
;;   :commands emmet-mode
;;   :defer t
;;   :init
;;   (setq emmet-indentation 2)
;;   ;;(setq emmet-move-cursor-between-quotes t)
;;   :config
;;   (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;;   (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;;   )

;; monokai theme
(use-package monokai-theme
  :ensure t
  :straight t)

;; joining the evil side
(use-package evil
  :defer 1
  :straight t
  :config
  (evil-mode t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil))

;; ;; Writing Mode
;; (use-package darkroom
;;   :straight (darkroom :type git :host github :repo "joaotavora/darkroom")
;;   :config
;;   (global-set-key [f1] 'darkroom-mode))

;; beautiful powerline
(use-package powerline-evil
  :defer 2
  :straight (powerline-evil :type git :host github :repo "johnson-christopher/powerline-evil")
  :init
  (powerline-evil-center-color-theme))

;; magit
(use-package magit
  :straight t
  :ensure t
  :defer 10
  :bind (("C-x g" . magit-status)))

;; you can't escape from yaml
(use-package yaml-mode
  :defer 20
  :straight t
  :ensure t)

;; This causes errors on startup
;; ;; make plain markdown pretty
;; (use-package markdown-mode
;;   :ensure t
;;   :defer 20
;;   :straight (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
;;   ;; <TODO> This throws some sequencep errors
;;   ;; :mode
;;   ;; (("README\\.md\\'" . gfm-mode)
;;   ;; ("\\.md\\'" . markdown-mode)
;;   ;; ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown")
;;   :config
;;   (add-hook 'markdown-mode-hook 'turn-on-flyspell))

;; kill typos
;; tell emacs to use aspell instead of ispell
;; https://stackoverflow.com/questions/17126951/emacs-cannot-find-flyspell-ispell
(defvar ispell-program-name)
(setq ispell-program-name "/usr/local/bin/aspell")

(use-package flyspell-correct
  :after flyspell
  :straight t
  :defer 20
  :bind (("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;; Org Mode
;;; Setup to make org mode experience more pleasant
;;; There is a lot more juice here https://github.com/jezcope/dotfiles/blob/master/emacs.d/init-org.org
(defvar org-startup-indented)
(defvar org-startup-folded)
(defvar org-tags-column)
(setq org-startup-indented t ;; this helps with indented headline
      org-startup-folded 'content
      org-tags-column 0)

(add-hook 'org-mode-hook 'visual-line-mode) ;; this trys to intelligently wrap line at word
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; turn on flyspell in org
;; Activation
;; https://orgmode.org/manual/Activation.html#Activation

;; I don't really understand org-store-link
;; global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Agenda
(defvar org-agenda-files)
(setq org-agenda-files (list "~/Wip/time-machine"
                             ;;"~/org/school.org"
))


(use-package org-journal
  :ensure t
  :straight t
  :defer 5
  :bind (("C-c n j" . org-journal-new-entry))
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Wip/time-machine")
  (org-journal-find-file 'find-file)
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-bullets
  :ensure t
  :straight t
  :defer 5
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '("⌁")))

;; Typescript
(use-package typescript-mode
  :straight t
  :ensure t
  :defer 5)

(use-package tide
  :ensure t
  :straight t
  :defer 5
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; emoji
(defvar emojify-company-tooltips-p)
(use-package emojify
  :ensure t
  :straight t
  :defer 3
  :config
  (setq emojify-company-tooltips-p t)
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Keep load time in check
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :commands (esup))

;; Golang setup
(use-package go-mode
  :ensure t
  :defer 5
  :straight t)

(use-package gotest
  :ensure t
  :defer 5
  :straight t
  :config
  (define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
  (define-key go-mode-map (kbd "C-x x") 'go-run))

;; Solidity setup
(use-package solidity-mode
  :ensure t
  :straight t
  :defer t
  :config
  (add-hook 'solidity-mode-hook
	(lambda ()
	(set (make-local-variable 'company-backends)
		(append '((company-solidity company-capf company-dabbrev-code))
			company-backends))))

  (setq solidity-solc-path "/usr/local/bin/solcjs")
  (setq solidity-solium-path "/usr/local/bin/solium")
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-solium-checker-active t)
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :straight t
  :defer 20
  :ensure t)

(use-package company-solidity
  :straight t
  :defer 20
  :ensure t)
;; Rust

(use-package rust-mode
  :straight t
  :defer t
  :hook ((rust-mode . lsp)))

;; cargo-mode for all the cargo related operations
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :straight t
  :defer 20
  :hook ((rust-mode . cargo-minor-mode))
  :bind (("C-c C-c C-n" . cargo-process-new)))

(use-package flycheck-rust
  :straight t
  :defer t
  :hook ((flycheck-mode-hook . flycheck-rust-setup)))

;; JS /React
;; web-mode
(use-package web-mode
  :straight t
  :defer 10
  :ensure t
  :mode
  ("\\.ejs\\'"
   "\\.hbs\\'"
   "\\.html\\'"
   "\\.php\\'"
   "\\.[jt]sx?\\'"
   "\\.css?\\'")
  :hook ((web-mode . company-mode))
  :hook ((web-mode . lsp-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t))

;; prettier.el for linting web files
(use-package prettier
  :straight t
  :defer 10
  :ensure t
  :config
  (put 'prettier-mode 'safe-local-variable #'fixnump)
  :hook ((web-mode . prettier-mode)))

;; yasnippet
(use-package yasnippet
  :ensure t
  :straight t
  :defer 5)

(use-package emmet-mode
  :ensure t
  :straight t
  :defer 5
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package react-snippets
  :straight t
  :defer 10
  :ensure t)

;; project specific .editorconfig
(use-package editorconfig
  :straight t
  :ensure t
  :defer 10
  :config
  (editorconfig-mode 1))

(use-package json-mode
  :straight t
  :ensure t
  :defer 10)

(provide 'packages)

;;; packages.el ends here


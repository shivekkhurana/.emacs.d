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

;; cider
(use-package cider
  :defer t
  :straight t
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
  :defer 5
  :straight t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :straight t
  :defer 5
  :init (global-flycheck-mode))

;; First install the package, then install the checker as soon as `clojure-mode' is loaded
(use-package flycheck-clj-kondo
  :straight t
  :defer 5)

;; make sure the indentation in fine with clojure and clojurescript
(use-package clojure-mode
  :straight t
  :defer 5
  :config
  (require 'flycheck-clj-kondo))

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

;; ;; load .graphql files gracefully
;; (use-package graphql-mode
;;   :defer t
;;   :ensure t)

 ;; feel a little more at home with neotree
(use-package neotree
  :defer t
  :straight t
  :config
  (global-set-key [f8] 'neotree-toggle))

;; complete anything mode
(use-package company
  :defer 5
  :straight t
  :init
  (global-company-mode)
  (setq company-begin-commands
        '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash cljr-slash)))

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

;; ;; you can't escape from yaml
;; (use-package yaml-mode
;;   :defer t
;;   :ensure t)

;; ;; make plain markdown pretty
;; (use-package markdown-mode
;;   :straight (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown")
;;   :config
;;   (add-hook 'markdown-mode-hook 'turn-on-flyspell))

;; ;; kill typos

;; ;; tell emacs to use aspell instead of ispell
;; ;; https://stackoverflow.com/questions/17126951/emacs-cannot-find-flyspell-ispell
;; (defvar ispell-program-name)
;; (setq ispell-program-name "/usr/local/bin/aspell")

;; (defvar flyspell-mode-map)
;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct)

;; ;; Org Mode
;; ;;; Setup to make org mode experience more pleasant
;; ;;; There is a lot more juice here https://github.com/jezcope/dotfiles/blob/master/emacs.d/init-org.org
;; (defvar org-startup-indented)
;; (defvar org-startup-folded)
;; (defvar org-tags-column)
;; (setq org-startup-indented t ;; this helps with indented headline
;;       org-startup-folded 'content
;;       org-tags-column 0)

;; (add-hook 'org-mode-hook 'visual-line-mode) ;; this trys to intelligently wrap line at word
;; (add-hook 'org-mode-hook 'turn-on-flyspell) ;; turn on flyspell in org
;; ;; Activation
;; ;; https://orgmode.org/manual/Activation.html#Activation

;; ;; I don't really understand org-store-link
;; ;; global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)

;; ;; Agenda
;; (defvar org-agenda-files)
;; (setq org-agenda-files (list "~/WIP/TimeMachine"
;;                              ;;"~/org/school.org"
;; ))


;; (use-package org-journal
;;   :bind
;;   ("C-c n j" . org-journal-new-entry)
;;   :custom
;;   (org-journal-date-prefix "#+TITLE: ")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-dir "~/WIP/TimeMachine/")
;;   (org-journal-find-file 'find-file)
;;   (org-journal-date-format "%A, %d %B %Y"))

;; (use-package org-bullets
;;   :commands org-bullets-mode
;;   :init
;;   (add-hook 'org-mode-hook 'org-bullets-mode)
;;   (setq org-bullets-bullet-list '("⌁")))

;; ;; Typescript
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; (use-package tide
;;   :config
;;   (add-hook 'before-save-hook 'tide-format-before-save)
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;; 	    (lambda ()
;; 	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
;; 		(setup-tide-mode))))
;;   (flycheck-add-mode 'typescript-tslint 'web-mode))


;; emoji
(defvar emojify-company-tooltips-p)
(use-package emojify
  :defer 3
  :config
  (setq emojify-company-tooltips-p t)
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Aggresive Indent
;; This is too aggressive :sweat-smile:
;; (use-package aggressive-indent
;;   :ensure t
;;   :config
;;   (global-aggressive-indent-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

;; Keep load time in check
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :commands (esup))

(provide 'packages)

;;; packages.el ends here

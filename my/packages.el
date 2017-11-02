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

;; Cider
(use-package cider
  :ensure t
  :pin melpa-stable)

;; try
(use-package try
  :ensure t)

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :commands (mc/add-cursor-on-click
	     mc/edit-beginning-of-lines
	     mc/edit-lines
	     mc/insert-numbers
	     mc/mark-all-dwim
	     mc/mark-all-in-region-regexp
	     mc/mark-all-like-this
	     mc/mark-next-like-this
	     mc/mark-previous-like-this
	     mc/mark-sgml-tag-pair
	     mc/reverse-regions
	     mc/skip-to-next-like-this
	     mc/skip-to-previous-like-this
	     mc/sort-regions
	     mc/unmark-next-like-this
	     mc/unmark-previous-like-this)
  )

;; Remove startup screen
(setq inhibit-startup-message t)

;; Remove scroll bar, menu bar and tool bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setup ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Keep customization mess out of init.el
(setq custom-file (concat user-emacs-directory "_customize.el"))
(load custom-file t)

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; package --- internals.el

;;; Commentary:
;;; Remove startup screen

;;; Code:
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

;; Turn off that annoying bell
(setq ring-bell-function 'ignore)

;; Make fonts prettier
;; height config
;; 128 for code
;; 200 for presentations
;; 240 for screencasts
(set-face-attribute 'default nil :font "Monaco" :height 128)

;; Show trailing spaces
(setq-default show-trailing-whitespace t)

;; Add local bins to exec-path
(add-to-list 'exec-path "/usr/local/bin")

;; Show line numbers
(global-display-line-numbers-mode)


;; Default javascript modes overwrides
;; https://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode
(setq js-indent-level 2)

;; need this setting for emacs 27
;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27
(setq custom--inhibit-theme-enable nil)

;; Highlight current line
(defvar hl-line-face)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1a1b22")
(set-face-foreground 'highlight nil)


(provide 'internals)

;;; internals.el ends here

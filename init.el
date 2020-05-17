;;; package --- Init Emacs
;;; Commentary:
;;; Author - Shivek Khurana
;;; Emacs is taking over my life

;;; Code:
;; Let the show begin
(package-initialize)

(load "~/.emacs.d/my/internals.el")
(load "~/.emacs.d/my/packages.el")
(load "~/.emacs.d/my/org.el")

;; https://github.com/joaotavora/darkroom/blob/master/darkroom.el
(load "~/.emacs.d/my/darkroom.el")
(global-set-key [f1] 'darkroom-mode)


;;; init.el ends here


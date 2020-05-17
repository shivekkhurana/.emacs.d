;;; package --- A humble attempt to tame the org-mode dragon

;;; Commentary:
;;; Setup to make org mode experience more pleasant
;;; There is a lot more juice here https://github.com/jezcope/dotfiles/blob/master/emacs.d/init-org.org

;;; Code:
(defvar org-startup-indented)
(defvar org-startup-folded)
(defvar org-tags-column)
(setq org-startup-indented t ;; this helps with indented headline
      org-startup-folded 'content
      org-tags-column 0)

(add-hook 'org-mode-hook 'turn-off-filladapt-mode)
(add-hook 'org-mode-hook 'visual-line-mode) ;; this trys to intelligently wrap line at word


;; Activation
;; https://orgmode.org/manual/Activation.html#Activation

;; I don't really understand org-store-link
;; (global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Agenda
(defvar org-agenda-files)
(setq org-agenda-files (list "~/WIP/TimeMachine"
                             ;;"~/org/school.org"
))


(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/WIP/TimeMachine/")
  (org-journal-find-file 'find-file)
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-bullets
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '("⌁")))


;; real bullets for list elements
;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; variable width font
(let* ((variable-tuple (cond ((x-list-fonts "ETBembo") '(:font "ETBembo"))
                             (nil (warn "Cannot find a Sans Serif Font."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          ;; `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.04))))
                          ;; `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.12))))
                          ;; `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.20))))
                          ;; `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.28))))
                          ;; `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.36))))
                          ;; `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.44))))
                          ;; `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.52))))
                          ;; `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.6))))
                          ;; `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))
			  ))

;;; org.el ends here

;;; package --- Org Mode Juice

;;; Commentary:
;;; Everything to mold org and org-roam to my taste

;;; Code:

;; Export backlinks
(defun my/org-roam--backlinks-list-with-content (file)
  (with-temp-buffer
    (if-let* ((backlinks (org-roam--get-backlinks file))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          (insert (format "\n\n* %d Backlinks\n"
                          (length backlinks)))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** [[file:%s][%s]]\n"
                              file-from
                              (org-roam--get-title-or-slug file-from)))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                  (insert "\n\n")))))))
    (buffer-string)))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;; Change fonts for writing
;; https://www.emacswiki.org/emacs/FacesPerBuffer#toc3
(defvar buffer-face-mode-face)
(defun org-buffer-face-mode-variable ()
   "Set font to a variable width (proportional) fonts in current buffer."
   (interactive)
   (setq buffer-face-mode-face '(:family "Helvetica" :height 200 :width semi-condensed))
   (buffer-face-mode))

;; (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)

;;; org.el ends here

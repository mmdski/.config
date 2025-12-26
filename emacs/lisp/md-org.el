;;; md-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; org - Outline-based notes management and organizer
(use-package
 org
 :ensure nil

 :custom
 (org-return-follows-link t)
 (org-mouse-1-follows-link t)
 (org-link-descriptive t)
 (org-hide-emphasis-markers t)
 (org-log-done t)
 (org-list-allow-alphabetical t)
 (org-html-inline-images t)
 (org-latex-create-formula-image-program 'dvisvgm)
 (org-file-apps '((auto-mode . emacs) ("\\.pdf\\'" . emacs)))
 (setq org-latex-packages-alist '(("" "mathtools" t)))

 :config
 (add-to-list
  'org-file-apps
  '("\\.html\\'" . (lambda (path) (browse-url-default-browser path))))
 (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

 :hook
 (org-mode . org-indent-mode)
 (org-mode . flyspell-mode)
 (org-mode . visual-line-mode)
 (org-after-todo-statistics . org-summary-todo))

;;; org-appear - Auto-toggle Org elements.
(use-package org-appear :hook (org-mode . org-appear-mode))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise.  N-DONE N-NOT-DONE."
  (let (org-log-done
        org-todo-log-states) ; turn off logging
    (org-todo
     (if (= n-not-done 0)
         "DONE"
       "TODO"))))

(with-eval-after-load 'org
  (setq
   org-cite-global-bibliography
   (if (and (boundp 'citar-bibliography) citar-bibliography)
       citar-bibliography
     (list (expand-file-name "~/Documents/notes/references.bib")))
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar)

  (define-key org-mode-map (kbd "C-c b") #'org-cite-insert))

;;; _
(provide 'md-org)
;;; md-org.el ends here

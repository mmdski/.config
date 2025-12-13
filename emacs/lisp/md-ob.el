;;; md-ob.el --- Org Babel configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun md-org-confirm-babel-evaluate (lang body)
  "Return non-nil to require confirmation before evaluating an Org Babel block.

LANG is the source block language as a string.
BODY is the source block contents.

This function disables confirmation for a small, trusted set of
languages (e.g. Emacs Lisp, Julia, Scheme, Racket, Python), while
requiring confirmation for all others.  It is intended for use as
the value of `org-confirm-babel-evaluate`."
  (not
   (member
    lang
    '("elisp"
      "julia"
      "scheme"
      "racket"
      "gnuplot"
      "jupyter-julia"
      "python"))))

(use-package
 org
 :ensure nil
 :custom (org-confirm-babel-evaluate #'md-org-confirm-babel-evaluate)
 :config
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((julia . t)
    (scheme . t)
    (gnuplot . t)
    (python . t)
    (shell . t)
    (jupyter . t)
    (racket . t))))

(use-package
 ob-racket
 :ensure nil
 :after org
 :custom (org-babel-racket-command "racket"))

(provide 'md-ob)
;;; md-ob.el ends here

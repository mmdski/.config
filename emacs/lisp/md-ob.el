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
    '("elisp" "julia" "scheme" "racket" "gnuplot" "jupyter-julia" "python"))))

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

(use-package
 ob-julia-vterm
 :after org
 :config (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
 (defalias
   'org-babel-variable-assignments:julia
   'org-babel-variable-assignments:julia-vterm)
 (with-eval-after-load 'org
   (add-to-list 'org-babel-load-languages '(julia-vterm . t))
   (org-babel-do-load-languages
    'org-babel-load-languages org-babel-load-languages)))

(provide 'md-ob)
;;; md-ob.el ends here

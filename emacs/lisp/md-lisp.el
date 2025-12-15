;;; md-lisp.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; eldoc - Show function arglist or variable docstring in echo area
(use-package eldoc :ensure nil)

;; paredit - minor mode for editing parentheses
;; activation is per-language
(use-package paredit)

;; aggressive-indent — Minor mode to aggressively keep code indented
;; activation is per-language
(use-package aggressive-indent)

;;; Emacs Lisp

;; package-lint - A linting library for elisp package authors
(use-package package-lint)

;; package-lint-flymake - A package-lint Flymake backend.
(use-package
 package-lint-flymake
 :after package-lint
 :commands package-lint-flymake-setup
 :hook (emacs-lisp-mode . package-lint-flymake-setup))

;; elisp-autofmt Emacs lisp auto-format.
(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :custom
 (elisp-autofmt-style 'native)
 (elisp-autofmt-on-save-p 'always))

;; set universal hooks for emacs-lisp-mode
(use-package
 emacs-lisp-mode
 :ensure nil
 :hook (emacs-lisp-mode . enable-paredit-mode))

;; lisp interaction mode
(use-package
 lisp-mode
 :ensure nil
 :hook
 (lisp-interaction-mode
  .
  (lambda ()
    (when (bound-and-true-p paredit-mode)
      (local-set-key (kbd "C-j") #'eval-print-last-sexp)))))

;;; Scheme/Racket

(defun md-indent-scheme-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package
 scheme
 :ensure nil
 :custom (scheme-program-name "racket")
 :hook
 ((scheme-mode . enable-paredit-mode)
  (scheme-mode . aggressive-indent-mode)))

;; geiser
(use-package
 geiser
 :custom
 (geiser-active-implementations '(racket))
 (geiser-default-implementation 'racket))

(use-package geiser-racket :after geiser)

;;; _
(provide 'md-lisp)
;;; md-lisp.el ends here

;;; md-writing.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package text-mode :ensure nil :hook (text-mode . flyspell-mode))

;; set the Ispell personal dictionary location
(setq ispell-personal-dictionary
      (expand-file-name "aspell/.aspell.en.pws" user-emacs-directory))

(use-package
 markdown-mode
 :ensure t
 :mode
 (("README\\.md\\'" . gfm-mode) ("\\.md\\'" . markdown-mode))
 :custom (markdown-command "pandoc")
 :bind (:map markdown-mode-map ("C-c C-e" . markdown-do))
 :hook
 ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode)))

(use-package
 pandoc-mode
 :hook ((markdown-mode . pandoc-mode) (gfm-mode . pandoc-mode))
 :custom
 (pandoc-binary "pandoc")
 (pandoc-data-dir (expand-file-name "~/.pandoc"))
 (pandoc-default-format "markdown")
 (pandoc-default-output-format "html"))

;;; _
(provide 'md-writing)
;;; md-writing.el ends here

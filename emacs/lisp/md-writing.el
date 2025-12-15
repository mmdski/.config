;;; md-writing.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package text-mode :ensure nil :hook (text-mode . flyspell-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq delete-trailing-lines t)
(setq require-final-newline t)
(setq-default fill-column 80)

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

;;; denote - Simple notes with an efficient file-naming scheme
(use-package
 denote
 :ensure t
 :hook (dired-mode . denote-dired-mode)
 :bind
 (("C-c n n" . denote)
  ("C-c n r" . denote-rename-file)
  ("C-c n l" . denote-link)
  ("C-c n b" . denote-backlinks)
  ("C-c n d" . denote-dired)
  ("C-c n g" . denote-grep))
 :config (setq denote-directory (expand-file-name "~/Documents/notes/"))

 ;; Automatically rename Denote buffers when opening them so that
 ;; instead of their long file name they have, for example, a literal
 ;; "[D]" followed by the file's title.  Read the doc string of
 ;; `denote-rename-buffer-format' for how to modify this.
 (denote-rename-buffer-mode 1))

(use-package
 citar
 :custom
 (citar-bibliography
  (list (expand-file-name "~/Documents/notes/references.bib"))))

;;; _
(provide 'md-writing)
;;; md-writing.el ends here

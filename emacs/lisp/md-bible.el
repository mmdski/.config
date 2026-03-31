;;; md-bible.el --- Org Babel configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package
 bible-mode
 :ensure nil
 :init
 (setq
  bible-mode-default-module "CPDV"
  bible-mode-word-study-enabled t)
 :config (setenv "SWORD_PATH" (expand-file-name "~/.sword")))

(provide 'md-bible)
;;; md-bible.el ends here

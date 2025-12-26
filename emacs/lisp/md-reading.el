;;; md-reading.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package
 pdf-tools
 :mode ("\\.pdf\\'" . pdf-view-mode)
 :custom (doc-view-resolution 300)
 :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
 :config (pdf-tools-install))

(use-package
 nov
 :mode ("\\.epub\\'" . nov-mode)
 :hook (nov-mode . visual-line-mode))

(use-package
 elfeed
 :bind (("C-x w" . elfeed))
 :config
 (setq elfeed-feeds
       '("http://nullprogram.com/feed/"
         "https://protesilaos.com/codelog.xml"
         "https://lee-phillips.org/updates.xml"
         "https://viralinstruction.com/feed.xml"
         "https://www.masteringemacs.org/feed"
         "https://brainbaking.com/index.xml")))

;;; _
(provide 'md-reading)
;;; md-reading.el ends here

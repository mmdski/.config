;;; early-init.el --- Crafted Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load
 (expand-file-name "crafted-emacs/modules/crafted-early-init-config"
                   user-emacs-directory))

(defvar md-big-frame-alist
  '((width . 175) (height . 46) (left . 10) (top . 30)))
(defvar md-small-frame-alist
  '((width . 85) (height . 25) (left . 10) (top . 30)))
(defvar md-narrow-frame-alist
  '((width . 85) (height . 35) (left . 10) (top . 30)))
(defvar md-half-frame-alist
  '((width . 88) (height . 46) (left . 10) (top . 30)))

(setq default-frame-alist md-small-frame-alist)

(setq initial-frame-alist default-frame-alist)

;;; _
(provide 'early-init)
;;; early-init.el ends here

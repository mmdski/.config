;;; md-ui.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load-theme 'modus-operandi-deuteranopia)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; Modes: call the function
(blink-cursor-mode -1)
(display-time-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package
 dired
 :ensure nil
 :hook (dired-mode . dired-hide-details-mode))

;; Global variables: set with setq
(setq
 ring-bell-function #'ignore
 inhibit-splash-screen t
 column-number-mode t
 use-dialog-box nil)

;; Buffer defaults: set with setq-default
(setq-default
 line-spacing 2
 truncate-lines t)

;; turn off the menu bar when on linux
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))

;; show line numbers
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook
   hook
   (lambda ()
     (setq display-line-numbers-type 'relative)
     (display-line-numbers-mode t))))

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command
         '(scroll-up-command
           scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))


;;; use-package - A better *help* buffer.
(use-package
 helpful
 :bind
 (("<remap> <describe-command>" . helpful-command)
  ("<remap> <describe-function>" . helpful-callable)
  ("<remap> <describe-key>" . helpful-key)
  ("<remap> <describe-symbol>" . helpful-symbol)
  ("<remap> <describe-variable>" . helpful-variable)
  ("C-h F" . helpful-function)
  ("C-h K" . describe-keymap))
 :bind
 (:map helpful-mode-map ("<remap> <revert-buffer>" . helpful-update)))

;;; elisp-demos - Elisp API Demos.
(use-package
 elisp-demos
 :after helpful
 :config
 (advice-add
  'helpful-update
  :after #'elisp-demos-advice-helpful-update))

;;; all-the-icons - A library for inserting Developer icons.
(use-package all-the-icons)

;;; breadcrumb
(use-package breadcrumb :init (breadcrumb-mode 1))

;;; _
(provide 'md-ui)
;;; md-ui.el ends here

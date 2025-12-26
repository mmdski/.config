;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary: Heavily inspired by Crafted Emacs

;;; Code:

;;;; 0. Minimal early hygiene

;; Suppress warnings from third-party packages lacking lexical-binding cookies
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;; Keep Custom out of init.el
(setq custom-file (expand-file-name "md-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

;;;; 1. Load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; 2. Package system
(require 'package)

(add-to-list
 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities
      '(("gnu" . 99) ("nongnu" . 80) ("stable" . 70) ("melpa" . 0)))

(package-initialize)

;; use-package defaults
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;; 3. Global behavior defaults

(global-auto-revert-mode 1)
(delete-selection-mode)
(setq large-file-warning-threshold 100000000)
(setq switch-to-buffer-obey-display-actions t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;;; 4. Keybindings / display rules

;; define a key to define the word at point.
(keymap-set global-map "M-#" #'dictionary-lookup-definition)

;; Show dictionary definition on the left
(add-to-list
 'display-buffer-alist
 '("^\\*Dictionary\\*"
   (display-buffer-in-side-window)
   (side . left)
   (window-width . 70)))

;;;; 5. Frame helpers

;;; Sets frame size
(defun md-set-frame-size (frame-alist)
  "Apply width and height from FRAME-ALIST."
  (let ((params
         `((width . ,(cdr (assq 'width frame-alist)))
           (height . ,(cdr (assq 'height frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

;; these are initialized in early-init.el
(defvar md-small-frame-alist)
(defvar md-big-frame-alist)
(defvar md-narrow-frame-alist)
(defvar md-half-frame-alist)

(defun md-sf ()
  "Apply small frame settings."
  (interactive)
  (md-set-frame-size md-small-frame-alist))
(defun md-bf ()
  "Apply big frame settings."
  (interactive)
  (md-set-frame-size md-big-frame-alist))
(defun md-nf ()
  "Apply narrow frame settings."
  (interactive)
  (md-set-frame-size md-narrow-frame-alist))
(defun md-hf ()
  "Apply half frame settings."
  (interactive)
  (md-set-frame-size md-half-frame-alist))
(defun md-rp ()
  "Reset the position of the frame."
  (interactive)
  (let ((params
         `((left . ,(cdr (assq 'left md-big-frame-alist)))
           (top . ,(cdr (assq 'top md-big-frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

;;;; 6. Environment / PATH / exec-path

;;; Set up path
(defvar md-env-path-sep ":"
  "Separator for entries in the PATH environment variable.")

(defun md-env-path-prepend (path-to-prepend)
  "Prepend PATH-TO-PREPEND to the PATH environment variable present.

This ensures the given directory takes precedence when resolving executables."
  (let*
      ((env-path (getenv "PATH"))
       (paths (split-string env-path md-env-path-sep))
       (normalized (directory-file-name path-to-prepend))) ;; remove trailing slash
    (unless (member normalized paths)
      (setenv "PATH" (concat path-to-prepend md-env-path-sep env-path)))))

(when (not (eq system-type 'windows-nt))
  (md-env-path-prepend (expand-file-name "~/.local/bin")))

(require 'info)

;; macOS specific settings
(when (eq system-type 'darwin) ; macOS
  ;  (setq mac-command-modifier 'meta) ; Command key is Meta
  ;  (setq mac-option-modifier 'super) ; Option key is Super
  (global-set-key (kbd "s-q") #'delete-frame)
  (global-set-key (kbd "s-w") #'kill-buffer-and-window)
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info")
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
  (add-to-list 'Info-directory-list (expand-file-name "~/.local/share/info"))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/opt/make/libexec/gnubin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (add-to-list 'exec-path (expand-file-name "~/.juliaup/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (md-env-path-prepend "/usr/local/bin")
  (md-env-path-prepend "/opt/homebrew/bin")
  (md-env-path-prepend "/opt/homebrew/opt/make/libexec/gnubin")
  (md-env-path-prepend "/opt/homebrew/opt/llvm/bin")
  (md-env-path-prepend "/Library/TeX/texbin")
  (md-env-path-prepend (expand-file-name "~/.juliaup/bin"))
  (md-env-path-prepend (expand-file-name "~/.cargo/bin")))

;;;; 7. Load feature modules

;;; UI settings
(require 'md-ui)

;;; Completion
(require 'md-completion)

;;; IDE
(require 'md-ide)

;;; Lisp/Scheme
(require 'md-lisp)

;;; org
(require 'md-org)

;;; org-babel
(require 'md-ob)

;;; writing
(require 'md-writing)

;;; reading
(require 'md-reading)

;;; _
(provide 'init)
;;; init.el ends here

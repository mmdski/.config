;;; init.el --- Crafted Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; begin crafted Emacs setup

;;; Initial phase

;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load
 (expand-file-name "crafted-emacs/modules/crafted-init-config"
                   user-emacs-directory))

;;; Packages phase
;; Collect list of packages to install.

;; add crafted-ui package definitions to selected packages list
(require 'crafted-ui-packages)

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(require 'crafted-completion-packages)

;; add crafted-ide package definitions to selected packages list
(require 'crafted-ide-packages)
(add-to-list 'package-selected-packages 'magit) ; considered "ide stuff"
(add-to-list 'package-selected-packages 'julia-mode)
(add-to-list 'package-selected-packages 'eglot-jl)
(add-to-list 'package-selected-packages 'julia-formatter)

;; add crafted-lisp package definitions to selected packages list
;; currently not working for mit-scheme
;; (require 'crafted-lisp-packages)
;; (add-to-list 'package-selected-packages 'geiser-mit)
(add-to-list 'package-selected-packages 'elisp-autofmt)

;; add crafted-org package definitions to selected packages list
(require 'crafted-org-packages)

;; add crafted-writing package definitions to selected packages list
(require 'crafted-writing-packages)
(add-to-list 'package-selected-packages 'pdf-tools)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase

(setq ispell-personal-dictionary (expand-file-name "aspell/.aspell.en.pws"))

;; Load crafted-defaults configuration
(require 'crafted-defaults-config)

;; Load crafted-ui configuration
(require 'crafted-ui-config)

;; Load configuration for the completion module
(require 'crafted-completion-config)

;; Load crafted-speedbar configuration
(require 'crafted-speedbar-config)

;; Load crafted-ide configuration
(require 'crafted-ide-config)
(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter '(toml python))
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Load crafted-lisp configuration
;; not workign for mit-scheme
;; (require 'crafted-lisp-config)
(add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode)
(add-hook
 'elisp-autofmt-mode-hook
 (lambda () (add-hook 'before-save-hook #'elisp-autofmt-buffer nil 'local)))
(customize-set-variable 'scheme-program-name "scheme")
(require 'xscheme)

;; Load crafted-updates configuration
(require 'crafted-updates-config)

;; Load crafted-org configuration
(require 'crafted-org-config)
(add-hook 'org-mode-hook #'flyspell-mode)

;; Load crafted-writing configuration
(require 'crafted-writing-config)
(add-hook 'text-mode-hook #'flysepll-mode)

;; end crafted emacs setup

(defun md-set-frame-size (frame-alist)
  "Apply width and height from FRAME-ALIST."
  (let ((params
         `((width . ,(cdr (assq 'width frame-alist)))
           (height . ,(cdr (assq 'height frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

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
(defun md-rp ()
  "Reset the position of the frame."
  (interactive)
  (let ((params
         `((left . ,(cdr (assq 'left md-big-frame-alist)))
           (top . ,(cdr (assq 'top md-big-frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

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

;; macOS specific settings
(when (eq system-type 'darwin) ; macOS
  ;  (setq mac-command-modifier 'meta) ; Command key is Meta
  ;  (setq mac-option-modifier 'super) ; Option key is Super
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info")
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
  (add-to-list 'Info-directory-list (expand-file-name "~/.local/share/info"))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/opt/make/libexec/gnubin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (md-env-path-prepend "/usr/local/bin")
  (md-env-path-prepend "/opt/homebrew/bin")
  (md-env-path-prepend "/opt/homebrew/opt/make/libexec/gnubin")
  (md-env-path-prepend "/opt/homebrew/opt/llvm/bin")
  (md-env-path-prepend "/Library/TeX/texbin"))

;; global formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace 'delete-trailing-lines)
(setq require-final-newline t)
(setq-default fill-column 80)

(load-theme 'modus-operandi t)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

(display-time)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq-default
 line-spacing 2
 truncate-lines t
 inhibit-splash-screen t)
(setq ring-bell-function 'ignore)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(setq column-number-mode t)
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook
   hook
   (lambda ()
     (setq display-line-numbers-type 'relative)
     (display-line-numbers-mode t))))

;;; _
(provide 'init)
;;; init.el ends here

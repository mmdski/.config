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

(add-to-list 'package-selected-packages 'nov)

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(require 'crafted-completion-packages)

;; add crafted-ide package definitions to selected packages list
(require 'crafted-ide-packages)
(add-to-list 'package-selected-packages 'magit) ; considered "ide stuff"
(add-to-list 'package-selected-packages 'julia-mode)
(add-to-list 'package-selected-packages 'eglot-jl)
(add-to-list 'package-selected-packages 'julia-repl)

;; add crafted-lisp package definitions to selected packages list
;; (require 'crafted-lisp-packages)
(add-to-list 'package-selected-packages 'elisp-autofmt)
(add-to-list 'package-selected-packages 'paredit)

;; add crafted-org package definitions to selected packages list
(require 'crafted-org-packages)
(add-to-list 'package-selected-packages 'ess)
(add-to-list 'package-selected-packages 'ob-julia)

;; add crafted-writing package definitions to selected packages list
(require 'crafted-writing-packages)
(add-to-list 'package-selected-packages 'pdf-tools)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase

(setq ispell-personal-dictionary
      (expand-file-name "aspell/.aspell.en.pws" user-emacs-directory))

;; Load crafted-defaults configuration
(require 'crafted-defaults-config)

;; Load crafted-ui configuration
(require 'crafted-ui-config)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Load configuration for the completion module
(require 'crafted-completion-config)
(keymap-unset corfu-map "RET")

;; Load crafted-speedbar configuration
(require 'crafted-speedbar-config)

;; Load crafted-ide configuration
(require 'crafted-ide-config)
(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter '(toml python))
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(setq project-mode-line t)
(setq project-kill-buffers-display-buffer-list t)
(add-to-list
 'project-switch-commands '(project-switch-to-buffer "Switch buffer"))

(setq eglot-connect-timeout 300) ;; for eglot-jl

;; Julia
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(setenv "JULIA_EDITOR" "emacsclient -a -r")

;; language server
(add-hook
 'julia-mode-hook
 (lambda ()
   (require 'eglot-jl)
   (eglot-jl-init)
   (eglot-ensure)))

(defun md-julia-format-on-save ()
  "Enable format-on-save in Julia buffers using eglot."
  (when (and (boundp 'eglot--managed-mode) eglot--managed-mode)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

(add-hook
 'julia-mode-hook
 (lambda ()
   (require 'eglot-jl)
   (eglot-jl-init)
   (eglot-ensure)
   (md-julia-format-on-save)))

;; Load crafted-lisp configuration
;; not workign for mit-scheme
;; (require 'crafted-lisp-config)

;; lisp
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(customize-set-variable 'scheme-program-name "scheme")
(require 'xscheme)
(defun md-indent-scheme-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook
 'scheme-mode-hook
 (lambda () (add-hook 'before-save-hook #'md-indent-scheme-buffer nil t)))

;; elisp
(add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook
 'elisp-autofmt-mode-hook
 (lambda () (add-hook 'before-save-hook #'elisp-autofmt-buffer nil 'local)))

;; Load crafted-updates configuration
(require 'crafted-updates-config)

;; Load crafted-org configuration
(require 'crafted-org-config)

(setq org-log-done t)
(setq org-list-allow-alphabetical t)
(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done
        org-todo-log-states) ; turn off logging
    (org-todo
     (if (= n-not-done 0)
         "DONE"
       "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(setq org-file-apps '((auto-mode . emacs) ("\\.pdf\\'" . emacs)))
(add-to-list
 'org-file-apps
 '("\\.html\\'" . (lambda (file path) (browse-url-default-browser path))))

(setq org-html-inline-images t)
(setq org-html-with-latex 'dvipng)

(setq org-latex-create-formula-image-program 'dvisvgm)
(with-eval-after-load 'org
  (setq org-latex-format-options
        (plist-put org-format-latex-options :scale 1.5)))

;; org babel
(org-babel-do-load-languages
 'org-babel-load-languages '((julia . t) (scheme . t)))
(with-eval-after-load 'ob-scheme
  (setq org-babel-scheme-command "guile"))
(defun md-org-confirm-babel-evaluate (lang body)
  (not (member lang '("elisp" "C" "python" "julia" "scheme"))))
(setq org-confirm-babel-evaluate #'md-org-confirm-babel-evaluate)
(setq org-babel-default-header-args:scheme
      '((:results . "value") (:session . "guile") (:exports . "both")))

;; Load crafted-writing configuration
(setq markdown-command "pandoc")
(require 'crafted-writing-config)
(add-hook 'text-mode-hook #'flyspell-mode)

(autoload 'pdf-view-mode "pdf-tools" "Major mode for viewing PDF files." t)
(setq doc-view-resolution 300)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(with-eval-after-load 'pdf-tools
  (pdf-tools-install))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

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
  (add-to-list 'exec-path (expand-file-name "~/.juliaup/bin"))
  (md-env-path-prepend "/usr/local/bin")
  (md-env-path-prepend "/opt/homebrew/bin")
  (md-env-path-prepend "/opt/homebrew/opt/make/libexec/gnubin")
  (md-env-path-prepend "/opt/homebrew/opt/llvm/bin")
  (md-env-path-prepend "/Library/TeX/texbin")
  (md-env-path-prepend (expand-file-name "~/.juliaup/bin")))

;; global formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace 'delete-trailing-lines)
(setq require-final-newline t)
(setq-default fill-column 80)

(load-theme 'modus-operandi-deuteranopia)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
;; (add-to-list 'default-frame-alist '(font . "Ioseivka-14"))

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

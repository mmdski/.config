;;; md-ide.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Desktop
(use-package
 desktop
 :ensure nil
 :init (desktop-save-mode 1)
 :config (add-to-list 'desktop-path "."))

;;; Speedbar
(use-package
 speedbar
 :ensure nil
 :custom
 ;; Look & Feel
 (speedbar-update-flag t) (speedbar-use-images nil)
 (speedbar-frame-parameters
  '((name . "speedbar")
    (title . "speedbar")
    (minibuffer . nil)
    (border-width . 2)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t)
    (left-fringe . 10)))

 :bind
 (:map
  speedbar-mode-map
  ("b" .
   (lambda ()
     (interactive)
     (speedbar-change-initial-expansion-list "quick buffers"))))

 :config
 ;; File extensions
 (speedbar-add-supported-extension
  '(
    ;; Lisp
    ".cl" ".li?sp"
    ;; Lua / Fennel
    ".lua" ".fnl" ".fennel"
    ;; JVM
    ".kt" ".mvn" ".gradle" ".properties" ".cljs?"
    ;; Shell
    ".sh" ".bash"
    ;; Web
    ".php" ".ts" ".html?" ".css" ".less" ".scss" ".sass"
    ;; Make
    "makefile" "MAKEFILE" "Makefile"
    ;; Data
    ".json" ".yaml" ".toml"
    ;; Notes / markup
    ".md" ".markdown" ".org" ".txt" "README")))

;;; Project
(use-package
 project
 :ensure nil
 :custom (project-mode-line t) (project-kill-buffers-display-buffer-list t)
 :config
 (add-to-list
  'project-switch-commands '(project-switch-to-buffer "Switch buffer")))

;;; Flyspell
(use-package flyspell :ensure nil :hook (prog-mode . flyspell-prog-mode))

;; aggressive-indent — Minor mode to aggressively keep code indented
;; activation is per-language
(use-package aggressive-indent)

;;; Eglot
;; activation is per-language
(use-package eglot :ensure nil)

;;; editorconfig
(use-package editorconfig :ensure nil :hook (prog-mode . editorconfig-mode))

(use-package
 magit
 :ensure t
 :config
 ;; Show tracked files in magit-status
 (magit-add-section-hook
  'magit-status-sections-hook 'magit-insert-tracked-files nil 'append))

;;; Tree-sitter
(use-package
 treesit
 :ensure nil
 :when (and (fboundp 'treesit-available-p) (treesit-available-p))
 :custom (treesit-font-lock-level 4))

;;; ibuffer-project -  Group ibuffer's list by project or any function.
(use-package
 ibuffer-project
 :after ibuffer
 :hook
 (ibuffer
  .
  (lambda ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))))

;;; VTerm
(use-package vterm :ensure t)

;;; Jupyter
(use-package jupyter)

;;; Julia

;; Eglot Julia
(use-package eglot-jl :after eglot)

;; Julia REPL
(use-package
 julia-repl
 :ensure t
 :after (vterm julia-mode)
 :hook (julia-mode . julia-repl-mode)
 :config
 (with-eval-after-load 'julia-repl
   (julia-repl-set-terminal-backend 'vterm)))

;; Julia mode
(use-package
 julia-mode
 :hook
 (julia-mode
  .
  (lambda ()
    ;; Julia-specific Eglot tuning
    (setq eglot-connect-timeout 300)

    ;; Start Julia language server
    (eglot-jl-init)
    (eglot-ensure)

    ;; Format on save (buffer-local)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))
 :init
 (setenv "JULIA_EDITOR"
         (if (daemonp)
             "emacsclient -a -r"
           "emacs")))

;;; toml
;; Tree-sitter TOML grammar
(with-eval-after-load 'treesit
  (add-to-list
   'treesit-language-source-alist
   '(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))

(use-package toml-ts-mode :ensure nil :mode ("\\.toml\\'" . toml-ts-mode))

;;; _
(provide 'md-ide)
;;; md-ide.el ends here

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; from tsoding's rc.el
(defvar md/package-contents-refreshed nil)
(defun md/package-refresh-contents-once ()
  "Refreshes package contents once per session."
  (when (not md/package-contents-refreshed)
    (setq md/package-contents-refreshed t)
    (package-refresh-contents)))

(defun md/require-package (package)
  (when (not (package-installed-p package))
    (md/package-refresh-contents-once)
    (package-install package)))

(defun md/set-frame-size (frame-alist)
  "Apply width and height from FRAME-ALIST"
  (let ((params
         `((width . ,(cdr (assq 'width frame-alist)))
           (height . ,(cdr (assq 'height frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

(defun md/sf ()
  "Apply small frame settings."
  (interactive)
  (md/set-frame-size md/small-frame-alist))
(defun md/bf ()
  "Apply big frame settings."
  (interactive)
  (md/set-frame-size md/big-frame-alist))
(defun md/nf ()
  "Apply narrow frame settings."
  (interactive)
  (md/set-frame-size md/narrow-frame-alist))
(defun md/rp ()
  "Reset the position of the frame."
  (interactive)
  (let ((params
         `((left . ,(cdr (assq 'left md/big-frame-alist)))
           (top . ,(cdr (assq 'top md/big-frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

(defvar md/env-path-sep ":"
  "Separator for entries in the PATH environment variable.")

(defun md/env-path-prepend (path-to-prepend)
  "Prepend PATH-TO-PREPEND to the PATH environment variable present.

This ensures the given directory takes precedence when resolving executables."
  (let*
      ((env-path (getenv "PATH"))
       (paths (split-string env-path md/env-path-sep))
       (normalized (directory-file-name path-to-prepend))) ;; remove trailing slash
    (unless (member normalized paths)
      (setenv "PATH" (concat path-to-prepend md/env-path-sep env-path)))))

(md/require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(when (not (eq system-type 'windows-nt))
  (md/env-path-prepend (expand-file-name "~/.local/bin")))

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
  (md/env-path-prepend "/usr/local/bin")
  (md/env-path-prepend "/opt/homebrew/bin")
  (md/env-path-prepend "/opt/homebrew/opt/make/libexec/gnubin")
  (md/env-path-prepend "/opt/homebrew/opt/llvm/bin")
  (md/env-path-prepend "/Library/TeX/texbin"))

;; global formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace 'delete-trailing-lines)
(setq require-final-newline t)
(setq-default fill-column 80)

;; themes
(defvar md/light-theme 'modus-operandi
  "Preferred light theme.")
(defvar md/dark-theme 'modus-vivendi
  "Preferred dark theme.")

(load-theme md/light-theme t)

(defun md/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current-theme md/light-theme)
        (load-theme md/dark-theme t)
      (load-theme md/light-theme t))))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; tressitter
(md/require-package 'treesit-auto)
(require 'treesit-auto)

(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)

(display-time)

;; minor modes
(tool-bar-mode -1)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(setq column-number-mode t)
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook
   hook
   (lambda ()
     (setq display-line-numbers-type 'relative)
     (display-line-numbers-mode t))))
(setq ispell-program-name "aspell")
(setq-default
 line-spacing 2
 truncate-lines t
 inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(delete-selection-mode t)

;; elisp-autofmt
(md/require-package 'elisp-autofmt)
(add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode)
(add-hook
 'elisp-autofmt-mode-hook
 (lambda () (add-hook 'before-save-hook #'elisp-autofmt-buffer nil 'local)))

;; major modes
(require 'xscheme)

(md/require-package 'magit)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; org-mode options
(setq org-directory "~/Documents/org")
(md/require-package 'gnuplot)
(md/require-package 'gnuplot-mode)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-list-allow-alphabetical t)
(setq org-log-done t)
(with-eval-after-load 'org
  (setq org-latex-format-options
        (plist-put org-format-latex-options :scale 1.5)))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-file-apps '((auto-mode . emacs) ("\\.pdf\\'" . emacs)))
(add-to-list
 'org-file-apps
 '("\\.html\\'" . (lambda (file path) (browse-url-default-browser path))))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done
        org-todo-log-states) ; turn off logging
    (org-todo
     (if (= n-not-done 0)
         "DONE"
       "TODO"))))
(setq org-startup-with-inline-images t)
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; org-babel
(require 'ob-C)
(require 'ob-julia)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (python . t) (C . t) (julia .t))))
(defun md/org-confirm-babel-evaluate (lang body)
  (not (member lang '("C" "python" "julia"))))
(setq org-confirm-babel-evaluate #'md/org-confirm-babel-evaluate)
(setq org-babel-default-header-args:python
      '((:results . "output graphics") (:session . "py") (:exports . "both")))

;; Obsidian
;; only on macOs for now
;; Location of obsidian vault
;; (when (eq system-type 'darwin)
;;   (require 'obsidian)
;;   (setopt obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Toha Heavy Industries")
;;   (setopt obsidian-inbox-directory "Notes")
;;   (setopt markdown-enable-wiki-links t)
;;   (global-obsidian-mode t))

;; toml-ts-mode
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-hook 'toml-mode-hook #'flyspell-mode)
(add-hook 'toml-ts-mode-hook #'flyspell-mode)

(setq dired-listing-switches "-alh")

(setq doc-view-resolution 300)

(md/require-package 'pdf-tools)
(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Core IDE tooling
(dolist (pkg '(lsp-mode lsp-ui flycheck company projectile))
  (md/require-package pkg))

(require 'lsp-mode)
(require 'lsp-ui)
(require 'flycheck)
(require 'company)
(require 'projectile)

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-delay 0.5)
(setq lsp-ui-sideline-show-hover t)

(projectile-mode +1)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-c p") 'projectile-command-map)

;; Julia-specific setup
(defun md/julia-setup ()
  (lsp)) ;; assumes lsp-mode already installed and loaded

(md/require-package 'julia-mode)

(add-hook 'julia-mode-hook #'md/julia-setup)
(setq lsp-julia-command '("julia"))
(md/require-package 'julia-repl)

(add-hook 'julia-mode-hook #'julia-repl-mode)

(setq julia-repl-executable-records '((default "julia")))

(md/require-package 'format-all)

(add-hook 'julia-mode-hook #'format-all-mode)

;; Python-specific setup
(dolist (pkg '(pyvenv blacken))
  (md/require-package pkg))

(require 'pyvenv)
(pyvenv-mode 1)

(require 'blacken)

(defun md/python-setup ()
  (lsp)
  (blacken-mode))

(add-hook 'python-mode-hook #'md/python-setup)
(add-hook 'python-ts-mode-hook #'md/python-setup)

;; Rust-specific setup
(md/require-package 'helm-ag) ; for rust docs
(md/require-package 'rustic)
(md/require-package 'yasnippet)
(require 'rustic)
(setq rustic-lsp-client 'lsp-mode)
(setq rustic-format-on-save t)
(md/env-path-prepend (expand-file-name "~/.cargo/bin"))
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-extra-env nil) ;; suppress the sequence warning

  (add-to-list 'lsp-language-id-configuration '(rustic-mode . "rust"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("rust-analyzer"))
    :major-modes '(rustic-mode)
    :server-id 'rust-analyzer
    :initialization-options
    (lambda ()
      ;; Don't pass extraEnv at all
      (ht ("cargo" (ht)))))))
(setq projectile-project-root-files-bottom-up
      (append '("Cargo.toml") projectile-project-root-files-bottom-up))

(when (not (eq system-type 'windows-nt))
  (md/require-package 'vterm))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(provide 'init)
;;; init.el ends here

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; macos specific settings
(when (eq system-type 'darwin)  ; macOS
  (setq mac-command-modifier 'meta)  ; Command key is Meta
  (setq mac-option-modifier 'super) ; Option key is Super
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info")
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; themes
(defvar md/light-theme 'adwaita
  "Preferred light theme")
(defvar md/dark-theme 'adwaita-dark
  "Preferred dark theme")

(unless (package-installed-p 'adwaita-dark-theme)
  (package-refresh-contents)
  (package-install 'adwaita-dark-theme))

(load-theme md/dark-theme t)

(defun md/toggle-theme ()
  "Toggle between light and dark themese."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current-theme md/light-theme)
	(load-theme md/dark-theme t)
      (load-theme md/light-theme t))))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; formatting
(add-hook 'before-save-hook
	  'delete-trailing-whitespace
	  'delete-trailing-lines)
(setq require-final-newline t)

;; tressitter
(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)

;; minor modes
(tool-bar-mode -1)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq column-number-mode t)

(global-display-line-numbers-mode -1)
(setq display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook
		conf-mode-hook
		text-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode t))))

(setq ispell-program-name "aspell")

(setq-default line-spacing 2
	      truncate-lines t
	      inhibit-splash-screen t)
(setq ring-bell-function 'ignore)

;; major modes
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

;; org-mode options
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 1)))
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . emacs)))

;; toml-ts-mode
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-hook 'toml-mode-hook #'flyspell-mode)
(add-hook 'toml-ts-mode-hook #'flyspell-mode)

(setq dired-listing-switches "-alh")

(setq doc-view-resolution 300)

(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; python mode
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook 'blacken-mode)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

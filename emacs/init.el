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

(defun md/apply-frame-alist (alist)
  "Apply frame parameters from ALIST to the current frame."
  (let ((frame (selected-frame)))
    (dolist (param alist)
      (modify-frame-parameters frame (list param)))))

(defun md/sf ()
  "Apply small frame settings."
  (interactive)
  (md/apply-frame-alist md/small-frame-alist))
(defun md/bf ()
  "Apply big frame settings."
  (interactive)
  (md/apply-frame-alist md/big-frame-alist))

;; macos specific settings
(when (eq system-type 'darwin) ; macOS
  ;  (setq mac-command-modifier 'meta) ; Command key is Meta
  ;  (setq mac-option-modifier 'super) ; Option key is Super
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info")
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (let ((texbin-path "/Library/TeX/texbin")
        (path-sep ":")
        (env-path (getenv "PATH")))
    (unless (string-match-p texbin-path env-path)
      (setenv "PATH" (concat env-path path-sep texbin-path)))))

;; keybindings
(global-set-key (kbd "M-o") 'other-window)

;; global formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace
          'delete-trailing-lines)
(setq require-final-newline t)

;; themes
(md/require-package 'adwaita-dark-theme)
(defvar md/light-theme 'adwaita
  "Preferred light theme")
(defvar md/dark-theme 'adwaita-dark
  "Preferred dark theme")

(load-theme md/dark-theme t)

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
26
(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)

;; minor modes
(tool-bar-mode -1)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq column-number-mode t)
(dolist (hook
         '(prog-mode-hook
           conf-mode-hook text-mode-hook Info-mode-hook))
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
 (lambda ()
   (add-hook 'before-save-hook #'elisp-autofmt-buffer nil 'local)))

;; major modes
(md/require-package 'magit)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; org-mode options
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 1)))
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-file-apps '((auto-mode . emacs) ("\\.pdf\\'" . emacs)))

;; obsidian
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
(add-hook
 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; python mode
(md/require-package 'blacken)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook 'blacken-mode)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(when (not (eq system-type 'windows-nt))
  (md/require-package 'vterm))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

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
  "Resets the position of the frame"
  (interactive)
  (let ((params
         `((left . ,(cdr (assq 'left md/big-frame-alist)))
           (top . ,(cdr (assq 'top md/big-frame-alist))))))
    (modify-frame-parameters (selected-frame) params)))

(defvar md/env-path-sep ":"
  "Separator for entries in the PATH environment variable.")

(defun md/env-path-prepend (path-to-prepend)
  "Prepend `path-to-prepend` to the PATH environment variable if it's not already present.

This ensures the given directory takes precedence when resolving executables."
  (let*
      ((env-path (getenv "PATH"))
       (paths (split-string env-path md/env-path-sep))
       (normalized (directory-file-name path-to-prepend))) ;; remove trailing slash
    (unless (member normalized paths)
      (setenv "PATH" (concat path-to-prepend md/env-path-sep env-path)))))

(when (not (eq system-type 'windows-nt))
  (md/env-path-prepend "~/.local/bin"))

;; Macos specific settings
(when (eq system-type 'darwin) ; macOS
  ;  (setq mac-command-modifier 'meta) ; Command key is Meta
  ;  (setq mac-option-modifier 'super) ; Option key is Super
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info")
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/opt/make/libexec/gnubin")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (md/env-path-prepend "/Library/TeX/texbin")
  (md/env-path-prepend "/opt/homebrew/opt/make/libexec/gnubin")
  (md/env-path-prepend "/opt/homebrew/opt/llvm/bin")
  (md/env-path-prepend "/usr/local/bin"))

;; global formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace 'delete-trailing-lines)
(setq require-final-newline t)
(setq-default fill-column 80)

;; themes
(defvar md/light-theme 'modus-operandi
  "Preferred light theme")
(defvar md/dark-theme 'modus-vivendi
  "Preferred dark theme")

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

;; minor modes
(tool-bar-mode -1)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(setq column-number-mode t)
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook Info-mode-hook))
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
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

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

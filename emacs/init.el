(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; --- Personal Commands ---
(defun md/append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
		     value)))

(defvar md/light-theme 'adwaita
  "Preferred light theme")
(defvar md/dark-theme 'adwaita-dark
  "Preferred dark theme")
;; (defvar md/dark-theme 'doom-xcode
;;   "Preferred dark theme")

(defun md/toggle-theme ()
  "Toggle between light and dark themese."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current-theme md/light-theme)
	(load-theme md/dark-theme t)
      (load-theme md/light-theme t))))

 ;; themes
(unless (package-installed-p 'adwaita-dark-theme)
  (package-refresh-contents)
  (package-install 'adwaita-dark-theme))

(load-theme md/dark-theme t)

; (set-face-attribute 'default nil :font "Source Code Pro-14")
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; minor modes
(tool-bar-mode -1)
(scroll-bar-mode 0)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(blink-cursor-mode 0)

(setq-default line-spacing 2
	      truncate-lines t
	      inhibit-splash-screen t)
(setq ring-bell-function 'ignore)

;; major modes
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

;; org-mode options
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . emacs)))

(setq dired-listing-switches "-alh")

(setq doc-view-resolution 300)

;; macos specific settings
;; keybindings
(when (eq system-type 'darwin)  ; macOS
  (setq mac-command-modifier 'meta)  ; Command key is Meta
  (setq mac-option-modifier 'super))

;; hooks
(add-hook 'before-save-hook
	  'delete-trailing-whitespace
	  'delete-trailing-lines)



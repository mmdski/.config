;;; md-completion.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Enable Vertico.
;; VERTical Interactive COmpletion
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shring the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
;; Enrich existing commands with completion annotations
(use-package marginalia
  :init
  (marginalia-mode))

;;; Enable Consult
;; Consulting completing-read
(use-package consult
  :bind (("C-s" . consult-line)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

;;; Orderless
;; Completion style for matching regexps in any order
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles . (partial-completion))))))

;;; Embark — Conveniently act on minibuffer completions 
(use-package embark
  :bind (("<remap> <describe-bindings>" . embark-bindings)
         ("C-." . embark-act))

  :init
  ;; Use Embark for prefix help (C-h after a prefix)
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (use-package embark-consult
    :after (embark consult)
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

;;; Corfu — Completion in Region FUnction
(use-package corfu
  :init
  (global-corfu-mode 1)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)

  :config
  ;; Popup info is an extension included with Corfu (not a separate package)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)

  :bind
  (:map corfu-map
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-d" . corfu-popupinfo-toggle)
	("RET" . nil)))

;;; Cape — Completion At Point Extensions
(use-package cape
  :init
  ;; Add useful CAPFs
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence pcomplete CAPF (important for Corfu)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  :config
  ;; No auto-completion / no completion-on-quit in eshell
  (defun crafted-completion-corfu-eshell ()
    "Special settings for when using corfu with eshell."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode 1))
  (add-hook 'eshell-mode-hook #'crafted-completion-corfu-eshell))

;;; _
(provide 'md-completion)
;;; md-completion.el ends here

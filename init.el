;;
;; Package management.
;;

;; Load straight.el for package management instead of package.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (error
     "Install straight.el by cloning https://github.com/radian-software/straight.el into %s"
     bootstrap-file))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;;
;; Startup prefs.
;;

;; Turn off the startup screen.
(setq inhibit-startup-screen t)

;; Set initial message of scratch buffer.
(setq initial-scratch-message "")

;; Disable the startup message displayed in the echo area.
(defun display-startup-echo-area-message ()
  (message (concat "Let the hacking begin, " user-login-name ".")))

;; Get rid of the tool bar (we don't need buttons).
(tool-bar-mode 0)

;; Get rid of the menu bar (don't need to see menus).
(menu-bar-mode 0)

;; Git rid of scroll bars (don't need mouse handles).
(scroll-bar-mode 0)

;;
;; Global settings.
;;

;; Show column number in mode line.
(column-number-mode)

;; Fill settings.
(setq-default fill-column 100)
(global-display-fill-column-indicator-mode t)

;; Turn off the stupid bell.
(setq ring-bell-function 'ignore)

;; No tabs, please.
(setq-default indent-tabs-mode nil)

;; Theme.
(use-package zenburn-theme
  :straight t
  :config (load-theme 'zenburn t))

;; Disable the stupid *~ backup files, which only happen on first save anyway.
(setq make-backup-files nil)

;; Evil mode (with evil-collection's keybindings instead of the default).
(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config
  (use-package evil-collection
    :straight t
    :init (setq evil-collection-setup-minibuffer t)
    :config (evil-collection-init))
  (use-package evil-escape
    :straight t
    :config (evil-escape-mode))
  (use-package org-evil
    :straight t)
  (evil-mode 1))

;; Maximize the frame.
(toggle-frame-maximized)

;;
;; Tools.
;;

;; Helm for better M-x.
(use-package helm
  :straight t
  :bind (("M-x" . helm-M-x)
	 ("C-h a" . helm-apropos)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ("C-s" . helm-occur))
  :demand t
  :config (helm-mode 1))

;; Terminal (vterm).
(use-package vterm
  :straight t
  :bind ("C-c v" . vterm))

;; Treemacs (eclipse-like project explorer).
(use-package treemacs
  :straight t
  :config
  (use-package treemacs-evil :straight t)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-git-mode 'deferred)
  :bind ("C-c t" . treemacs))

;; Magit (git client).
(use-package magit
  :straight t)

;; Web mode.
(use-package web-mode
  :straight t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (define-derived-mode tsx-mode web-mode "TypeScriptX"
    "Minor mode for editing Typescript files with JSX syntax extensions."
    (setq-local web-mode-enable-auto-quoting nil))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))

;; Javascript mode.
;; TODO: Should we use lsp mode for JS as well?
(setq js-indent-level 2)

;; Typescript mode.
(use-package typescript-mode
  :straight t
  :config (setq typescript-indent-level 2))

;; LSP.
(use-package lsp-mode
  :straight t
  :hook ((typescript-mode . lsp-deferred)
	 (tsx-mode . lsp-deferred)))

(use-package lsp-treemacs
  :straight t
  :bind ("C-c e" . lsp-treemacs-errors-list))

;; Prettier.
(use-package prettier
  :straight t
  :commands prettier-prettify)

;;
;; Commands.
;;

;; Shortcut to open this init.el file.
(defun visit-user-init-file ()
  "Visit your emacs init file."
  (interactive)
  (find-file user-init-file))

;;
;; Specific to my projects.
;;
(setq safe-local-variable-values
      '((eval let
	      ((project-directory
		(car
		 (dir-locals-find-file default-directory))))
	      (setq lsp-clients-typescript-server-args
		    `("--tsserver-path"
		      ,(concat project-directory
			       ".yarn/sdks/typescript/bin/tsserver")
		      "--stdio")))))

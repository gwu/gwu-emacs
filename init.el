;;
;; Env.
;;
(setenv "PATH" (concat (getenv "PATH") ":/home/gwu/n/bin"))
(add-to-list 'exec-path "/home/gwu/n/bin")

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

(use-package general
  :straight t)

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
  :after general
  :straight t
  :general ('motion "," nil)
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :general
  (magit-status-mode-map "SPC" nil)
  :init (setq evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :config (evil-escape-mode))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :general
  ('normal org-mode-map :prefix ","
           "c" '(org-toggle-checkbox :wk "Toggle checkbox"))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-def 'motion org-agenda-mode-map "SPC" nil))

;;
;; Tools.
;;

;; Org mode.
(add-hook
 'org-mode-hook
 (lambda ()
   (auto-fill-mode)
   (setq-default fill-column 65)))

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
  :straight t)

;; Treemacs (eclipse-like project explorer).
(use-package treemacs
  :straight t
  :config
  (use-package treemacs-evil :straight t)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-git-mode 'deferred))

;; Magit (git client).
(use-package magit
  :straight t
  :commands magit)

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
  :commands lsp-treemacs-errors-list)

;; Prettier.
(use-package prettier
  :straight t
  :init (setenv "NODE_ENV" "/home/gwu/n/bin"))

;; Which key (to show what keybindings with leaders are).
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-add-column-padding 8)
  (setq which-key-max-display-columns 4))

;;
;; Commands.
;;

;; Shortcut to open this init.el file.
(defun visit-user-init-file ()
  "Visit your emacs init file."
  (interactive)
  (find-file user-init-file))


;;
;; Global key bindings
;;
(general-def 'motion :prefix "SPC"
  "" '(nil :wk "Main")
  "SPC" '(helm-M-x :wk "Execute extended command")
  "b" '(nil :wk "Buffer")
  "b b" '(helm-buffers-list :wk "Switch to buffer")
  "b d" '(evil-delete-buffer :wk "Delete buffer")
  "b l" '(list-buffers :wk "List buffers")
  "f" '(nil :wk "File")
  "f f" '(helm-find-files :wk "Find file")
  "f s" '(save-buffer :wk "Save file")
  "f e" '(visit-user-init-file :wk "Open Emacs init.el file")
  "s" '(helm-occur :wk "Search buffer")
  "t" '(vterm :wk "Open terminal")
  "p" '(treemacs-select-window :wk "Project panel")
  "g" '(magit :wk "Git status")
  "=" '(nil :wk "Format")
  "= =" '(prettier-prettify :wk "Prettify buffer")
  "h" '(evil-window-left :wk "Select window left")
  "k" '(evil-window-up :wk "Select window up")
  "j" '(evil-window-down :wk "Select window down")
  "l" '(evil-window-right :wk "Select window right")
  "H" '(evil-window-move-far-left :wk "Move window right")
  "L" '(evil-window-move-far-right :wk "Move window left")
  "J" '(evil-window-move-very-bottom :wk "Move window down")
  "K" '(evil-window-move-vary-top :wk "Move window up")
  "w" '(nil :wk "Window")
  "w d" '(evil-window-delete :wk "Delete window")
  "w a" '(ace-window :wk "Select window")
  "o" '(nil :wk "Org")
  "o a" '(org-agenda :wk "Org agenda")
  "o t" '(org-todo-list :wk "Org todo list")
  "q" '(nil :wk "Quit")
  "q q" '(save-buffers-kill-terminal :wk "Quit Emacs"))

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

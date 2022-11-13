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

;; Start in full screen mode.
(toggle-frame-fullscreen)

;; Get rid of the tool bar (we don't need buttons).
(tool-bar-mode 0)

;; Get rid of the menu bar (don't need to see menus).
(menu-bar-mode 0)

;;
;; Global settings.
;;

;; Fill settings.
(setq-default fill-column 100)
(global-display-fill-column-indicator-mode t)

;; Turn off the stupid bell.
(setq ring-bell-function 'ignore)

;; No tabs, please.
(setq indent-tabs-mode nil)

;; Theme.
(use-package zenburn-theme
  :straight t
  :config (load-theme 'zenburn t))

;; Disable the stupid *~ backup files, which only happen on first save anyway.
(setq make-backup-files nil)

;; Evil mode (with evil-collection's keybindings instead of the default)
(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))
(use-package evil-collection
  :straight t
  :after evil
  :config (evil-collection-init))

;;
;; Tools.
;;

;; Terminal (vterm).
(use-package vterm
  :straight t)

;; Treemacs (eclipse-like project explorer).
(use-package treemacs
  :straight t
  :config (use-package treemacs-evil :straight t))

;; Magit (git client).
(use-package magit
  :straight t)

;;
;; Commands.
;;

;; Shortcut to open this init.el file.
(defun visit-user-init-file ()
  "Visit your emacs init file."
  (interactive)
  (find-file user-init-file))

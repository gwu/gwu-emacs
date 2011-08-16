;; Emacs server
(server-start)

;; Load Java settings.
(load-file (expand-file-name "~/.emacs.d/java.el"))

;; Fix the color formatting of the shell buffer
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Global key bindings
(global-set-key (kbd "M-g c") 'compile)
(global-set-key (kbd "M-g r") 'recompile)
(global-set-key (kbd "M-g M-i") 'jde-import-all)
(global-set-key (kbd "M-g i") 'java-organize-imports)
(global-set-key (kbd "M-s") 'speedbar-frame-mode)
(global-set-key (kbd "<backtab>") 'dabbrev-expand)
(global-set-key (kbd "M-g j") 'jde-open-class-at-point)
(global-set-key (kbd "M-g M-j") 'jde-help-symbol)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(compile-command "maven test-compile")
 '(default-frame-alist (quote ((vertical-scroll-bars) (width . 100) (height . 45))))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jde-help-docsets (quote (("JDK API" "http://download.oracle.com/javase/6/docs/api" nil))))
 '(jde-javadoc-exception-tag-template "\"* @throws \" type")
 '(jde-jdk (quote ("1.6")))
 '(jde-jdk-doc-url "http://download.oracle.com/javase/6/docs/api")
 '(jde-jdk-registry (quote (("1.6" . "/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents"))))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-trailing-whitespace t)
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(user-mail-address "wugarrett@gmail.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#dee")))))

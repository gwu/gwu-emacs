;; Java settings and functions.

;; JDE setup
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elib-1.0"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/cedet-1.0/common"))
(load-file (expand-file-name "~/.emacs.d/cedet-1.0/common/cedet.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/jdee-2.4.0.1/lisp"))
(require 'jde)

;; Java Code formatting
(defun java-indent-setup ()
  (progn
    (c-set-offset 'arglist-intro '++)
    (c-set-offset 'arglist-cont-nonempty '++)
    (c-set-offset 'statement-cont '++)
    (c-set-offset 'inher-intro '++)
    (c-set-offset 'func-decl-cont '++)))
(add-hook 'java-mode-hook 'java-indent-setup)

(defun java-organize-imports ()
  "Organizes the imports in the current Java file by sorting and removing extranous imports"
  (interactive)
  (jde-import-kill-extra-imports)
  (jde-import-organize))

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

;; Functions.
(defun java-organize-imports ()
  "Organizes the imports in the current Java file by sorting and removing extranous imports"
  (interactive)
  (jde-import-kill-extra-imports)
  (jde-import-organize))

;; Speedbar customization.
(defun speedbar-expand-children ()
  "Expands all children of the current node"
  (interactive)
  (setq p (point-marker))
  (beginning-of-line)
  (loop do
        (speedbar-expand-line)
        (forward-line)
        while (and (not (eobp)) (not (equal (char-after) 48))))
  (goto-char p))
(define-key speedbar-key-map (kbd "o") 'speedbar-expand-children)
(define-key speedbar-key-map (kbd "j") 'speedbar-next)
(define-key speedbar-key-map (kbd "k") 'speedbar-prev)
(define-key speedbar-key-map (kbd "c") 'speedbar-contract-line)

;; Parse maven compilation error messages.
(add-to-list 'compilation-error-regexp-alist 'maven)
(add-to-list 'compilation-error-regexp-alist-alist
             '(maven "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                     1 2 3))

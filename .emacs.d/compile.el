;; Compilation functions and settings.

(defun compilation-buffer-hook ()
  "Called to configure the compilation buffer."
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook 'compilation-buffer-hook)

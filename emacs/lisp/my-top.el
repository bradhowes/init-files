;;; package -- my-top -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Functions for running top and htop
;;; Code:

;;;###autoload
(defun my/htop ()
  "Run htop in a term buffer."
  (interactive)
  (let* ((name "*htop*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "sudo htop" name))))

;;;###autoload
(defun my/top ()
  "Run top in a term buffer."
  (interactive)
  (let* ((name "*top*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "top" name))))

(provide 'my-top)
;;; my-top.el ends here

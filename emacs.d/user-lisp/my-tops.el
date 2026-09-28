;;; my-tops.el --- custom window/frame navigation routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-constants)

;;;###autoload
(defun my/htop ()
  "Run htop in a term buffer."
  (interactive)
  (let ((name "*htop*")
        (cmd (if my/is-macosx "sudo htop" "/bin/htop")))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term cmd name))))

;;;###autoload
(defun my/top ()
  "Run top in a term buffer."
  (interactive)
  (let ((name "*top*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "/usr/bin/top" name))))

(provide 'my-tops)

;;; my-tops.el ends here.

;;; package -- my-scratch-plus-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'scratch-plus)

;;;###autoload
(defun my/show-scratch-buffer ()
  "Show scratch buffer."
  (interactive)
  (switch-to-buffer (scratch-plus-buffer initial-major-mode nil)))

;;;###autoload
(defun my/scratch-plus-mode-hook ()
  "Custom hook for `scratch-plus-mode'."
  (unless (file-exists-p scratch-plus-save-directory)
    (make-directory scratch-plus-save-directory t))
  (my/show-scratch-buffer))

;;;###autoload
(defun my/scratch-plus-mode-init ()
  "Custom init for `scratch-plus-mode'."
  (unless (file-exists-p scratch-plus-save-directory)
    (make-directory scratch-plus-save-directory t))
  (scratch-plus-mode))

(provide 'my-scratch-plus-mode)
;;; my-scratch-plus-mode.el ends here

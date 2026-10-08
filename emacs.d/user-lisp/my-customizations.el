;;; package --- my-customizations.el -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Runtime constants that describe various properties of the environment.
;;; Code:

;;;###autoload
(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

;;;###autoload
(defcustom my/use-c-ts-mode nil
  "Use `c-ts-mode' when t."
  :type '(boolean)
  :group 'my/customizations)

(provide 'my-customizations)

;;; my-customizations.el ends here

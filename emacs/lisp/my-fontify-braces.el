;;; package -- my/my-fontify-braces -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defface my/font-lock-brace
  '((((class color) (background light))
     (:foreground "tomato1" :weight bold))
    (((class color) (background dark))
     (:foreground "tomato1" :weight bold)))
  "Custom face used to highlight parentheses, braces, and brackets."
  :group 'my/customizations)

(defun my/fontify-braces (&optional mode)
  "Add font-lock for braces ([] {} ()) to MODE.
If MODE is nil then apply to the current mode."
  (font-lock-add-keywords mode '(("[][(){}]" . 'my/font-lock-brace))))

(provide 'my-fontify-braces)
;;; my-fontify-braces.el ends here

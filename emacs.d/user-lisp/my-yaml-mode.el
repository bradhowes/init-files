;;; package -- my-yaml-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'yaml-ts-mode)

(defun my/yaml-ts-mode-hook ()
  "Customization hook for `yaml-ts-mode'."
  (setopt yaml-ts-mode-yamllint-options '("-d" "relaxed"))
  (flymake-mode t))

(defun my/yaml-ts-setup-hook ()
  "Initialization hook for `yaml-ts-mode'."
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

(provide 'my-yaml-mode)
;;; my-yaml-mode.el ends here

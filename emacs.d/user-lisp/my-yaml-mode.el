;;; package -- my-yaml-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'yaml-ts-mode)

(defun my/yaml-ts-mode-hook ()
  "Customization hook for `yaml-ts-mode'."
  (setopt yaml-ts-mode-yamllint-options '("-d" "{extends: relaxed, rules: {line-length: disable}}"))
  (flymake-mode t))

(defun my/yaml-ts-setup-hook ()
  "Initialization hook for `yaml-ts-mode'."
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

(provide 'my-yaml-mode)
;;; my-yaml-mode.el ends here

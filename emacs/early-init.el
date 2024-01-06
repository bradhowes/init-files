;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(tool-bar-mode -1)

(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold threshold))))

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")


;;; early-init.el ends here.

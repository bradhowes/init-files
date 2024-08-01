;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      load-prefer-newer t)

(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold threshold))))

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;;; early-init.el ends here.

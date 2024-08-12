;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) initial-frame-alist)
(push '(tool-bar-lines . 0) initial-frame-alist)
(push '(vertical-scroll-bar) initial-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bar) default-frame-alist)

;; Resizing the Emacs frame can be an expensive part of changing the font. Inhibit this to reduce startup times with
;; fonts that are larger than the system default
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      load-prefer-newer t)

(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  ;; NOTE: needs lexical-binding for this to work and capture the value of 'threshold'
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold threshold))))

;; Stop Emacs from flashing a `white' screen when starting up
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;;; early-init.el ends here.

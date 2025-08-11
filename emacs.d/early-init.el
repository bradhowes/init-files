;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (setq native-comp-speed -1)

(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))) (menu-bar-mode 1))
(when (fboundp 'tab-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Resizing the Emacs frame can be an expensive part of changing the font. Inhibit this to reduce startup times with
;; fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      custom-file nil
      load-prefer-newer t)

(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  ;; NOTE: needs lexical-binding for this to work and capture the value of `threshold'
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold threshold))))

;; Stop Emacs from flashing a `white' screen when starting up
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

(message "user-emacs-directory: %s" user-emacs-directory)

;; Our personal Emacs files are found in the `lisp' directory in the `user-emacs-directory'.
(let ((my/lisp (file-name-as-directory (file-name-concat user-emacs-directory "lisp"))))
  (push my/lisp load-path)
  (push my/lisp trusted-content)
  (push user-emacs-directory trusted-content))

(message "trusted-content: %s" trusted-content)
(message "load-path: %s" load-path)

;;; early-init.el ends here.

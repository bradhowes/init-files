;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'comp))

(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode 1)
  (push '(menu-bar-lines . 0) default-frame-alist))

(when (fboundp 'tab-bar-mode)
  (tool-bar-mode -1)
  (push '(tool-bar-lines . 0) default-frame-alist))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Resizing the Emacs frame can be an expensive part of changing the font. Inhibit this to reduce startup times with
;; fonts that are larger than the system default.
(setq custom-file nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      load-prefer-newer t
      native-comp-speed 2)

;; Increase GC threshold to reduce startup times. Restore threshold once emacs is running.
(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  ;; NOTE: needs lexical-binding for this to work and capture the value of `threshold'
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold threshold))))

;; Stop Emacs from flashing a `white' screen when starting up
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;; (message "user-emacs-directory: %s" user-emacs-directory)

;; Our personal Emacs files are found in the `lisp' directory in the `user-emacs-directory'.
(let ((my/lisp (file-name-as-directory (file-name-concat user-emacs-directory "lisp"))))
  (push my/lisp load-path)
  (push (file-name-concat my/lisp "key-chord") load-path)
  (push my/lisp trusted-content)
  (push user-emacs-directory trusted-content))

;; (message "trusted-content: %s" trusted-content)
;; (message "load-path: %s" load-path)

;;; early-init.el ends here.

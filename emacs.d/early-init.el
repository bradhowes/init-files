;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'comp))

;; Do not show menu bar if running on TTY
(when (and (fboundp 'menu-bar-mode)
           (tty-type))
  (menu-bar-mode -1)
  (push '(menu-bar-lines . 0) default-frame-alist))

;; Never grew to like tabs
(when (fboundp 'tab-bar-mode)
  (tool-bar-mode -1)
  (push '(tool-bar-lines . 0) default-frame-alist))

;; ...or toolbars either
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Save some space by removing vertical toolbars.
;; Use `modeline' indicator to gauge buffer position.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Resizing the Emacs frame can be an expensive part of changing the font. Inhibit this to reduce startup times with
;; fonts that are larger than the system default.
(setq custom-file nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      ;; Load the newer of *.el/*.elc files without warning
      load-prefer-newer t)

;; Increase GC threshold to reduce startup times. Restore threshold once emacs is running.
(let ((threshold gc-cons-threshold)
      (gc-cons-threshold most-positive-fixnum))
  ;; NOTE: needs lexical-binding for this to work and capture the value of `threshold'
  (add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold threshold
                                        inhibit-redisplay nil))))

;; Stop Emacs from flashing a `white' screen when starting up
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;; Temporary hack to fix Emacs launching in iTerm2.
(load "/Applications/Emacs.app/Contents/Resources/site-lisp/site-start" t t)

;; (message "user-emacs-directory: %s" user-emacs-directory)
;; (message "trusted-content: %s" trusted-content)
;; (message "load-path: %s" load-path)

;;; early-init.el ends here.

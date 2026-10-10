;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
(let* ((saved-gc-cons-threshold gc-cons-threshold)
       (saved-gc-cons-percentage gc-cons-percentage)
       (gc-cons-threshold most-positive-fixnum)
       (gc-cons-percentage 0.8)
       (saved-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  ;; NOTE: needs lexical-binding for this to work and capture the value of `threshold'
  (add-hook 'after-init-hook (lambda ()
                               (setq gc-cons-threshold saved-gc-cons-threshold
                                     gc-cons-percentage saved-gc-cons-percentage
                                     file-name-handler-alist saved-file-name-handler-alist))))

;; Stop Emacs from flashing a `white' screen when starting up
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;; (message "user-emacs-directory: %s" user-emacs-directory)
;; (message "trusted-content: %s" trusted-content)
;; (message "load-path: %s" load-path)

;;; early-init.el ends here.

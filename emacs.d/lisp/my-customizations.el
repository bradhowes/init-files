;;; package --- my-customizations.el -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Runtime constants that describe various properties of the environment.
;;; Code:

(require 'popper)

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defcustom my/screen-4k-pick 0
  "The 4K screen to use for Emacs frames."
  :type '(natnum)
  :options '(0 1)
  :group 'my/customizations)

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

(defcustom my/git-sync-buffer-name " *my/git-sync*"
  "The name of the buffer to use to hold output from my/git-sync func."
  :type '(string)
  :group 'my/customizations
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (set-default-toplevel-value 'popper-reference-buffers (append (list value) popper-reference-buffers))))

(provide 'my-customizations)

;;; my-customizations.el ends here

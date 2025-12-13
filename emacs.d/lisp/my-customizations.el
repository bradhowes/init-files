;;; package --- my-customizations.el -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Runtime constants that describe various properties of the environment.
;;; Code:

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

(defcustom my/git-sync-buffer-name " *my/git-sync*"
  "The name of the buffer to use to hold output from my/git-sync func."
  :type '(string)
  :group 'my/customizations)

;;   :set (lambda (symbol value)
;;          (set-default-toplevel-value symbol value)
;;          ;; (set-default-toplevel-value 'popper-reference-buffers (append (list value) popper-reference-buffers))))
;;          (set-default-toplevel-value 'popper-reference-buffers (append (list value) popper-reference-buffers))))

(defcustom my/layout-default-display-4k 0
  "The 4K display to use for new Emacs frames."
  :type '(natnum)
  :options '(0 1)
  :group 'my/customizations)

(defcustom my/layout-rows-4k 103
  "Number of rows in a new frame on a 4K desktop."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-rows-laptop 88
  "Number of rows in a new frame on a laptop desktop."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-rows-terminal 40
  "Number of rows in a terminal."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-cols-4k 132
  "Number of columns in a new frame on a 4K desktop."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-cols-laptop 132
  "Number of columns in a new frame on a laptop desktop."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-cols-terminal 80
  "Number of columns in a terminal."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-frame-pixel-width-4k 1338
  "Width of a frame in pixels on a 4K desktop.
NOTE: this should be calculated instead of being a constant."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-frame-pixel-width-laptop 944
  "Width of a frame in pixels on a 4K desktop.
NOTE: this should be calculated instead of being a constant."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-font-size-4k 16
  "Font size to use when on 4K desktop."
  :type '(natnum)
  :group 'my/customizations)

(defcustom my/layout-font-size-laptop 12
  "Font size to use when on laptop desktop."
  :type '(natnum)
  :group 'my/customizations)

(provide 'my-customizations)

;;; my-customizations.el ends here

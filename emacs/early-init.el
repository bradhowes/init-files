;;; package --- Summary
;;; Commentary:
;;; Code:

(defconst my-gc-cons-threshold gc-cons-threshold
  "Original value from start of Emacs.")

(setq gc-cons-threshold most-positive-fixnum)

(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")

;;; early-init.el ends here.

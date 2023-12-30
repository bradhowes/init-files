;;; package --- Summary
;;; Commentary:
;;; Code:

(defconst my-gc-cons-threshold gc-cons-threshold
  "Original value from start of Emacs.")

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold my-gc-cons-threshold)))

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")

;;; early-init.el ends here.

;;; package -- my-keycast-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(use-package keycast
  :defer nil
  :ensure t
  :commands (keycast-invisible-mode)
  :vc (:url "https://github.com/tarsius/keycast")
  :custom
  (keycast-mode-line-format "%2s%k%c%r"))

;;;###autoload
(define-minor-mode my/keycast-mode
  "Show current command and its key binding in the mode line.
See https://github.com/tarsius/keycast/issues/7#issuecomment-3985332002."
  :global t
  :group 'my/customizations
  (let ((my/keycast-mode-line '("" keycast-mode-line " ")))
    (if my/keycast-mode
        (progn
          (keycast-invisible-mode 1)
          (add-to-list 'global-mode-string my/keycast-mode-line)
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-hook 'minibuffer-exit-hook 'keycast--minibuffer-exit t))
      (setopt global-mode-string (delete my/keycast-mode-line global-mode-string))
      (remove-hook 'pre-command-hook 'keycast--update)
      (remove-hook 'minibuffer-exit-hook 'keycast--minibuffer-exit)
      (keycast-invisible-mode -1))))

(provide 'my-keycast-mode)
;;; my-keycast-mode.el ends here

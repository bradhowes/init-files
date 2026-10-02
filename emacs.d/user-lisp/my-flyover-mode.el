;;; my-flyover-mode.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'flyover)

;;;###autoload
(defun my/flyover-mode-hook ()
  "Custom hook for flyover."
  (setq flyover-background-lightness 45
        flyover-percent-darker 40
        flyover-display-mode 'hide-on-same-line
        flyover-max-line-length 120))

(provide 'my-flyover-mode)

;;; my-flyover-mode.el ends here.

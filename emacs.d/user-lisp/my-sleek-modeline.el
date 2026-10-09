;;; my-sleek-modeline-.el --- Custom mode segments for sleek-modeline -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'sleek-modeline-core)

;;;###autoload
(defun my/sleek-modeline-keycast-register ()
  "Register a segment to show `keycast' info when enabled."
  (condition-case nil
      (require 'keycast)
    (error "The `keycast' package is not available -- cannot continue"))

  (defun my/sleek-modeline-keycast ()
    "Obtain formatted keycast content or empty string."
    (or (and (fboundp 'keycast-invisible-mode)
             keycast-invisible-mode
             (keycast-format keycast-mode-line-format))
        ""))

  (sleek-modeline-register-segment 'keycast
				   :fn 'my/sleek-modeline-keycast
				   :side 'left
				   :priority 40))

(provide 'my/sleek-modeline)
;;; my-sleek-modeline.el ends here

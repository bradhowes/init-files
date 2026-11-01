;;; my-speedbar.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'speedbar)

(defun my/speedbar-toggle-expansion-list ()
  "Toggle between `files' and `quick buffers' mode."
  (interactive)
  (speedbar-change-initial-expansion-list (if (string= speedbar-initial-expansion-list-name "files")
                                              "quick buffers"
                                            "files")))

;;;###autoload
(defun my/speedbar-show ()
  "Move into speedbar enabling if necessary."
  (interactive)
  (if (speedbar-frame-or-window)
      (speedbar-get-focus)
    (speedbar)
    (speedbar-get-focus)))

;;;###autoload
(defun my/speedbar-hide ()
  "Hide any active speedbar."
  (interactive)
  (when (speedbar-frame-or-window)
      (speedbar)))

(defun my/speedbar-edit-line ()
  "Customized `speedbar-edit-line'."
  (interactive)
  (speedbar-edit-line)
  (my/speedbar-hide))

;;;###autoload
(defun my/speedbar-mode-hook ()
  "Customize `speedbar-mode'."
  ;; !!! Needs work
  (keymap-set speedbar-mode-map "q" #'my/speedbar-hide)
  (keymap-set speedbar-mode-map "b" #'my/speedbar-toggle-expansion-list)
  (keymap-set speedbar-mode-map "f" #'my/speedbar-toggle-expansion-list)
  (keymap-set speedbar-mode-map "RET" #'my/speedbar-edit-line)
  (keymap-set speedbar-mode-map "e" #'my/speedbar-edit-line))

;;;###autoload
(defun my/speedbar-initialize ()
  "Customize speedbar."
  (setopt speedbar-prefer-window t
          speedbar-use-images nil))

(provide 'my-speedbar)
;;; my-speedbar.el ends here.

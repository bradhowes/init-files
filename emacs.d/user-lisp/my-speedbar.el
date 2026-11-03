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
(defun my/speedbar-toggle-view ()
  "Toggle the speedbar window appearance."
  (interactive)
  (if (eq speedbar-buffer (current-buffer))
      (speedbar)
    (speedbar-get-focus)))

(defun my/speedbar-edit-line-and-close ()
  "Perform `speedbar-edit-line' and then close speedbar view."
  (interactive)
  (speedbar-edit-line)
  (my/speedbar-toggle-view))

;;;###autoload
(defun my/speedbar-reconfigure-keymaps-hook ()
  "Customizing keymaps hook for `speedbar-mode'."
  (keymap-local-set "RET" #'my/speedbar-edit-line-and-close)
  (keymap-local-set "e" #'my/speedbar-edit-line-and-close)
  (keymap-local-set "TAB" #'speedbar-toggle-line-expansion)
  (keymap-local-set "/" #'isearch-forward)
  (keymap-local-set "q" #'my/speedbar-toggle-view))

(provide 'my-speedbar)
;;; my-speedbar.el ends here.

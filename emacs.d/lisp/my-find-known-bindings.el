;;; package -- my-find-known-bindings -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(condition-case nil
    (require 's)
  (file-error
   (package-refresh-contents)
   (package-install 's)))

(require 's)

(defun my/find-known-bindings (key)
  "Find all key bindings for KEY.
Reads in KEY if not provided. Format is what would be
seen in `describe-key' output (e.g. `C-c' or `C-M-S-u')."
  (interactive "sList known bindings for key sequence: ")
  (let ((parsed (key-parse key)))
    (with-current-buffer (get-buffer-create "*known bindings*")
      (erase-buffer)
      (mapatoms (lambda (sym)
                  (when (or (eq sym 'global-map)
                            (and (boundp sym)
                                 (symbol-value sym)
                                 (s-ends-with-p "-mode-map" (symbol-name sym))
                                 (keymapp (symbol-value sym))))
                    (let ((binding (lookup-key (symbol-value sym) parsed t)))
                      (when (and binding
                                 (not (numberp binding)))
                        (insert (format "%-40s %s\n" sym (if (keymapp binding) "KEYMAP" binding))))))))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (insert
       (format "Known bindings for key: %s\n\n" (key-description key))
       (format "%-40s %s" "Map" "Binding\n")
       (s-repeat 40 "-") " " (s-repeat 30 "-") "\n")
      (display-buffer (current-buffer)))))

(provide 'my/find-known-bindings)

;;; my-find-known-bindings.el ends here

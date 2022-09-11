(require 'python)
(require 'font-lock)
(require 'doxygen)

(font-lock-add-keywords 'python-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my-python-insert-block-comment ()
  (interactive)
  (insert "#")
  (newline-and-indent)
  (insert "#")
  (python-indent-line)
  (newline-and-indent)
  (insert "#")
  (python-indent-line)
  (forward-line -1)
  (end-of-line)
  (insert " "))

(defun my-python-newline ()
  (interactive)
  (newline)
  (indent-for-tab-command))

(defun my-string-trim (string)
  (replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" string))

(defun my-python-doxygen-insert-prepped-comment (prefix found)
  "Insert a Doxygen comment block and fill it with some tags to represent the
data contained in FOUND. The comment and its contents are indented with the
value PREFIX. Places point at the end of the first line of the comment block."
  (let ((begin (nth 0 found))
	(end (nth 1 found))
	(info (nth 2 found)))
    (goto-char begin)
    (insert prefix "## ")
    (doxygen-push-mark)
    (when doxygen-insert-summary
      (insert (nth 1 info))
      (doxygen-push-mark))
    (save-excursion
      (insert "\n")
      (let ((args (mapcar 'my-string-trim (nth 2 info))))
	(message "%s %s" args (nth 0 args))
	(when args
	  (if (string-equal "self" (nth 0 args))
	      (setq args (cdr args)))
	  (mapc (function (lambda (a)
                            (insert prefix "# \\param " a " ")
                            (doxygen-push-mark)
                            (insert "\n")))
                args)))
      (when (nth 0 info)
	(insert prefix "# \\return " )
	(doxygen-push-mark)
	(insert "\n"))
      (insert prefix "#\n"))))

(defun my-python-doxygen-insert-empty-comment ()
  "Insert an empty Doxygen comment block. The point is left at the end of the
first line. The comment is indented with PREFIX."
  (indent-according-to-mode)
  (let ((prefix (doxygen-line-prefix)))
    (beginning-of-line)
    (insert prefix "## ")
    (doxygen-push-mark)
    (save-excursion (insert "\n" prefix "#\n"))))

(defun my-python-doxygen-insert-block-comment ()
  "Insert a Doxygen block comment for the next non-blank line."
  (interactive "*")
  (let ((found (doxygen-find-declaration)))
    (if found
	(my-python-doxygen-insert-prepped-comment (progn
						    (goto-char (nth 0 found))
						    (doxygen-line-prefix))
						  found)
      (my-python-doxygen-insert-empty-comment))
    (doxygen-update-marks)))

(defun my-python-mode-hook ()
  (font-lock-mode t)
  (auto-fill-mode 1)
  ;; (show-paren-mode t)
  (local-set-key [(meta control \;)] 'my-python-insert-block-comment)
  (local-set-key [(control c)(control meta f)] 'c-forward-into-nomenclature)
  (local-set-key [(control c)(control meta b)] 'c-backward-into-nomenclature)
  (local-set-key [(control c)(control meta u)] 'my-capitalize-nomenclature)
  (local-set-key [(control c)(control meta d)] 'my-forward-delete-nomenclature)
  (local-set-key [(control c)(meta control \;)]
		 'my-python-doxygen-insert-block-comment)
  ;; (local-set-key [(return)] 'my-python-newline)
  )

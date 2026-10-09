;;; my-functions.el --- useful free functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'popper)

;;;###autoload
(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (when-let* ((filename (if (equal major-mode 'dired-mode)
                            default-directory
                          (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

;;;###autoload
(defun my/display-prefix (arg)
  "Display the value of the raw prefix ARG.
For debugging purposes."
  (interactive "P")
  (message "%s" arg))

;;;###autoload
(defun my/dump-hashtable (hashtable)
  "Show the contents of HASHTABLE."
  (interactive "Xhash table: ")
  (when (hash-table-p hashtable)
    (let ((tmp (get-buffer-create "*dump*")))
      (switch-to-buffer-other-window tmp)
      (erase-buffer)
      (maphash (lambda (key value)
                 (insert key " -> " value "\n")) hashtable))))

;;;###autoload
(defun my/goto-mark ()
  "Move back to mark without enabling transient mode."
  (interactive)
  (set-mark-command 4))

;;;###autoload
(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun my/is-valid-directory (dir)
  "Check if DIR is valid, returning it if so or nil if not.
Note that `file-directory-p' returns t if the (string) length of DIR is
zero (0), so we detect that and report that as false."
  (and (> (length dir) 0)
       (file-directory-p dir)
       dir))

;;;###autoload
(defun my/mark-line (&optional arg)
  "Blah blah ARG blah."
  (interactive "p")
  (unless mark-active
    (beginning-of-line)
    (push-mark)
    (setq mark-active t))
  (forward-line arg))

;;;###autoload
(defun my/matching-paren ()
  "When point is on a paren-type character, jump to its twin."
  (interactive)
  (cond ((looking-at "[[({]")
	 (forward-sexp 1)
	 (forward-char -1))
	((looking-at "[]})]")
	 (forward-char 1)
	 (forward-sexp -1))
	(t
	 nil)))

;;;###autoload
(defun my/reload-buffer ()
  "Reload the current buffer from disk.
Checks to see if buffer needs saving, aborting the reload if changes not saved.
Useful for Elisp content or changes to mode settings."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and (not buffer-read-only)
               filename
               (or (not (buffer-modified-p))
                   (and (string= "yes" (read-answer "Save changes? "
                                                    '(("yes" ?y "save buffer")
                                                      ("quit" ?q "abort"))))
                        (progn (save-buffer) t))))
      (find-alternate-file filename)
      (message "Reloaded."))))

;;;###autoload
(defun my/remove-all-text-properties ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

;;;###autoload
(defun my/set-mark-no-activate ()
  "Push `point' to `mark-ring' but does not activate the region."
  (interactive)
  (push-mark (point) t nil))

;;;###autoload
(defun my/sort-lines-by-integer-key (pattern &optional direction)
  "Sort lines by an integer value that is found via PATTERN in a line.
The sort is in increasing numerical order if DIRECTION is nil; otherwise
it is in descending order."
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr direction
                   #'forward-line       ; NEXTRECFUN
                   #'end-of-line        ; ENDRECFUN
                   (lambda ()                ; STARTKEYFUN -- returns numeric key
                     (when (looking-at pattern)
                       (string-to-number (match-string 0))))
                   nil                  ; ENDKEYFUN
                   nil)))))             ; PREDICATE

;;;###autoload
(defun my/sort-lines-by-leading-integer (arg)
  "Sort lines in current region by extracting integer values from start of line.
If ARG is not nil, sort in descending order.
Nothing fancy about parsing, it just matches any number at the beginning of
the line, ignoring any whitespace characters. If that fails, then the sort
will treat the whole line as a value to compare against."
  (interactive "P")
  (my/sort-lines-by-integer-key "^\\s *[0-9]+" arg))

;;;###autoload
(defun my/trusted-content-p (original-response)
  "Advice for `trusted-content-p' to trust the `*scratch*' buffer.
Honors ORIGINAL-RESPONSE when not nil and then checks the buffer's name
if it is `*scratch*'. This is a loosening of security but the risk is
very small for me, and it remove the obnoxious message at startup about
the buffer having untrusted content."
  (or original-response
      (buffer-name "*scratch*")))

(defun my/launch-ssh-emacs (host)
  "Launch X11 Emacs on remote HOST."
  (interactive)
  (message "Starting Emacs on %s..." host)
  (start-process
   (concat "ssh-" host)
   (concat " *ssh-" host "*")
   "/usr/bin/ssh"
   "-Y"
   host
   "exec /opt/homebrew/bin/emacs"))

(provide 'my-functions)

;;; my-functions.el ends here.

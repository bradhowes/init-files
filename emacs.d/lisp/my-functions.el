;;; my-functions.el --- useful free functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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


(provide 'my-functions)

;;; my-functions.el ends here.

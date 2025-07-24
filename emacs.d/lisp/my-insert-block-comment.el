;;; package -- my-insert-block-comment -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my/insert-block-comment (newlineAndIndent commentBegin commentMiddle commentEnd)
  "Insert comment block in code.
NEWLINEANDINDENT -- method to call to create a newline and indent it
COMMENTBEGIN -- the string to insert to start the comment
COMMENTMIDDLE -- the string to insert for each line insdie the comment
COMMENTEND -- the string to insert to close the comment block"
  (interactive)
  (insert commentBegin)
  (when commentEnd
    (funcall newlineAndIndent)
    (when commentMiddle
      (insert commentMiddle)
      (funcall newlineAndIndent))
    (insert commentEnd)
    (forward-line -1))
  (end-of-line)
  (insert " "))

(provide 'my-insert-block-comment)
;;; my-insert-block-comment.el ends here

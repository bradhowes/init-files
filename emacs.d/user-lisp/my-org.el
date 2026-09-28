;;; my-org.el --- custom org routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'org)
(require 'tempo)

;;;###autoload
(defun tempo-template-my/org-emacs-lisp-source (&optional _)
  "Define empty function to satisfy flymake/byte-compile (ARG is ignored).")

(tempo-define-template "my/org-emacs-lisp-source" '("#+begin_src emacs-lisp" & r % "#+end_src")
                       "<m"
                       "Insert an Emacs Lisp source block in an org document.")

;;;###autoload
(defun my/org-emacs-lisp-source-with-indent ()
  "Execute `my/org-emacs-lisp-source' and then indent block."
  (interactive)
  (tempo-template-my/org-emacs-lisp-source)
  (forward-line -1)
  (org-cycle))

;;;###autoload
(defun my/org-filter-buffer-substring (start end delete)
  "Custom filter on buffer text from START to END.
When DELETE is t, delete the contents from the range.
Otherwise, removes all properties from a span in a buffer.
Useful when copying code into Org blocks so that the copy does not contain any
artifacts such as indentation bars."
  (if delete
      (delete-and-extract-region start end)
    (buffer-substring-no-properties start end)))

(provide 'my-org)

;;; my-org.el ends here.

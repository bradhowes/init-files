;;; package -- my-org -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Org mode settings
;;; Code:

(use-package org
  :commands (org-store-link)
  :config
  (defvar my/org-key-map (make-sparse-keymap)
    "Keymap for my org mode access.")
  (keymap-set my/org-key-map "a" #'org-agenda)
  (keymap-set my/org-key-map "c" #'org-capture)
  (keymap-set my/org-key-map "l" #'org-store-link)
  :bind-keymap ("H-o" . my/org-key-map))

(use-package tempo
  :commands (tempo-define-template))

(defun tempo-template-my/org-emacs-lisp-source ()
  "Define empty function to satisfy flymake/byte-compile.")

(tempo-define-template "my/org-emacs-lisp-source" '("#+begin_src emacs-lisp" & r % "#+end_src")
                       "<m"
                       "Insert an Emacs Lisp source block in an org document.")

(defun my/org-emacs-lisp-source-with-indent ()
  "Execute `my/org-emacs-lisp-source' and then indent block."
  (interactive)
  (tempo-template-my/org-emacs-lisp-source)
  (forward-line -1)
  (org-cycle))

(defun my/filter-buffer-substring (start end delete)
  "Custom filter on buffer text from START to END.
When DELETE is t, delte the contents in the range.
Otherwise, remove all properties from a span in a buffer.
Useful when copying code into Org blocks so that the copy does not contain any
artifacts such as indentation bars."
  (if delete
      (delete-and-extract-region start end)
    (buffer-substring-no-properties start end)))

;; NOTE: this is setting a global variable, but we should really just do this when operating in an org buffer.
(setq filter-buffer-substring-function #'my/filter-buffer-substring)

(provide 'my-org)
;;; my-org.el ends here

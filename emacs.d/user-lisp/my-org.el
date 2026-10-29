;;; my-org.el --- custom org routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-env)
(require 'org)
(require 'org-capture)
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

(defun my/org-mode-hook ()
  "Org mode setup."
  (local-set-key "H-<up>" #'org-priority-up)
  (local-set-key "H-<down>" #'org-priority-down)
  (local-set-key "H-<right>" #'org-shiftmetaright))

;;;###autoload
(defun my/org-initialize ()
  "Custom org initialization."
  (let ((org-dir (file-name-as-directory (file-name-concat (my/emacs.d) "org"))))
    (make-directory org-dir t)
    (setopt org-directory org-dir
            org-agenda-files `(,org-dir)
            org-capture-templates (list
                                   `("g" "General To-Do"
                                     entry
                                     (file+headline ,(file-name-concat org-dir "todos.org") "Tasks")
                                     "* TODO [#B] %?\n:Created: %T\n "
                                     :empty-lines 0)
                                    `("j" "Work Log Entry"
                                      entry
                                      (file+olp+datetree ,(file-name-concat org-dir "work-log.org"))
                                      "* %?"
                                      :empty-lines 0)
                                    `("m" "Meeting"
                                      entry
                                      (file+olp+datetree ,(file-name-concat org-dir "meetings.org"))
                                      "* %? :meeting:%^g
:Created: %T
** Attendees
*** \n
 Notes
** Action Items
*** TODO [#A] "
                                      :tree-type week
                                      :clock-in t
                                      :clock-resume t
                                      :empty-lines 0)
                                    `("n" "Note"
                                      entry (file+headline ,(file-name-concat org-dir "notes.org") "Random Notes")
                                      "** %?"
                                      :empty-lines 0)
                                    )
            org-hide-emphasis-markers t
            org-log-done 'time
            org-return-follows-link t
            org-todo-keywords '((sequence "TODO(t)"
                                          "PLANNING(p)"
                                          "IN-PROGRESS(i@/!)"
                                          "VERIFYING(v!)"
                                          "BLOCKED(b@)"
                                          "|"
                                          "DONE(d!)"
                                          "OBE(o@!)"
                                          "WONT-DO(w@/!)"))
            org-todo-keyword-faces '(("TODO" . (:foreground "GoldenRod" :weight bold))
                                     ("PLANNING" . (:foreground "DeepPink" :weight bold))
                                     ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
                                     ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
                                     ("BLOCKED" . (:foreground "Red" :weight bold))
                                     ("DONE" . (:foreground "LimeGreen" :weight bold))
                                     ("OBE" . (:foreground "LimeGreen" :weight bold))
                                     ("WONT-DO" . (:foreground "LimeGreen" :weight bold)))))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'my/org-mode-hook)
  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'my-org)

;;; my-org.el ends here.

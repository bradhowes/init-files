;;; package -- my-markdown-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'flyspell)

(defconst my-markdown-skip-spellcheck-properties
  (list 'markdown-code-face 'markdown-url-face 'markdown-inline-code-face)
  "List of text properties to ignore when spell checking.")

(defun my-markdown-is-spellcheckable (p)
  "Determine if element P is spellcheckable."
  (if (consp p)
      (eq (seq-count 'my-markdown-is-spellcheckable p) (seq-length p))
    (or (null p)
        (null (memq p my-markdown-skip-spellcheck-properties)))))

(defun my-markdown-generic-textmode-verify ()
  "Used for `flyspell-generic-check-word-predicate' in text modes."
  ;; (point) is next char after the word. Must check one char before.
  (let* ((f (get-text-property (- (point) 1) 'face))
         (s (my-markdown-is-spellcheckable f)))
    s))

(defun brh-make-link ()
  "Make a link."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (pos1 (car bounds))
         (pos2 (cdr bounds))
         (tag (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert "[" tag "](" tag ")")))

(defun my-markdown-mode-hook ()
  "Custom Markdown mode."
  ;; (flyspell-mode t)
  ;; (setq flyspell-generic-check-word-predicate 'my-markdown-generic-textmode-verify)
  (local-set-key [(f8)] 'brh-make-link))

(provide 'my-markdown)
;;; my-markdown.el ends here

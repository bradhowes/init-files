;;; package -- my-markdown-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)

(use-package impatient-mode
  :vc (:fetcher github :repo "skeeto/impatient-mode")
  :commands (imp-set-user-filter impatient-mode)
  :defines (imp-user-filter)
  :config
  (setq-default imp-user-filter #'my/markdown-to-html))

(defconst my/markdown-skip-spellcheck-properties
  (list 'markdown-code-face 'markdown-url-face 'markdown-inline-code-face)
  "List of text properties to ignore when spell checking.")

(defun my/markdown-is-spellcheckable (p)
  "Determine if element P is spellcheckable."
  (if (consp p)
      (eq (seq-count 'my/markdown-is-spellcheckable p) (seq-length p))
    (or (null p)
        (null (memq p my/markdown-skip-spellcheck-properties)))))

(defun my/markdown-generic-textmode-verify ()
  "Used for `flyspell-generic-check-word-predicate' in text modes."
  ;; (point) is next char after the word. Must check one char before.
  (let* ((f (get-text-property (- (point) 1) 'face))
         (s (my/markdown-is-spellcheckable f)))
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

(defun my/markdown-mode-hook ()
  "Custom Markdown mode."
  ;; (flyspell-mode t)
  ;; (setq flyspell-generic-check-word-predicate 'my/markdown-generic-textmode-verify)
  (local-set-key [(f8)] #'brh-make-link))

(defun my/point-min-after-front-matter ()
  "Skip any front matter in current buffer and return POINT.
Front matter is defined as a set of lines in the buffer
that start and end with lines containing only `---'."
  (goto-char (point-min))
  (when (looking-at "^---$")
    (forward-line 1)
    (while (not (looking-at "^---$"))
      (forward-line 1))
    (forward-line 1))
  (point))

(defun my/markdown-to-html (buffer)
  "Generate HTML from Markdown content in BUFFER.
Will strip away any `denote' front matter that
starts with `---' alone on the first line of the buffer and
ends with the same `---' on its own line."
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html>
<html>
 <title>Impatient Markdown</title>
 <xmp theme=\"united\" style=\"display:none;\">
 %s
 </xmp>
 <script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\">
 </script>
</html>" (buffer-substring-no-properties (save-excursion
                                           (my/point-min-after-front-matter))
                                         (point-max))))
         (current-buffer)))

(defun my/markdown-mode ()
  "Customization hook for `markdown-mode'."
  (impatient-mode)
  (imp-set-user-filter #'my/markdown-to-html))

(provide 'my-markdown)
;;; my-markdown.el ends here

;;; package -- my-markdown-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'markdown-mode)

(use-package impatient-mode
  :ensure t
  :config
  (setq-default imp-user-filter #'my/markdown-to-html))

(require 'impatient-mode)

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

(defun my/escape-last-word ()
  "Escape '_' and '*' characters in the word before point."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (re-search-forward "\\s " nil nil -1)
      (format-replace-strings '(("_" . "\\_") ("*" . "\\*")) nil (point) pos))))

(defun my/codify-last-word ()
  "Escape wrap the last word with backquotes."
  (interactive)
  (save-excursion
    (re-search-forward "\\s " nil nil -1)
    (when (looking-at "\\s ")
      (forward-char 1))
    (insert-char ?`))
  (insert-char ?`))

(defun my/make-link ()
  "Make a link."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (pos1 (car bounds))
         (pos2 (cdr bounds))
         (tag (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert "[" tag "](" tag ")")))

(defun my/markdown-mode-hook ()
  "Customization hook for `markdown-mode'."
  (impatient-mode)
  (imp-set-user-filter #'my/markdown-to-html)
  (keymap-local-set "C-c *" #'my/escape-last-word)
  (keymap-local-set "C-c `" #'my/codify-last-word)
  (keymap-local-set "C-c <space>" #'my/make-link))

(defun my/fixup-code-region ()
  "Get the region to work on as a CONS cell of start, end.
If region is active, return that. If point is currently
in a code block, return the start and end of the block.
Otherwise, return the current point and max point."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (cons (use-region-beginning) (use-region-end))
      (if-let ((block (markdown-code-block-at-pos (point))))
          (cons (car block) (cadr block))
        (cons (point) (point-max))))))

(defun my/fixup-log-paste ()
  "Fix-up log lines that were copied from terminal.
If no region is active, see if point is in a markdown code block, and if so
use the block limits for BEG and END values. Otherwise, work on lines from
current line to the end of buffer."
  (interactive)
  (save-excursion
    (let* ((re "\\(\\\\\n \\)\\|\\( +$\\)\\|\\(^ +\\)")
           (region (my/fixup-code-region))
           (beg (car region))
           (end (cdr region)))
      (message "region: %s" region)
      (goto-char beg)
      (beginning-of-line 1)
      (while (re-search-forward re end t)
        (replace-match "" nil nil)))))

(defun my/format-fixdropcopy ()
  "Format quickfix dropcopy messages pasted from TCS logs.
Simply replaces all runs of '^A' (two characters) with a
newline and 2 spaces."
  (interactive)
  (save-excursion
    (let* ((re "\\^A\\|")
           (region (my/fixup-code-region))
           (beg (car region))
           (end (cdr region)))
      (goto-char beg)
      (while (re-search-forward re end t)
        (replace-match "\n  " nil nil)))))

(defun my/format-xml ()
  "Format XML pasted into code block.
Simply replaces all spaces between attributes in a clause with a
newline and 2 spaces.

Example:

  <foo a=\"1\" b=\"2\"/>

transforms into

  <foo
    a=\"1\"
    b=\"2\"
  />"
  (interactive)
  (save-excursion
    (let* ((re "\\( [a-zA-Z0-9_]+=\\)\\|\\(/>\\)")
           (region (my/fixup-code-region))
           (beg (car region))
           (end (cdr region)))
      (goto-char beg)
      (while (re-search-forward re end t)
        (goto-char (match-beginning 0))
        (insert "\n")
        (if (match-beginning 2)
            (forward-char 3)            ; match />
          (insert " "))                 ; match before attribute
        (when end
          (setq end (+ 2 end)))
        (goto-char (match-end 0))))))

(defun my/sort-fixdropcopy ()
  "Sort FIX dropcopy lines in region BEG to END."
  (interactive)
  (my/sort-lines-by-integer-key "^\\s *[0-9]+="))

(provide 'my-markdown-mode)
;;; my-markdown-mode.el ends here

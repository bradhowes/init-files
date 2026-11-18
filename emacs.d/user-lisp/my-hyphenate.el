;;; my-hyphenate.el --- Hyphenating functions by way of pyphen -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:
;;;
;;; ## hyphenate shell script at `~/src/hyphenator/hyphenate'
;;; #!/bin/bash
;;; # Trampoline to enter Python and execute the hyphenate.py script.
;;; root=$(dirname "${BASH_SOURCE[0]}")
;;;
;;; lang="${1}"
;;; min_len="${2}"
;;;
;;; # Create venv + pyphen
;;; if [[ ! -d "${root}/venv" ]]; then
;;;   python3 -m venv venv
;;;   # shellcheck disable=SC1091
;;;   source "${root}/venv/bin/activate"
;;;   pip install -U pip pyphen
;;; else
;;;   # shellcheck disable=SC1091
;;;   source "${root}/venv/bin/activate"
;;; fi
;;;
;;; exec python3 "${root}/hyphenate.py" "${lang}" "${min_len}" 2>/dev/null
;;; # END
;;;
;;; ## hyphenate.py Python script at `~/src/hyphenator/hyphenate.py'
;;;
;;; #!/Users/howes/src/hyphenator/venv/bin/python
;;;
;;; # Hyphenate the text from stdin.
;;; #
;;; # - first parameter is the source language (eg. 'fr')
;;; # - the text to hyphenate is taken from stdin
;;; # - the hyphenated text is written to stdout
;;;
;;; import pyphen
;;; import re
;;; import sys
;;;
;;;
;;; def hyphenate_text(text: str, lang: str, min_len: int) -> str:
;;;     dic = pyphen.Pyphen(lang=lang)
;;;     pattern = rf"\b[A-Za-z]{{{min_len},}}\b"
;;;     hyphen = "\u00ad"
;;;     hyphen = "-"
;;;
;;;     def repl(match: re.Match) -> str:
;;;         return dic.inserted(match.group(0), hyphen)
;;;
;;;     print(re.sub(pattern, repl, text))
;;;
;;;
;;; if __name__ == "__main__":
;;;     hyphenate_text(sys.stdin.read().strip(), sys.argv[1], sys.argv[2])
;;;     sys.exit(0)
;;; # END

(defcustom my/hyphenate-lang "en_US"
  "Language locale passed to Python's pyphen (e.g., \"en_US\", \"fr\", \"de_DE\")."
  :type '(string)
  :group 'my/customizations)

(defcustom my/hyphenate-min-word-len 6
  "Minimum word length required before considering hyphenation."
  :type '(integer)
  :group 'my/customizations)

(defun my/hyphenate--insert-soft-hyphens ()
  "Insert Unicode soft hyphens (U+00AD) into words via pyphen."
  (let* ((cmd "~/src/hyphenator/hyphenate")
         (delete t)
         (buffer t)
         (display nil))
    (call-process-region (point-min) (point-max)
                         cmd
                         delete
                         buffer
                         display
                         my/hyphenate-lang
                         (format "%d" my/hyphenate-min-word-len))))

;;;###autoload
(define-minor-mode my/hyphenate-fill-mode
  "Minor mode to enable Pyphen hyphenation.
Affects `fill-paragraph' and `fill-region' operations."
  :global nil
  :lighter " HyF"
  (modify-category-entry ?\u00ad ?|)
  (set (make-local-variable fill-region-as-paragraph-function)
       (if my/hyphenate-fill-mode
           #'my/hyphenate--fill-region-as-paragraph-with-hyphenation
         #'fill-region-as-paragraph-default)))

(defun my/hyphenate--undo-past-hyphenation ()
  "Remove hyphens at the end of lines."
  (goto-char (point-min))
  (while (re-search-forward "-\n" nil t)
    (replace-match "" t)))

(defun my/hyphenate--convert-soft-hyphens ()
  "Convert soft-hyphens into hard ones at end of lines.
Removes all others."
  (goto-char (point-min))
  (while (re-search-forward "\n?\u00ad\n?" nil t)
    (if (> (- (match-end 0) (match-beginning 0)) 1)
        (replace-match "-\n" t)
      (replace-match "" t))))

(defun my/hyphenate--fill-region-as-paragraph-with-hyphenation (from to &optional justify nosqueeze squeeze-after)
  "Hyphenate region FROM to TO when filling.
See documentation for `fill-region-as-paragraph-default' for documentation on
the args JUSTIFY, NOSQUEEZE, SQUEEZE-AFTER."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (my/hyphenate--undo-past-hyphenation)
      (my/hyphenate--insert-soft-hyphens)
      (let ((prefix (fill-region-as-paragraph-default (point-min) (point-max) justify nosqueeze squeeze-after)))
        (my/hyphenate--convert-soft-hyphens)
        prefix))))


(provide 'my-hyphenate)

;;; my-hyphenate.el ends here.

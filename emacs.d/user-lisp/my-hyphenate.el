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

(defconst my/hyphenate-soft-hyphen ?\u00ad
  "The soft-hyphen character found in UTF8.
If changed, similar change must be made in the Python code.")

(defcustom my/hyphenate-lang "en_US"
  "Language locale passed to Python's pyphen (e.g., \"en_US\", \"fr\", \"de_DE\")."
  :type '(string)
  :group 'my/customizations)

(defcustom my/hyphenate-min-word-len 5
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

(defun my/hyphenate--undo-past-hyphenation ()
  "Remove any hard hyphens at the end of lines.
This is done before attempting a new hyphenated fill."
  (goto-char (point-min))
  (while (re-search-forward "-\n" nil t)
    (replace-match "" t)))

(defconst my/hyphenate--eol-soft-hyphen-re (concat "\n?" (list my/hyphenate-soft-hyphen) "\n?")
  "A regular-expression that matches soft-hyphens at the end of a line.")

(defun my/hyphenate--convert-soft-hyphens ()
  "Convert end-of-line soft-hyphens into hard ones, remove the rest.
This is done after a hyphenated fill."
  (goto-char (point-min))
  (while (re-search-forward my/hyphenate--eol-soft-hyphen-re nil t)
    (if (> (- (match-end 0) (match-beginning 0)) 1)
        (replace-match "-\n" t)
      (replace-match "" t))))

(defun my/hyphenate--fill-paragraph (from to &rest _)
  "Perform a hyphenated fill in region FROM to TO.
This is installed in `fill-region-as-paragraph-function' by
`my/hyphenate-fill-paragraph' to do the hyphenation work."
  (save-excursion
    (save-restriction
      (goto-char from)
      (skip-chars-forward " \t\n")
      (narrow-to-region (point) to)
      (my/hyphenate--undo-past-hyphenation)
      (my/hyphenate--insert-soft-hyphens)
      ;; TODO: we cannot rely on this to do the filling. It seems that it counts the soft hyphens, so
      ;; the filling algorithm miscalculates when the line is filled.
      (fill-region-as-paragraph-default (point-min) (point-max))
      (my/hyphenate--convert-soft-hyphens))))

(defun my/hyphenate--move-to-break-point (linebeg)
  "Alternative to `fill-move-to-break-point' that hyphenates text.
Uses text between LINEBEG and (point)."
  (message "move-to-break-point: %d" linebeg))

;;;###autoload
(defun my/hyphenate-fill-paragraph ()
  "Perform a hyphenated fill on the current paragraph.
Relies on `fill-paragraph' to do the setup, and then
`my/hyphenate-fill-paragraph' for the hyphenated filling."
  (interactive)
  ;; Enable splitting at soft-hyphen positions.
  (let ((prev-category (aref (category-table) my/hyphenate-soft-hyphen))
        (fill-region-as-paragraph-function #'my/hyphenate--fill-paragraph))
    (modify-category-entry my/hyphenate-soft-hyphen ?|)
    (unwind-protect
        (fill-paragraph)
      (aset (category-table) my/hyphenate-soft-hyphen prev-category))))

(provide 'my-hyphenate)

;;; my-hyphenate.el ends here.

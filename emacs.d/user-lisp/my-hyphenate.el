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

(defun my/hyphenate--insert-soft-hyphens (beg end)
  "Insert Unicode soft hyphens (U+00AD) into words between BEG and END via pyphen."
  (let* ((cmd "~/src/hyphenator/hyphenate")
         (delete t)
         (buffer t)
         (display nil))
    (call-process-region beg end
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
  (modify-category-entry ?\u00ad ?|))

(defun my/hyphenate--fill-region (orig-fun from to &rest args)
  "Hyphenate region FROM to TO when filling.
Invokes ORIG-FUN with ARGS."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (message "1 from: %d to: %d" (point-min) (point-max))
      (if (not my/hyphenate-fill-mode)
          (apply orig-fun (append (list (point-min) (point-max)) args))
        (goto-char (point-min))
        (while (re-search-forward "-\n" nil t)
          (replace-match "" t))
        (message "2 from: %d to: %d" (point-min) (point-max))
        (my/hyphenate--insert-soft-hyphens (point-min) (point-max))
        (message "3 from: %d to: %d" (point-min) (point-max))
        (apply orig-fun (append (list (point-min) (point-max)) args))
        (message "4 from: %d to: %d" (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "\n?\u00ad\n?" nil t)
          (if (> (- (match-end 0) (match-beginning 0)) 1)
              (replace-match "-\n" t)
            (replace-match "" t)))
      ))))

(advice-add 'fill-region-as-paragraph :around #'my/hyphenate--fill-region)

(provide 'my-hyphenate)

;;; my-hyphenate.el ends here.

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
         (display t))
    (call-process-region beg end
                         cmd
                         delete
                         buffer
                         display
                         my/hyphenate-lang
                         (format "%d" my/hyphenate-min-word-len))))

(defun my/hyphenate--fill-region (from to &optional justify nosqueeze to-eop)
  "Hyphenate region from FROM to TO then fill.
The JUSTIFY, NOSQUEEZE, and TO-EOP optional arguments are the same as for
`fill-region' -- they are not used by the hyphenate process."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      ;; 1. Populate valid break points using soft hyphens
      (my/hyphenate--insert-soft-hyphens (point-min) (point-max))
      ;; 2. Run Emacs's native fill-region (converts broken soft hyphens to '-')
      (fill-region (point-min) (point-max) justify nosqueeze to-eop)
      ;; 3. Clean up remaining unused soft hyphens
      (goto-char (point-min))
      (while (search-forward "\u00ad" nil t)
        (replace-match "" t t)))))

;;;###autoload
(define-minor-mode my/hyphenate-fill-mode
  "Minor mode to enable Pyphen hyphenation.
Affects `fill-paragraph' and `fill-region' operations."
  :global nil
  :lighter " HyF"
  (if my/hyphenate-fill-mode
      (advice-add 'fill-region :around #'my/hyphenate--fill-region)
    (advice-remove 'fill-region #'my/hyphenate--fill-region)))

;;; Today’s focus is hyphenation. Hyphenation is not a product of digital typesetting; as early as the Middle Ages in
;;; Latin manuscripts, hyphens were used for word division. Scribes would often use a short dash to split long words
;;; when they needed to move to the next line, thereby saving space. At that time, the rules for hyphenation were quite
;;; casual, with no fixed standards.
;;;
;;; With the invention of the Gutenberg printing press (circa 1440), printed text became the primary means of book
;;; production, and typesetting rules became more standardized. Since printed text needed to be arranged neatly on
;;; fixed-size pages, the demand for word division became more prominent. Typesetters would manually hyphenate words to
;;; ensure that each line was neatly aligned without leaving too much blank space. During this period, hyphenation
;;; became more standardized, but the rules largely depended on the typesetter’s experience and language habits. As
;;; printing technology advanced into the 16th and 17th centuries, printers across Europe began to establish basic rules
;;; for hyphenation. For example, in English, hyphenation methods gradually formed based on syllables, while in German,
;;; more emphasis was placed on dividing words by roots and affixes.
;;;
;;; By the 19th century, hyphenation rules were increasingly standardized by linguists, dictionary compilers, and
;;; typesetting industry norms. For example, the Oxford English Dictionary (OED) and some spelling manuals began
;;; specifying where words could be divided. Typesetters began relying on written standards for hyphenation. Hyphenation
;;; was frequently used in the typesetting of books, newspapers, and periodicals to handle line breaks.
;;;
;;; In the 20th century, with the rise of digital typesetting, hyphenation entered a new phase in the typesetting and
;;; printing industry. Electronic typesetting replaced manual typesetting, and hyphenation algorithms became a hot topic
;;; in the field of digital typesetting. For example, in the TeX typesetting system developed by Donald Knuth, automated
;;; hyphenation began. Later, Frank Liang introduced concepts such as packed Trie and Hyphenation patterns, optimizing
;;; hyphenation algorithms to improve their efficiency and accuracy. We will detail the development of hyphenation
;;; algorithms in the “Algorithm Section.”


(provide 'my-hyphenate)

;;; my-hyphenate.el ends here.

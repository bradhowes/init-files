;;; package -- my-cmake-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cmake-mode)
(require 'font-lock)

(font-lock-add-keywords 'cmake-mode '(("[][(){}]" . font-lock-brace-face)))

;; Macro to lowercase CMake commands and remove spaces betwee the () and arguments.
;;
(fset 'my/cmake-reformat-lowercase
   [?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p ?\C-m ?\\ ?\( ?\[ ?A ?- ?Z ?0 ?- ?9 ?_ ?\] ?+ ?\\ ?\) ?\( ?  ?\\ ?\( ?\[ ?^ ?\\ ?\) ?\] ?* ?\\ ?\) ?  ?\) ?\C-m ?\\ ?, ?\( ?d ?o ?w ?n ?c ?a ?s ?e ?  ?\\ ?1 ?\) ?\( ?\\ ?2 ?\) ?\C-m])

(defun my/cmake-reformat ()
  "Reformat file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([A-Z0-9_]+\\)( \\([^\)]*\\))" nil t)
      (let ((args (match-string 2)))
        (while (eq (elt args (- (length args) 1)) ?\ )
          (setq args (substring args 0 (- (length args) 1))))
        (replace-match (concat (downcase (match-string 1)) "(" args ")") t t)
        ))

    (goto-char (point-min))
    (while (re-search-forward "^[ 	]*
#[ 	]*
# " nil t)
      (replace-match "
# " t t)
      (fill-paragraph))
    (goto-char (point-min))
    (indent-region (point-min) (point-max))
    ))

(define-skeleton my/cmake-header
  "Insert header for file."
  (read-string "Name: "
               (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
  "# -*- Mode: CMake -*-\n"
  "#\n"
  "# CMake build file for the " str " " _ "algorithm\n"
  "#\n"
  "\n"
  "# Production specification for the " str " " _ "algorithm\n"
  "#"
  )

(define-abbrev cmake-mode-abbrev-table "hh" "" 'my/cmake-header)

(defun my/cmake-mode-hook ()
  "CMake mode hook."
  (font-lock-mode t)
  (auto-fill-mode)
  (setq tab-width 4)
  (local-set-key [(f3)] 'my/cmake-reformat-lowercase)
  (show-paren-mode t))

(provide 'my-cmake-mode)
;;; my-cmake-mode.el ends here

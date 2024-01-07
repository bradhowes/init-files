;;; package -- my/c++-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cc-mode)
(require 'doxygen)
(require 'my-c-mode-common)
(require 'my-insert-block-comment)

(define-skeleton my/c++-class-skeleton
  "Insert a C++ class skeleton."
  (read-string "Name: "
	       (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name))))
  "class " str " " _ \n
  "{\npublic:\n\n"
  "private:\n};\n")

(define-abbrev c++-mode-abbrev-table "cc" "" 'my/c++-class-skeleton)

(define-skeleton my/c++-copyright-skeleton
  "Insert a C++ copyright comment."
  nil
  "// (C) Copyright 2022, 2023, 2024 Brad Howes. All rights reserved.\n"
  "//\n\n"
)

(define-abbrev c++-mode-abbrev-table "cr" "" 'my/c++-copyright-skeleton)

(defun my/c++-include-tag (&optional namespace)
  "Insert an include statement.
If NAMESPACE is present, so something."
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (name (file-name-sans-extension file))
	 (tag (concat name "_" (file-name-extension file))))
    (upcase (if (and namespace (> (length namespace) 0))
		(concat namespace "_" tag)
	      tag))))

(defun my/c++-class-name ()
  "Obtain a C++ class name from the buffer's file name."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun my/c++-source-header (&optional namespace)
  "Insert the basics of a C++ source header.
Setup a namespace with NAMESPACE name if non-nil."
  (interactive)
  (let ((name (my/c++-class-name)))
    (insert "// -*= C++ -*-\n//\n")
    (copyright)
    (insert "\n//\n\n#include \"" name ".h\"\n\n")
    (when (and namespace (> (length namespace) 0))
      (insert "using namespace " namespace ";\n\n" ))
    (insert name "::" name "(")
    (doxygen-push-marker)
    (insert ")")
    (newline-and-indent)
    (insert "{")
    (indent-according-to-mode)
    (newline-and-indent)
    (doxygen-push-marker)
    (newline-and-indent)
    (insert "}")
    (indent-according-to-mode)
    (newline-and-indent)
    (newline-and-indent)
    (insert name "::~" name "()")
    (newline-and-indent)
    (insert "{")
    (indent-according-to-mode)
    (newline-and-indent)
    (doxygen-push-marker)
    (newline-and-indent)
    (insert "}")
    (indent-according-to-mode)
    (newline-and-indent)
    (newline-and-indent)
    (doxygen-update-markers)))

(defun my/c++-new-class (name)
  "Insert a new C++ class template with NAME."
  (interactive "MName: ")
  (when (> (length name) 0)
    (insert "class " name)
    (doxygen-push-marker)
    (newline-and-indent)
    (insert "{")
    (newline-and-indent)
    (insert "public:")
    (indent-according-to-mode)
    (newline-and-indent)
    (insert name "(")
    (doxygen-push-marker)
    (insert ");")
    (newline-and-indent)
    (insert "~" name "();")
    (newline-and-indent)
    (doxygen-push-marker)
    (insert "\nprivate:")
    (indent-according-to-mode)
    (end-of-line)
    (doxygen-push-marker)
    (newline-and-indent)
    (insert "};")
    (indent-according-to-mode)
    (newline-and-indent)
    (doxygen-update-markers)))

(defun my/c++-full-include-header (namespace)
  "Insert C++ header with NAMESPACE namespace."
  (interactive "MNamespace: ")
  (let ((tag (my/c++-include-tag namespace)))
    (insert "#ifndef " tag " // -*- C++ -*-")
    (comment-dwim nil)
    (end-of-line)
    (insert "\n#define " tag "\n//\n")
    (copyright)
    (insert "\n//\n\n")
    (when (> (length namespace) 0)
      (insert "namespace " namespace " {\n\n"))
    (my/c++-new-class (my/c++-class-name))
    (goto-char (point-max))
    (when (> (length namespace) 0)
      (insert "\n} // end namespace " namespace))
    (insert "\n#endif\n")))

(defun my/c++-insert-header (namespace)
  "Insert new header for NAMESPACE."
  (interactive "MNamespace: ")
  (goto-char (point-min))
  (let ((extension (file-name-extension (buffer-file-name))))
    (cond ((string-match "[Hh]" extension)
	   (my/c++-full-include-header namespace))
	  (t
	   (my/c++-source-header namespace)))
    (goto-char (nth 0 doxygen-marks))))

(defun my/c++-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (if (assq 'topmost-intro (c-guess-basic-syntax))
      (doxygen-insert-block-comment)
    (my/insert-block-comment 'newline-and-indent "//" "//" "//")))

(defun my/c++-vsemi-status-unknown-p()
  "Unknown what this does."
  nil)

(defun my/c++-at-vsemi-p (&optional pos)
  "Who knows what POS this is."
  (unless pos (setq pos (point)))
  (and (> pos (+ (point-min) 8))
       (string-equal (buffer-substring-no-properties (- pos 8) pos)
		     "Q_OBJECT")))

;; Customize topmost-intro indentation so that all elements inside of a namespace (innamespace) have no
;; indentation unless the element itself is 'namespace'.
;;
(eval-when-compile
  (defvar c-syntactic-context))

(defun my/c++-indent-topmost-intro ()
  "Control the indentation for the LANGELEM."
  (when (assq 'innamespace c-syntactic-context)
    (save-excursion
      (back-to-indentation)
      (if (looking-at "namespace")
          nil
        [0]))))

(defun my/c++-kill-copyright ()
  "Remove the copyright message."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "(C) Copyright" nil t)
    (goto-char (match-beginning 0))
    (beginning-of-line 0)
    (if (looking-at "/\\*")
        (progn
          (comment-kill 1)
          (forward-line -1)
          (when (looking-at "^\\s_*$")
            (kill-line 1)))
      (while (looking-at "^//")
        (kill-line 1)))
    (when (looking-at "^\\s_*$")
      (kill-line 1))))

(defun my/c++-cleanup ()
  "Perform various cleanups."
  (interactive)
  (save-excursion

    ;; Remove any previous copyright comment
    ;;
    (my/c++-kill-copyright)

    ;; Detect Doxygen tags (param, return) and make sure that they have a blank line before them so that
    ;; comment refill works properly. Also, if the tag starts with an '@' replace with a '\'
    (goto-char (point-min))
    (while (re-search-forward "[\\\\|@]\\(param\\|return\\)" nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (when (eq (char-after) ?@)
          (delete-char 1)
          (insert ?\\))
        (forward-line -1)
        (when (not (looking-at "^\\s-*$"))
          (end-of-line)
          (open-line 1))))

    ;; Remove any initial, standalone '//' in a comment block
    (goto-char (point-min))
    (while (re-search-forward "// *
\\s-*// " nil t)
      (replace-match "// ")
      (c-fill-paragraph))

    ;; Refill any Doxygen comment block. This only refills the first text block
    (goto-char (point-min))
    (while (re-search-forward "/\\*\\*" nil t)
      (c-fill-paragraph))

    ;; Remove space after '(' and '[' and before ')' and ']' -- old formatting style that I hate now
    (goto-char (point-min))
    (while (re-search-forward "\\(( \\)\\|\\( )\\)\\|\\(\\[ \\)\\|\\( \\]\\)" nil t)
      (let ((found (match-string-no-properties 0)))
        (if (eq (elt found 0) ?\s)
            (replace-match (substring found 1))
          (replace-match (substring found 0 1)))))

    ;; Convert system #include <...> statments for non-systemm libraries to be #include "..." instead
    (goto-char (point-min))
    (while (re-search-forward "#include <\\(\\(Qt\\|boost\\|ace\\).*\\)>" nil t)
      (replace-match "#include \"\\1\""))
    (indent-region (point-min) (point-max) nil))
  (basic-save-buffer))

(defun my/c++-cleanup-file (path)
  "Reformat the file PATH."
  (interactive "fFile: ")
  (find-file path)
  (my/c++-cleanup)
  (kill-buffer (current-buffer)))

(defun my/c++-cleanup-include ()
  "Toggle quote character surrounding path."
  (interactive)
  (save-excursion
    ;; Convert system #include <...> statments for non-systemm libraries to be #include "..." instead
    (goto-char (point-min))
    (while (re-search-forward "#include \"\\(\\(Qt\\|boost\\|ace\\).*\\)\"" nil t)
      (replace-match "#include <\\1\>")))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#include \"UnitTest/UnitTest.h\"" nil t)
      (replace-match "#include \"UnitTest/TestObj.h\"")))

  (basic-save-buffer))

    ;; (when (looking-at "#include \\([\"<]\\)\\(.*\\)\\([\">]\\)")
    ;;   (let ((quote (match-string-no-properties 1)))
    ;;     (if (string= quote "<")
    ;;         (replace-match "#include \"\\2\"")
    ;;       (replace-match "#include <\\2>"))))))

;;;###autoload
(defun my/c++-mode-hook ()
  "Custom C++ mode hook."
  (my/c-mode-common)
  (abbrev-mode 1)
  (c-add-style "My C++ Style" my/c-style t)
  (c-set-offset 'innamespace 0)
  (setq c-at-vsemi-p-fn 'my/c++-at-vsemi-p
	c-vsemi-status-unknown-p-fn 'my/c++-vsemi-status-unknown-p
        c-block-comment-prefix ""
        c-doc-comment-style 'doxygenf)

  (local-set-key [(f7)] #'compile)
  (local-set-key [(control c)(control i)] #'my/c++-copyright-skeleton)
  (local-set-key [(control meta \;)] #'my/c++-insert-block-comment)
  (local-set-key [(f1)] #'my/c++-cleanup-include))

(provide 'my/c++-mode)
;;; my-c++-mode.el ends here

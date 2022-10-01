;;; package -- my-c-mode-common -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'doxygen)

;; Hilight parentheses and braces
;;
(font-lock-add-keywords 'c-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'c++-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'objc-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'idl-mode '(("[][(){}]" . font-lock-brace-face)))

(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(constexpr\\|final\\|noexcept\\|nullptr\\)\\>" . 'font-lock-keyword-face)))

(define-skeleton my-c-include-statement
  "Sekeleton C/C++/ObjC include statement.
Inserts #include\"\" and leaves
the insertion point between the double-quotation marks. Use a PREFIx to insert
<> instead of \"\"."
  nil
  (if (eq major-mode 'objc-mode)
      "#import "
    "#include ")
  (if (null current-prefix-arg)
      "\""
    "<")
  -
  (if (null current-prefix-arg)
      "\""
    ">")
  \n)

(defconst my-c++-include-dir
  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
  ;; (let ((tmp "/usr/include/c++/4.2.1"))
  ;;   (if (shell-command "gcc -v" "*foo*")
  ;;       (with-current-buffer "*foo*"
  ;;         (goto-char (point-min))
  ;;         (if (search-forward-regexp
  ;;              "--prefix=\\([^ ]+\\).*--with-gxx-include-dir=\\(\\S +\\)"
  ;;              (point-max) t)
  ;;             (setq tmp (concat (match-string 1) (match-string 2))))))
  ;;   (kill-buffer "*foo*")
  ;;  tmp)
  "Determine the location of the standard include files.")

(defconst my-c-on-include-line-re
  "#\\(import\\|include\\)[^\"<]+[\"<]\\([^>\"]+\\)[>\"]"
  "Regular expression that matches a lines containing a C/C++/ObjC include.")

(defconst my-c-locate-include-file-re
  "\\(\\(.*\\)/\\)*\\([^/]+\\)"
  "Regular expression that separates include path into directory and file.
Group 1: directory + '/'
Group 2: directory sans trailing '/'
Group 3: file sans any directory prefix")

(defvar my-c-locate-include-dirs
  (list
   "./\\1\\3"				; Locate file in local directory
   "~/src/Mine/sidecar/\\1\\3"          ; In SideCar source tree
   "/opt/homebrew/include/\\1\\3"
   "/opt/homebrew/Cellar/qt@5/5.15.5_3/include/\\1\\3.h"        ; Handle Qt '#include "DIR/File"'
   "/System/Library/Frameworks/\\2.framework/Headers/\\3" ; In MacOS X framewks
   (concat my-c++-include-dir "/\\1\\3")		  ; in toolchain installation
   "/usr/include/\\1\\3")				  ; in OS includes
  "List of replacement patterns.")

(defun my-c-locate-include-file (tail proc)
  "Attempt to locate an include file with path ending in TAIL.
If found, invoke
the PROC method in order to show it. Relies on `my-c-locate-include-dirs'
variable to build complete paths."
  (let ((dirs my-c-locate-include-dirs)
        ;; Group 1: directory + '/'
        ;; Group 2: directory sans trailing '/'
        ;; Group 3: file sans any directory prefix
	(path (string-match my-c-locate-include-file-re tail)))
    (while (and dirs
		(setq path (replace-match (car dirs) nil nil tail))
		(not (file-exists-p path)))
      (setq dirs (cdr dirs)))
    (if dirs
	(funcall proc path)
      (error "Unable to locate file for '%s'" tail ))))

(defun my-c-expand-include-file (proc)
  "Locate include file using PROC."
  (let ((p nil))
    (save-excursion
      (beginning-of-line)
      (if (looking-at my-c-on-include-line-re)
	  (my-c-locate-include-file (match-string-no-properties 2) proc)
	(setq p (search-forward-regexp my-c-on-include-line-re (point-max) t))))
    (when p (goto-char p))))

(defun my-c-find-include-file ()
  "If on an empty line, insert the `my-c-include-statement' skeleton.
Otherwise, attempt to open the an include file."
  (interactive)
  (if (looking-at "^[ \t]*$")
      (my-c-include-statement)
    (my-c-expand-include-file (function find-file))))

(defun my-c-find-include-file-other-window ()
  "Attempt to open an include file in another window from the current one."
  (interactive)
  (my-c-expand-include-file (function find-file-other-window)))

(defun my-c-find-include-file-other-frame ()
  "Attempt to open an include file in another frame from the current one."
  (interactive)
  (my-c-expand-include-file (function find-file-other-frame)))

(defun my-c-find-file-with-extension (base extensions default)
  "Attempt to locate a file called BASE.
Look for files with an extension found in the list of
EXTENSIONS. If the list is empty, use DEFAULT as the extension."
  (if (null extensions)
      (concat base "." default)
    (let ((filename (concat base "." (car extensions))))
      (if (file-exists-p filename)
	  filename
	(my-c-find-file-with-extension base (cdr extensions) default)))))

(defun my-c-make-twin-file ()
  "Locate the 'twin' of the current source file.
For instance, if the current
file has 'cc' for its extension, attempt to locate and edit a file with the
same name but with an extension of 'h'. Support C/C++/ObjC file extensions."
  (let ((extension (file-name-extension (buffer-file-name)))
	(sans (file-name-sans-extension (buffer-file-name))))
    (if (string-match "[hH][xp]*" extension)

	;; Apparently the current source file is an include file. Use the
	;; file's mode to define the suffixes that the implementation file may
	;; have.
	;;
	(cond
	 ((string-equal "ObjC" mode-name)
	  (my-c-find-file-with-extension sans '("m" "mm") "mm"))
	 ((string-equal "C++" mode-name)
	  (my-c-find-file-with-extension sans '("cc" "cpp" "cxx" "C") "cc"))
	 (t
	  (my-c-find-file-with-extension sans
					 '("cc" "cpp" "c" "C" "cp" "m" "mm")
					 "cc")))
      ;;
      ;; Search for an include file with various suffixes.
      ;;
      (my-c-find-file-with-extension sans '("h" "hxx" "hpp" "H") "h"))))

(defvar my-make-next-file-name-generator
  (function my-c-make-twin-file)
  "Ha.")

(defun my-c-twin-file ()
  "Locate another file with the same basename as the current one."
  (let* ((sans (file-name-sans-extension (buffer-file-name)))
	 (files (directory-files (file-name-directory (buffer-file-name)) t
				 (concat "^" (file-name-nondirectory sans)
					 "\\.[chp]+$")))
	 (count (length files))
	 (index 0)
	 (found nil))

    ;; Locate the entry that matches the current file name. We will return the
    ;; one following it.
    ;;
    (if (< count 2)
	(my-c-make-twin-file)
      (while (and (not found) (< index count))
	(if (string-equal (buffer-file-name) (elt files index))
	    (setq found (+ index 1)))
	(setq index (+ index 1)))
      (if found
          (elt files (% found count))))))

(defun my-c-find-twin ()
  "Locate the 'twin' of the current buffer and show in the current window."
  (interactive)
  (find-file (my-c-twin-file)))

(defun my-c-find-twin-other-window ()
  "Locate the 'twin' of the current buffer and show in another window."
  (interactive)
  (find-file-other-window (my-c-twin-file)))

(defun my-c-find-twin-other-frame ()
  "Locate the 'twin' of the current buffer and show in another frame."
  (interactive)
  (find-file-other-frame (my-c-twin-file)))

(defun my-convert-to-c++ ()
  "Convert the contents of the current buffer to one that represents C++ code."
  (interactive)
  (goto-char (point-min))
  (insert "// -*- C++ -*-\n")
  (while (re-search-forward "[ \t]+" nil t)
    (replace-match " " nil t))
  (goto-char (point-min))
  (while (re-search-forward "#include <\\(.*/.*\\\)>" nil t)
    (replace-match "#include \"\\1\"" nil nil))
  (goto-char (point-min))
  (while (re-search-forward "\\([^<:]\\)\\([a-z]*[io]stream\\b\\)" nil t)
    (replace-match "\\1std::\\2" nil nil))
  (c++-mode)
  (indent-region (point-min) (point-max) nil))

(defconst my-objc-hanging-braces-alist
  '((brace-list-open)
    (brace-entry-open)
    (statement-cont)
    (substatement-open after)
    (block-close . c-snug-do-while)
    (extern-lang-open after)
    (namespace-open)
    (module-open after)
    (defun-open after)
    (composition-open after)
    (inexpr-class-open after)
    (inexpr-class-close before))
  "My Objective C hanging braces alist.")

(defconst my-c-hanging-braces-alist
  (assq-delete-all 'defun-open (copy-alist my-objc-hanging-braces-alist))
  "My C/C++ hanging braces alist.")

(defconst my-base-style
  '("stroustrup")
  "My base style.")

(defconst my-c-style
  (append (copy-alist my-base-style)
	  (list (cons 'c-hanging-braces-alist my-c-hanging-braces-alist)))
  "My C/C++ style.")

(defconst my-objc-style
  (append (copy-alist my-base-style)
	  (list (cons 'c-hanging-braces-alist my-objc-hanging-braces-alist)))
  "My Objective C style.")

(defun view-qt-doc-default (&optional pos)
  "Make a guess at a default Qt document entry based on the text at POS.
If POS is nil, the current point is used."
  (let (word)
    (save-excursion
      (if pos (goto-char pos))
      ;; Default man entry title is any word the cursor is on, or if
      ;; cursor not on a word, then nearest preceding word.
      (skip-chars-backward "-a-zA-Z0-9._+")
      (let ((start (point)))
	(skip-chars-forward "-a-zA-Z0-9._+")
	(setq word (buffer-substring-no-properties start (point))))
      (if (string-match "[._]+$" word)
	  (setq word (substring word 0 (match-beginning 0))))
      ;; If looking at something like *strcat(... , remove the '*'
      (if (string-match "^*" word)
	  (setq word (substring word 1)))
      ;; If looking at something like ::strcat(... , remove the '::'
      (if (string-match "^.*::" word)
	  (setq word (substring word (match-end 0))))
      )
    word))

(defvar view-qt-doc-history nil "Qt doc read history.")

(defun view-qt-doc (class)
  "Show the Qt documentation for CLASS."
  (interactive
   (list (let* ((default-entry (view-qt-doc-default))
		(input (read-string
			(format "Qt class:%s"
				(if (string= default-entry "")
				    ": "
				  (format " (default %s): " default-entry)))
			nil 'view-qt-doc-history default-entry)))
	   (if (string= input "")
	       (error "No Qt class given")
	     input))))
  (browse-url (concat "file:///opt/sidecar/qt/doc/html/"
			 (downcase class)
			 ".html")))

(defvar my-oob-directory-name "build" "The name of the directory to look for.")

(defun my-get-build-directory (file-name)
  "Locate the build directory for FILE-NAME."
  (let* ((dir (file-name-directory (expand-file-name file-name)))
	 (build (concat dir my-oob-directory-name))
	 (found nil)
	 (bit nil)
	 (bits '()))

    ;;
    ;; Walk up the directory path looking for something called 'build'.
    ;; Remember the directories we left before finding it.
    ;;
    (while (and (not (file-exists-p build))
		(not (string-equal dir "/")))
      (setq dir (directory-file-name dir)
	    bit (file-name-nondirectory dir)
	    bits (cons bit bits)
	    dir (file-name-directory dir)
	    build (concat dir my-oob-directory-name)))

    ;;
    ;; Try and go down the same directory path but starting in the 'build'
    ;; directory (only if it exists of course). This will handle the case when
    ;; the 'build' directory is inside the top-level source directory.
    ;;
    (while (and (not (null bits))
		(file-exists-p build))
      (setq found build
	    build (concat (file-name-as-directory build) (car bits))
	    bits (cdr bits)))

    ;;
    ;; If we have some remaining directory bits and the last checked directory
    ;; did not exist, try again. This will catch instances where the build
    ;; directory and the source directory are in the same parent directory.
    ;;
    (when (and bits found (not (file-exists-p build)))
      (setq build found)
      (while (and (not (null bits))
		  (file-exists-p build))
	(setq found build
	      build (concat (file-name-as-directory build) (car bits))
	      bits (cdr bits))))

    ;;
    ;; Final check to see if the last build directory is valid.
    ;;
    (when (file-exists-p build)
      (setq found build))

    found))

(defun my-open-block-c-mode (id action context)
  "Add new line after inserting pair of braces.
ID ACTION CONTEXT."
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-c-mode-common ()
  "Common hook for C/C++ modes."
  ;; (setq c-backspace-function 'backward-delete-char-untabify
  ;; c-block-comment-prefix "   "
  ;; c-basic-offset 4
  ;; c-tab-always-indent nil
  ;; )

  (unless (boundp 'my-c-c-c-4-keymap)
    (define-prefix-command 'my-c-c-c-4-keymap))

  (local-set-key [(control c)(control f)] 'my-c-find-twin)

  (local-set-key [(control c)(control meta f)] 'c-forward-into-nomenclature)
  (local-set-key [(control c)(control meta b)] 'c-backward-into-nomenclature)
  (local-set-key [(control c)(control meta u)] 'my-capitalize-nomenclature)
  (local-set-key [(control c)(control meta d)] 'my-forward-delete-nomenclature)

  (local-set-key [(control c)(?d)] doxygen-keymap)
  (local-set-key [(control c)(?\;)] 'doxygen-insert-block-comment)
  (local-set-key [(control c)(meta \;)] 'doxygen-insert-inline-comment)
  (local-set-key [(control c)(meta \:)] 'doxygen-transform-inline-comment)

  (local-set-key [(control c)(?i)] 'my-c-find-include-file)
  (local-set-key [(control c)(?q)] 'view-qt-doc)

  (local-set-key [(control c)(?4)] 'my-c-c-c-4-keymap)
  (local-set-key [(control c)(?4)(?i)] 'my-c-find-include-file-other-window)
  (local-set-key [(control c)(?4)(?f)] 'my-c-find-twin-other-window)

  (unless (boundp 'my-c-c-c-5-keymap)
    (define-prefix-command 'my-c-c-c-5-keymap))

  (local-set-key [(f5)] 'next-error)
  (local-set-key [(control c)(?5)] 'my-c-c-c-5-keymap)
  (local-set-key [(control c)(?5)(?i)] 'my-c-find-include-file-other-frame)

  (local-set-key [(return)] 'newline-and-indent)
  (local-set-key [(control j)] 'newline)

  ;;(local-set-key [(f8)] 'c++-mode)
  ;; (local-set-key [(shift) (f8)] 'objc-mode)

  ;;
  ;; Attempt to locate a 'build' directory to move to when compiling a source
  ;; file.
  ;;
  (unless (or (null buffer-file-name)
	      (file-exists-p "Makefile")
	      (file-exists-p "makefile")
	      (file-exists-p "GNUMakefile"))
    (let ((build-directory (my-get-build-directory buffer-file-name)))
      (if build-directory
	(set (make-local-variable 'compile-command)
	     (concat "make -C " build-directory)))))

  ;; (semantic-mode 1)

  ;;
  ;; No IMenu support in IDL files.
  ;;
  (unless (string-equal (file-name-extension (buffer-name)) "idl")
    (imenu-add-menubar-index))

  ;; (sp-local-pair 'c-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
  ;; (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-create-newline-and-enter-sexp))
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (show-paren-mode t))

(provide 'my-c-mode-common)
;;; my-c-mode-common.el ends here

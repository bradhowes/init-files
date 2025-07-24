(require 'my-c-mode-common)

(defun my-objc-rewrite-vars ()
  ""
  (interactive)
  (goto-char (point-min))
  (while
      (re-search-forward "\\(\\(\\b\\w+\\b\\)_\\)\\W+" (point-max) t)
    (goto-char (match-end 1))
    (insert "_" (match-string 2))
    (delete-region (match-beginning 1) (match-end 1))
    (goto-char (match-end 0))))

(defun my-objc-rewrite-procs ()
  ""
  (interactive)
  (goto-char (point-min))
  (while
      (re-search-forward "^[+-]\\(\\s-+\\)(" (point-max) t)
    (delete-region (match-beginning 1) (match-end 1))
    (goto-char (match-end 0))))
		   
(defun my-objc-insert-header-preamble ()
  ""
  (interactive)
  (goto-char (point-min))
  (insert "// -*- ObjC -*-\n"
	  "/**\n"
	  "   \\file " (file-name-nondirectory (buffer-file-name)) "\n"
	  "   \\author Brad Howes\n\n"
"   Copyright (C) 2005, 2006, 2015 B-Ray Software. All rights reserved.\n\n"
"   VideoNotator is free software; you can redistribute it and/or modify it\n"
"   under the terms of the GNU General Public license as published by the\n"
"   Free Software Foundation; either version 2, or (at your option) any later\n"
"   version.\n"
	  "*/\n\n"))

(defun my-objc-update-include-header ()
  ""
  (interactive)
  (goto-char (point-min))
  (let ((in-comment nil))
    (while (cond
	    (in-comment
	     (when (looking-at ".*\\*/") (setq in-comment nil))
	     t)
	    ((looking-at "^[ \t]*/[*/]")
	     (setq in-comment t))
	    ((looking-at "^[ \t]*$")
	     t)
	    (t
	     nil))
      (kill-whole-line 1)))
  (my-objc-insert-header-preamble))

(defun my-objc-include-header ()
  ""
  (interactive)
  (if (equal (point-min) (point-max))
      (progn
	(my-objc-insert-header-preamble)
	(insert "#import <Cocoa/Cocoa.h>\n\n@interface "
		(file-name-sans-extension (file-name-nondirectory
					   (buffer-file-name))))
	(save-excursion
	  (insert " : NSObject {\n@private\n}\n\n@end\n")))
    (my-objc-update-include-header)))

(defun my-objc-remove-comments ()
  ""
  (interactive)
  (goto-char (point-min))
  (while (looking-at "^//")
    (kill-whole-line 1))
  (while (re-search-forward "/\\*" nil t)
    (delete-region (match-beginning 0)
		   (re-search-forward"\\*/\\s-*\n?" nil t))))

(defun my-objc-comment-file ()
  ""
  (interactive)
  (when (not (eq major-mode 'objc-mode))
    (objc-mode))
  (goto-char (point-min))
  (my-objc-update-include-header)
  (doxygen-comment-file))

(defun my-objc-insert-block-comment ()
  ""
  (interactive)
  (let ((found (doxygen-find-declaration)))
    (if found
	(progn
	  (goto-char (car found))
	  (doxygen-insert-prepped-comment (doxygen-line-prefix) found))
      (my-insert-block-comment 'newline-and-indent "//" "//" "//"))))

(defun my-objc-mode-hook ()
  (setq c-tab-always-indent t)
  (c-add-style "My Objective C" my-objc-style t)
  ;; (local-set-key [(control meta \;)] 'my-objc-insert-block-comment)
  ;; (local-set-key [(control c)(contorl i)] 'my-objc-include-header)
  ;; (local-set-key [(control c)(?p)] 'my-objc-rewrite-procs)
  ;; (local-set-key [(control c)(?r)] 'my-objc-rewrite-vars)
  )

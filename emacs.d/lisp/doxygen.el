;;; package -- doxygen -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my/split-string (string &optional sep omit)
  "Split STRING at SEP.
If OMIT then remove any empty strings."
  (let ((result (split-string string sep)))
    (if omit
	(delete "" result)
      result)))

(defvar doxygen-find-declaration-configs
  '(objc-mode
    (("[ \t]*typedef\\b.*;"
      identity nil)
     ("^[ \t]*@\\(interface\\)\\b\\([^{:]*\\)[^}]*}"
      doxygen-process-c-container-match 1 2)
     ("^[ \t]*\\(class\\|struct\\|union\\|enum\\)\\b\\([^{;:]*\\)[{;:]"
      doxygen-process-c-container-match 1 2)
     ("^[ \t]*\\([+-]\\)[ \t]*(\\([^)]+\\))\\([^;{]+\\)\\(\\({[^}]*}\\)\\|;\\)"
      doxygen-process-obj-function-match)
     ("^[ \t]*\\([^(;{]+\\)(\\([^;{]*\\))[^{;]*[{;]"
      doxygen-process-c-function-match))

    c-mode
    (("[ \t]*typedef\\b.*;"
      identity nil)
     ("^[ \t]*\\(struct\\|union\\|enum\\)\\b\\([^{;]*\\)[{;]"
       doxygen-process-c-container-match 1 2)
     ("^[ \t]*\\([^(;{}]+\\)(\\([^;{]*\\))[ \t\n]*[{;]"
      doxygen-process-c-function-match))

    c++-mode
    (("[ \t]*typedef\\b.*;"
      identity nil)
     ("^[ \t]*\\(\\(template.*>[ \t\n]*\\)?\\(namespace\\|class\\|struct\\|union\\|enum\\)\\b\\([^{;:]*\\)\\)[{;:]"
      doxygen-process-c-container-match 3 4)
     ("^[ \t]*\\([^(;{]+\\)(\\([^;{]*\\))\\([ \t\n]*const\\)?[ \t\n]*throw(.*)[ \t\n]*[^{;]*[{;]"
      doxygen-process-c-function-match)
     ("^[ \t]*\\([^(;{]+\\)(\\([^;{]*\\))[^{;]*[{;]"
      doxygen-process-c-function-match))

    python-mode
    (("^[ \t]*def[ \t]+\\([^(]+\\)(\\(.*\\))[ \t]*:"
      doxygen-process-python-function-match)))
  "Regular epxression to use to locate function declarations.")

(defconst doxygen-locate-c-function-pointer-spec-re
  "([^)]*\\*[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*)"
  "Regular expression that matches function pointer in argument list.")

(defvar doxygen-insert-summary nil
  "If TRUE insert a short summary in the start of the comment block.")

(defvar doxygen-markers nil
  "A list of markers in a comment template.")

(defvar doxygen-group-history nil
  "Group history.")

(defun doxygen-toggle-insert-summary (arg)
  "Change the setting of the `doxygen-insert-summary' variable.
If prefix ARG is 1, the value is toggled. If <= 0, then the value is nil.
 Otherwise,
the value is set to t."
  (interactive "p")
  (setq doxygen-insert-summary (cond
				((eq arg 1) (not doxygen-insert-summary))
				((<= arg 0) nil)
				(t t)))
  (message "doxygen-insert-summary is %s" doxygen-insert-summary))

(defun doxygen-clear-marker()
  "Remove all doxygen markers for the current buffer."
  (interactive)
  (setq doxygen-markers nil))

(defun doxygen-push-marker ()
  "Add  marker to the start of `doxygen-markers'."
  (let ((pm (point-marker)))
    (unless (member pm doxygen-markers)
      (setq doxygen-markers (cons pm doxygen-markers)))))

(defun doxygen-update-markers ()
  "Rearrange contents of `doxygen-markers' so it is in ascending order.
Also removes duplicates."
  (when doxygen-markers
    (setq doxygen-markers (sort doxygen-markers '<)))
  (if (and (length> doxygen-markers 0)
	   (equal (point-min) (marker-position (nth 0 doxygen-markers))))
      (setq doxygen-markers (cdr doxygen-markers))))

(defun doxygen-line-prefix ()
  "Return the white-space found at the start of the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*")
	(match-string-no-properties 0)
      "")))

(defun doxygen-get-c-arg-name (scrap)
  "Return the argument name from a SCRAP of an argument list."

  ;; Strip off any default assignment values.
  ;;
  (when (string-match "=" scrap)
    (setq scrap (substring scrap 0 (match-beginning 0))))
  ;;
  ;; Take the last token in the string as the variable name (any function
  ;; pointer types have already been removed)
  ;;
  (setq scrap (car (last (my/split-string scrap nil t))))

  ;;
  ;; Strip off any trailing '[]'
  ;;
  (if (string-match "^\\(.*\\)\\[\\]$" scrap)
      (substring scrap (match-beginning 1) (match-end 1))
    scrap))

(defun doxygen-locate-c-function-pointer-specs (clause offset)
  "Return location of function in CLAUSE.
The search will start at OFFSET. Returns nil if not found."
  (and (string-match doxygen-locate-c-function-pointer-spec-re clause offset)
       (match-data)))

(defun doxygen-rewrite-c-arg-clause (clause match)
  "Remove any function pointer specs found CLAUSE.
Replace with just the argument name. If MATCH is a list, recursively call
`doxygen-rewrite-arg-clause' with the CDR of MATCH before doing our own
rewrite on CLAUSE. Returns the changed CLAUSE value"
  (cond
   (match
     (let ((us (car match))
	   (pos nil))
       (setq clause (doxygen-rewrite-c-arg-clause clause (cdr match)))
       (setq pos (string-match ")" clause (nth 1 us)))
       (when (null pos)
	 (error "%s" "Unbalanced  '(,)' in method/function declaration"))
       (concat (substring clause 0 (nth 0 us))
	       (substring clause (nth 2 us) (nth 3 us))
	       (substring clause (1+ pos) nil))))
   (t clause)))

(defun doxygen-clean-c-arg-clause (clause)
  "Scan CLAUSE and replace any function pointer specs.
Returns the potentially changed CLAUSE value."
  ;; Strip off the initialization clause for a C++ constructor if present.
  ;;
  (when (string-match "\\()[^:]*:[^:]\\)" clause)
      (setq clause (substring clause 0 (match-beginning 1))))
  (let ((all '())
	(found (doxygen-locate-c-function-pointer-specs clause 0)))
    (when found
      (while found
	(setq all (append all (list found))
	      found (doxygen-locate-c-function-pointer-specs clause
							     (nth 1 found))))
      (setq clause (doxygen-rewrite-c-arg-clause clause all)))
    clause))

(defun doxygen-process-c-container-match (kind name)
  "Convert raw match data into a list of values to fill Doxygen comment.
Operates on KIND expressions with NAME."
  (let ((beg (match-beginning 0))
	(end (match-end 0))
	(tag (mapconcat 'identity
			(my/split-string
			 (concat (capitalize
				  (match-string-no-properties kind))
				 " "
				 (match-string-no-properties name)) nil t)
			" ")))
    (list beg end
	  (list nil
		tag
		nil))))

(defun doxygen-clean-name+return (clause)
  "Obtain the name and return value of a CLAUSE."
  (let ((info nil)
	(items nil)
	(first nil)
	(name nil))
    (when (string-match "[ \t]*template\\b<.*>" clause)
      (setq info (cons (cons 'template (match-string-no-properties 0)) info)
	    clause (substring clause (match-end 0))))
    (setq items (my/split-string clause nil t)
	  first (car items))

    (when (string-equal "explicit" first)
      (setq info (cons (cons 'explicit t) info)
	    items (cdr items)
	    first (car items)))

    (when (string-equal "static" first)
      (setq info (cons (cons 'static t) info)
	    items (cdr items)
	    first (car items)))

    (when (string-equal "virtual" first)
      (setq info (cons (cons 'virtual t) info)
	    items (cdr items)
	    first (car items)))

    (setq name (car (last items)))
    (when (and (string-match "^\\(.*::\\)?\\(.*\\)$" name)
	       (match-end 1))
      (setq name (substring name (match-end 1))))

    (if (length= items 1)
	(cons (cons 'name name) info)
      (append (list (cons 'returns (not (string-equal "void" first)))
		    (cons 'name name)
		    (cons 'return-clause (butlast items)))
	      info))))

(defun doxygen-process-python-function-match ()
  "Convert match data into a list of values for Python comments.
comment block for a Python function or method. The returned list will contain
\( BEG END RETURN NAME ARGS ),where BEG is the buffer position of the start of
the function declaration being documented, END is the buffer position of the
end of the function declaration being documented, RETURN is t if the function
returns a value, and ARGS is either nil if the function takes no arguments, or
it is a list of argument names."
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (args (my/split-string (match-string-no-properties 2) "," t))
	  (name (match-string-no-properties 1)))
      (list beg end (list t name args))))

(defun doxygen-process-c-function-match ()
  "Convert match data into a list of values for C comments.
comment block for a C/C++ function or method. The returned list will contain
\( BEG END RETURN NAME ARGS ),where BEG is the buffer position of the start of
the function declaration being documented, END is the buffer position of the
end of the function declaration being documented, RETURN is t if the function
returns a value, and ARGS is either nil if the function takes no arguments, or
it is a list of argument names."
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (name+return (match-string-no-properties 1))
	  (args (match-string-no-properties 2))
	  (name nil)
	  (return nil))
      (setq name+return (doxygen-clean-name+return name+return)
	    name (cdr (assq 'name name+return))
	    return (cdr (assq 'returns name+return))
	    args (doxygen-clean-c-arg-clause args))
      (list beg end
	    (list return
		  (cond
		   ((assq 'static name+return)
		    (concat "Class method " name))

		   ((null (assq 'return-clause name+return))
		    (if (eq (elt name 0) ?~)
			(if (assq 'virtual name+return)
			    (concat "Virtual destructor for "
				    (substring name 1))
			  (concat "Destructor for "
				  (substring name 1)))
		      (concat "Constructor for " name)))

		   (t
		    (if (assq 'virtual name+return)
			(concat "Virtual method " name)
		      (if (eq major-mode 'c++-mode)
			  (concat "Method " name)
			(concat "Function " name)))))
		  (mapcar 'doxygen-get-c-arg-name (my/split-string args ","
								   t))))))

(defun doxygen-process-obj-function-match ()
  "Convert match data into a list of values for Obj-C comments.
comment block for an ObjC method. The returned list will contain
\( BEG END RETURN NAME ARGS ),where BEG is the buffer position of the start of
the function declaration being documented, END is the buffer position of the
end of the function declaration being documented, RETURN is t if the function
returns a value, and ARGS is either nil if the function takes no arguments, or
it is a list of argument names."
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (proc-type (match-string-no-properties 1))
	  (return-clause (match-string-no-properties 2))
	  (proc-clause (match-string-no-properties 3))
	  (pos 0)
	  (args '())
	  (name ""))
      ;;
      ;; Split the proc-clause into method name and args.
      ;;
      (while (string-match "\\(\\w+:\\)[ \t\n]*([^)]+)[ \t\n]*\\(\\w+\\)"
			   proc-clause pos)
	(setq args (append args (list (match-string-no-properties
				       2 proc-clause)))
	      name (concat name (match-string-no-properties 1 proc-clause))
	      pos (match-end 0)))

      (when (length= name 0)	; handle methods w/ no args
	  (setq name proc-clause))

      (list beg end
	    (list (not (string-match "\\b\\(void\\|IBAction\\)\\b"
				     return-clause))

		  (cond
		   ((string-equal proc-type "+")
		    (concat "Class method " name))
		   (t
		    (concat "Method " name)))
		  args))))

(defun doxygen-find-declaration-here ()
  "Locate interesting C/C++/ObjC object on the current line.
Returns a list to use in filling out a Doxygen comment block."
  (let ((configs (plist-get doxygen-find-declaration-configs major-mode)))
    (while (and configs
		(not (looking-at (car (car configs)))))
      (setq configs (cdr configs)))
    (when configs
      (apply (car (cdr (car configs))) (cdr (cdr (car configs)))))))

(defun doxygen-find-declaration ()
  "Identify any interesting C/C++ object following the current point.
Skips forward over white-space before calling `doxygen-find-declaration-here'."
  (save-excursion
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let ((found (doxygen-find-declaration-here)))
      (cond
       ((null found)
	nil)
       ((equal (point) (nth 0 found))
	found)
       (t
	nil)))))

(defun doxygen-insert-prepped-comment (prefix found)
  "Insert a Doxygen comment block and fill it with some tags.
Uses the data contained in FOUND. The comment and its contents are indented
with the value PREFIX. Places point at the end of the first line of the comment
block."
  (let ((begin (nth 0 found))
	(info (nth 2 found)))
    (goto-char begin)
    (insert prefix "/** ")
    (doxygen-push-marker)
    (when doxygen-insert-summary
      (insert (nth 1 info))
      (doxygen-push-marker))
    (save-excursion
      (insert "\n")
      (when (nth 2 info)
	(mapc (function (lambda (a)
			  (insert prefix "    \\param " a " ")
			  (doxygen-push-marker)
			  (insert "\n\n")))
	      (nth 2 info)))
      (when (nth 0 info)
	(insert prefix "    \\return " )
	(doxygen-push-marker)
	(insert "\n"))
      (insert prefix "*/\n"))))

(defun doxygen-insert-empty-comment ()
  "Insert an empty Doxygen comment block.
The point is left at the end of the
first line. The comment is indented with PREFIX."
  (indent-according-to-mode)
  (let ((prefix (doxygen-line-prefix)))
    (beginning-of-line)
    (insert prefix "/** ")
    (doxygen-push-marker)
    (save-excursion (insert "\n" prefix "*/\n"))))

(defun doxygen-insert-block-comment ()
  "Insert a Doxygen C/C++ block comment for the next non-blank line."
  (interactive "*")
  (let ((found (doxygen-find-declaration)))
    (if found
	(doxygen-insert-prepped-comment (progn
					  (goto-char (nth 0 found))
					  (doxygen-line-prefix))
					found)
      (doxygen-insert-empty-comment))
    (doxygen-update-markers)))

(defun doxygen-insert-group (name &optional from to)
  "Insert Doxygen comments to define a Doxygen group.
The group NAME is read
from the minibuffer. The group will enclose the region FROM TO unless a prefix
argument is present, in which case an empty group is inserted at the point."
  (interactive
   (let ((name (read-string "Group name: " nil 'doxygen-group-history)))
     (if current-prefix-arg
	 (list name (point) (point))
       (list name (region-beginning) (region-end)))))
  (goto-char to)			; insert group close first
  (doxygen-push-marker)
  (insert "\n//@}\n")
  (goto-char from)
  (insert "#pragma mark " (upcase name)
	  "\n/** \\name " (capitalize name))
  (doxygen-push-marker)
  (insert "\n*/\n//@{\n\n" )
  (doxygen-push-marker)
  (doxygen-update-markers))

(defun doxygen-first-marker-greater-than (point markers)
  "Obtain first item in MARKERS greater than POINT."
  (cond ((null markers) nil)
	((> (car markers) point) (car markers))
	(t (doxygen-first-marker-greater-than point (cdr markers)))))

(defun doxygen-forward-marker ()
  "Jump to the next mark in `doxygen-markers'."
  (interactive)
  (let ((marker (doxygen-first-marker-greater-than (point) doxygen-markers)))
    (if marker (goto-char marker)
      (if doxygen-markers (goto-char (car doxygen-markers))))))

(defun doxygen-first-marker-lesser-than (point markers)
  "Jump to the first marker in MARKERS before POINT."
  (cond ((null markers) nil)
	((< (car markers) point)
	 (or (doxygen-first-marker-lesser-than point (cdr markers)) (car markers)))
	(t nil)))

(defun doxygen-backward-marker ()
  "Jump to the previous mark in `doxygen-markers'."
  (interactive)
  (let ((marker (doxygen-first-marker-lesser-than (point) doxygen-markers)))
    (if marker (goto-char marker)
      (if doxygen-markers (goto-char (car (last doxygen-markers)))))))

(defun doxygen-insert-inline-comment ()
  "Insert an inline Doxygen comment.
This probably only works for C/C++/ObjC
comments. Maybe Java too."
  (interactive "*")
  (comment-dwim nil)
  (cond
   ((string-equal comment-start "/* ")
    (forward-char -1)
    (if (looking-at "*<")
	(forward-char 3)
      (insert "*<")
      (forward-char 1)))
   ((string-equal comment-start "// ")
    (forward-char -1)
    (if (looking-at "/<")
	(forward-char 3)
      (insert "/<")
      (forward-char 1)))))

(defun doxygen-comment-file ()
  "Look for all interesting objects in the current buffer.
Insert a Doxygen comment block before each."
  (interactive)
  (let ((found nil)
	(marker (make-marker)))
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (setq found (doxygen-find-declaration))
      (if found
	  (progn
	    (set-marker marker (car (cdr found)))
	    (goto-char (car found))
	    (doxygen-insert-prepped-comment (doxygen-line-prefix) found)
	    (goto-char marker))
	(forward-line 1)))
    (set-marker marker nil)))

(defvar doxygen-keymap nil)
(unless doxygen-keymap
  (define-prefix-command 'doxygen-keymap)
  (define-key doxygen-keymap [(?\;)] 'doxygen-insert-inline-comment)
  (define-key doxygen-keymap [(?d)] 'doxygen-insert-block-comment)
  (define-key doxygen-keymap [(?g)] 'doxygen-insert-group)
  (define-key doxygen-keymap [(?n)] 'doxygen-forward-mark)
  (define-key doxygen-keymap [(?p)] 'doxygen-backward-mark)
  (define-key doxygen-keymap [(?s)] 'doxygen-toggle-insert-summary))

(make-variable-buffer-local 'doxygen-insert-summary)
(make-variable-buffer-local 'doxygen-marks)

(provide 'doxygen)
;;; doxygen.el ends here

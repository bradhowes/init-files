;;
;; doxygen.el
;;
(defun my-split-string (string &optional sep omit)
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
  "")

(defconst doxygen-locate-c-function-pointer-spec-re
  "([^)]*\\*[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*)"
  "Regular expression that matches part of a function pointer spec found in an
argument list.")

(defvar doxygen-insert-summary nil
  "*If TRUE insert a short summary in the start of the comment block.")

(defvar doxygen-marks nil
  "A list of marks to jump to with `\\[doxygen-forward-mark]' and
`\\[doxygen-backward-mark]'.")

(defvar doxygen-group-history nil
  "")

(defun doxygen-toggle-insert-summary (arg)
  "Change the setting of the `doxygen-insert-summary' variable. If prefix
argument is 1, the value is toggled. If <= 0, then the value is nil. Otherwise,
the value is set to t."
  (interactive "p")
  (setq doxygen-insert-summary (cond
				((eq arg 1) (not doxygen-insert-summary))
				((<= arg 0) nil)
				(t t)))
  (message "doxygen-insert-summary is %s" doxygen-insert-summary))

(defun doxygen-clear-marks()
  "Remove all marks from the `doxygen-marks' variable for the current buffer."
  (interactive)
  (setq doxygen-marks nil))

(defun doxygen-push-mark ()
  "Add a mark to the start of `doxygen-marks'."
  (let ((pm (point-marker)))
    (unless (member pm doxygen-marks)
      (setq doxygen-marks (cons pm doxygen-marks)))))

(defun doxygen-update-marks ()
  "Rearrange contents of `doxygen-marks' so that its contents is in ascending
order, and no duplicate values exist."
  (when doxygen-marks
    (setq doxygen-marks (sort doxygen-marks '<)))
  (if (and (length doxygen-marks)
	   (equal (point-min) (marker-position (nth 0 doxygen-marks))))
      (setq doxygen-marks (cdr doxygen-marks))))

(defun doxygen-line-prefix ()
  "Return the white-space found at the start of the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*")
	(match-string-no-properties 0)
      "")))

(defun doxygen-get-c-arg-name (scrap)
  "Return the argument name from a fragment of an argument list."
  ;;
  ;; Strip off any default assignment values.
  ;;
  (when (string-match "=" scrap)
    (setq scrap (substring scrap 0 (match-beginning 0))))
  ;;
  ;; Take the last token in the string as the variable name (any function
  ;; pointer types have already been removed)
  ;;
  (setq scrap (car (last (my-split-string scrap nil t))))

  ;;
  ;; Strip off any trailing '[]'
  ;;
  (if (string-match "^\\(.*\\)\\[\\]$" scrap)
      (substring scrap (match-beginning 1) (match-end 1))
    scrap))

(defun doxygen-locate-c-function-pointer-specs (clause offset)
  "Return string positions if the regular expression
`doxygen-locate-function-pointer-spec-re' is found in CLAUSE. The search will
start at OFFSET. Returns nil if not found."
  (and (string-match doxygen-locate-c-function-pointer-spec-re clause offset)
       (match-data)))

(defun doxygen-rewrite-c-arg-clause (clause match)
  "Remove any function pointer specifications found in the argument list CLAUSE
and replace with just the argument name. If MATCH is a list, recursively call
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
  "Scan the arguments CLAUSE and replace any function pointer specifications
with the variable name contained in the specification. Returns the potentially
changed CLAUSE value."
  ;;
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
  "Convert raw match data into a list of values used to fill in a Doxygen
comment block for a C/C++ structure or C++ namespace."
  (let ((beg (match-beginning 0))
	(end (match-end 0))
	(tag (mapconcat 'identity
			(my-split-string
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
  ""
  (let ((info nil)
	(items nil)
	(first nil)
	(name nil))
    (when (string-match "[ \t]*template\\b<.*>" clause)
      (setq info (cons (cons 'template (match-string-no-properties 0)) info)
	    clause (substring clause (match-end 0))))
    (setq items (my-split-string clause nil t)
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

    (if (eq (length items) 1)
	(cons (cons 'name name) info)
      (append (list (cons 'returns (not (string-equal "void" first)))
		    (cons 'name name)
		    (cons 'return-clause (butlast items)))
	      info))))

(defun doxygen-process-python-function-match ()
  "Convert raw match data into a list of values used to fill in a Doxygen
comment block for a Python function or method. The returned list will contain
\( BEG END RETURN NAME ARGS ),where BEG is the buffer position of the start of
the function declaration being documented, END is the buffer position of the
end of the function declaration being documented, RETURN is t if the function
returns a value, and ARGS is either nil if the function takes no arguments, or
it is a list of argument names."
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (args (my-split-string (match-string-no-properties 2) "," t))
	  (name (match-string-no-properties 1))
	  (return nil))
      (list beg end (list t name args))))

(defun doxygen-process-c-function-match ()
  "Convert raw match data into a list of values used to fill in a Doxygen
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
		  (mapcar 'doxygen-get-c-arg-name (my-split-string args ","
								   t))))))

(defun doxygen-process-obj-function-match ()
  "Convert raw match data into a list of values used to fill in a Doxygen
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

      (when (eq (length name) 0)	; handle methods w/ no args
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
  "Identify any interesting C/C++/ObjC object on the current line and possibly
return a list to use in filling out a Doxygen comment block."
  (let ((configs (plist-get doxygen-find-declaration-configs major-mode)))
    (while (and configs
		(not (looking-at (car (car configs)))))
      (setq configs (cdr configs)))
    (when configs
      (apply (car (cdr (car configs))) (cdr (cdr (car configs)))))))

(defun doxygen-find-declaration ()
  "Identify any interesting C/C++ object following the current point. Skips
forward over white-space before calling `doxygen-find-declaration-here'."
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
  "Insert a Doxygen comment block and fill it with some tags to represent the
data contained in FOUND. The comment and its contents are indented with the
value PREFIX. Places point at the end of the first line of the comment block."
  (let ((begin (nth 0 found))
	(end (nth 1 found))
	(info (nth 2 found)))
    (goto-char begin)
    (insert prefix "/** ")
    (doxygen-push-mark)
    (when doxygen-insert-summary
      (insert (nth 1 info))
      (doxygen-push-mark))
    (save-excursion
      (insert "\n")
      (when (nth 2 info)
	(mapc (function (lambda (a)
			  (insert prefix "    \\param " a " ")
			  (doxygen-push-mark)
			  (insert "\n\n")))
	      (nth 2 info)))
      (when (nth 0 info)
	(insert prefix "    \\return " )
	(doxygen-push-mark)
	(insert "\n"))
      (insert prefix "*/\n"))))

(defun doxygen-insert-empty-comment ()
  "Insert an empty Doxygen comment block. The point is left at the end of the
first line. The comment is indented with PREFIX."
  (indent-according-to-mode)
  (let ((prefix (doxygen-line-prefix)))
    (beginning-of-line)
    (insert prefix "/** ")
    (doxygen-push-mark)
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
    (doxygen-update-marks)))

(defun doxygen-insert-group (name &optional from to)
  "Insert Doxygen comments to define a Doxygen group. The group NAME is read
from the minibuffer. The group will enclose the region FROM TO unless a prefix
argument is present, in which case an empty group is inserted at the point."
  (interactive
   (let ((name (read-string "Group name: " nil 'doxygen-group-history)))
     (if current-prefix-arg
	 (list name (point) (point))
       (list name (region-beginning) (region-end)))))
  (goto-char to)			; insert group close first
  (doxygen-push-mark)
  (insert "\n//@}\n")
  (goto-char from)
  (insert "#pragma mark " (upcase name)
	  "\n/** \\name " (capitalize name))
  (doxygen-push-mark)
  (insert "\n*/\n//@{\n\n" )
  (doxygen-push-mark)
  (doxygen-update-marks))

(defun doxygen-first-mark-greater-than (point marks)
  (cond ((null marks) nil)
	((> (car marks) point) (car marks))
	(t (doxygen-first-mark-greater-than point (cdr marks)))))

(defun doxygen-forward-mark ()
  "Jump to the next mark in `doxygen-marks'."
  (interactive)
  (let ((mark (doxygen-first-mark-greater-than (point) doxygen-marks)))
    (if mark (goto-char mark)
      (if doxygen-marks (goto-char (car doxygen-marks))))))

(defun doxygen-first-mark-lesser-than (point marks)
  (cond ((null marks) nil)
	((< (car marks) point)
	 (or (doxygen-first-mark-lesser-than point (cdr marks)) (car marks)))
	(t nil)))

(defun doxygen-backward-mark ()
  "Jump to the previous mark in `doxygen-marks'."
  (interactive)
  (let ((mark (doxygen-first-mark-lesser-than (point) doxygen-marks)))
    (if mark (goto-char mark)
      (if doxygen-marks (goto-char (car (last doxygen-marks)))))))

(defun doxygen-insert-inline-comment ()
  "Insert an inline Doxygen comment. This probably only works for C/C++/ObjC
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
  "Look for all interesting objects in the current buffer and insert a Doxygen
comment block before them."
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

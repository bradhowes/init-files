;;; package -- my-cc-block-comment -*- Mode: Emacs-Lisp lexical-binding: t -*-
;;; Commentary:
;;;
;;; Functions/Settings that provide "good" C block comment editing
;;;
;;; Save the following in a file named 'my-cc-block-comment', byte-compile,
;;; then add this to your .emacs file:
;;;
;;;  (autoload 'my-cc-block-comment-install "my-cc-block-comment")
;;;  (add-hook 'c++-mode-hook 'my-cc-block-comment-install)
;;;
;;; Now, when you edit a C or C++ file, the following mappings will be
;;; installed:
;;;
;;;   (local-set-key [?\r] 'my-cc-newline-and-indent)
;;;   (local-set-key [?\M-\C-\;] 'my-cc-new-block-comment)
;;;   (local-set-key [?\C-:] 'my-cc-new-header-comment)
;;;   (local-set-key [?\M-q] 'my-cc-refill-block-comment)
;;;
;;; Typing "C-:" before a function will insert a header with the function name,
;;; return value and parameter names inserted in the header. NOTE: this does
;;; not work with C++ functions yet. A "C-M-;" inserts a new C/C++ block
;;; comment where the cursor is. No need to type a return at the end of a
;;;  line -- just keep going and it should autowrap for you.
;;;
;;; Example:
;;;
;;;   If the point before the "int" in the following
;;;
;;;     int
;;;     main (int argc, char **argv)
;;;     {
;;;
;;;   then "C-:" will insert this
;;;
;;;     /*
;;;      * Function: main
;;;      *
;;;      * Inputs:
;;;      *  argc -
;;;      *  argv -
;;;      *
;;;      * Outputs:
;;;      *
;;;      *
;;;      *
;;;      */
;;;
;;;     int
;;;     main (int argc, char **argv)
;;;     {
;;;
;;; Code:

(require 'cc-mode)

(defun my-cc-inside-comment ()
  "Decides if the current point is inside of a C or C++ comment.
If it is
returns a prefix string that can be used to continue the comment properly.
Returns nil if not inside a comment"
  (let ((here (point))
	(prefix nil))
    (or

     ;; First, try comment block made up of '//'. Only do so if we are in C++
     ;; mode though.
     ;;
     (save-excursion
       (and (eq major-mode 'c++-mode)
            (or (beginning-of-line) t)

            ;; Handle two possibilities - one where POINT is within a C++
            ;; comment, and where the previous line indicates a C++ comment.
            ;;
            (and (looking-at "\\(\\s *//\\)\\(\\s *\\)?")
                 (setq prefix (buffer-substring (match-beginning 0)
                                                (match-end 0)))
                 (or (<= (match-end 1) here)
                     (progn
                       (forward-line -1)
                       (looking-at "\\s *//\\s *"))
                     (setq prefix nil)))))

     ;; Next, try traditional C++ comment block (/* */).
     ;;
     (save-excursion
       (and

        ;; Locate start of comment
        ;;
        (re-search-backward "\\(/[*]\\)\\|\\([*]/\\)" (point-min) t 1)

        ;; Are we at the start? If so, go back to original point and get
        ;; prefix.
        ;;
        (looking-at "/[*]")
        (progn
          (goto-char here)
          (re-search-backward "^\\s */?[*]\\s-*"))

        ;; Make prefix string by generating enough space characters to reach
        ;; the current column and appending the existing comment start prefix
        ;; to it. If present, change the '/*' sequence to ' *'.
        ;;
        (setq prefix (buffer-substring (match-beginning 0) (match-end 0))
              here (length prefix))
        (if (= (aref prefix (- here 2)) ?/)
            (aset prefix (- here 2) ? )))))
    prefix))

(defun my-cc-newline-and-indent ()
  "Front-end to usual `newline-and-indent' function.
For use in C/C++ code with
block-style comments. It properly adds whatever prefix it finds in previous
lines of a comment block whenever a new line is created."
  (interactive)
  (let ((prefix (my-cc-inside-comment)))

    ;; Either insert a comment for the prefix block, or just a newline and
    ;; indent.
    ;;
    (if prefix
        (progn
          (open-line 1)
          (forward-line 1)
          (insert prefix))

      ;; Not in a comment - just do usual newline with indentation.
      ;;
      (newline-and-indent))))

(defun my-cc-refill-block-comment ()
  "Routine to refill a C block comment.
This leaves alone the '/*' and '*/'
comment delimiters if they exist on a line by themselves."
  (interactive)
  (save-excursion
    (let ((here (point))
          (oldp fill-prefix)
          (rmin 0)
          (rmax 0)
          (prefix (my-cc-inside-comment)))

      ;; Only fill region if we find ourselves in a C or C++ comment block. We
      ;; first check for C++ // block comment.
      ;;
      (and prefix
           (or (and (eq major-mode 'c++-mode)
                    (progn
                      (beginning-of-line)
                      (looking-at "^\\s *//"))
                    (progn

                      ;; Work backwards until we find a non-comment line.
                      ;; Remember the location of the firt block comment line.
                      ;;
                      (save-excursion
                        (beginning-of-line)
                        (while
                            (if (looking-at "^\\s *//")
                                (= 0 (forward-line -1))
                              (forward-line 1)
                              nil))

                        ;; Don't fill the start of the comment block if it does
                        ;; not have any text in it.
                        ;;
                        (if (looking-at "^\\s *//\\s *$")
                            (forward-line 1))
                        (setq rmin (point)))

                      ;; Work forwards until we find a non-comment line.
                      ;; Remember the location of the last block comment line.
                      ;;
                      (save-excursion
                        (beginning-of-line)
                        (forward-line 1)
                        (while
                            (if (looking-at "^\\s *//")
                                (= 0 (forward-line 1))
                              (forward-line -1)
                              nil))

                        ;; Don't fill the end of the comment block if it does
                        ;; not have any text in it.
                        ;;
                        (if (looking-at "^\\s *//\\s *$")
                            ()
                          (end-of-line))
                        (setq rmax (point)))
                      t))

               ;; See if we are inside a C comment. Search backward for comment
               ;; start or end. Inside comment if we find start.
               ;;
               (and (re-search-backward "/[*]" (point-min) t 1)

                    ;; If there is no text following the start, move to the
                    ;; next line.
                    ;;
                    (progn
                      (beginning-of-line)
                      (if (not (looking-at "\\s */[*]\\s *$"))
                          (setq rmin (point))
                        (forward-line 1)
                        (setq rmin (point)))
                      (goto-char (match-end 0)))

                    ;; Search for the end of the comment.
                    ;;
                    (re-search-forward "[*]/" (point-max) t 1)
                    (progn
                      (setq rmax (goto-char (match-beginning 0)))
                      (beginning-of-line)

                      ;; If there is no text before the end, then we fill
                      ;; before it.
                      ;;
                      (if (looking-at "^\\s *[*]/")
                          (setq rmax (point)))
                      t)))

           ;; One of the above AND clauses returned true - one final check to
           ;; see if we have a valid region to refill.
           ;;
           (and (<= rmin here)
                (<= here rmax)
                (progn
                  (setq fill-prefix prefix)
                  (fill-region rmin rmax nil t nil)
                  (setq fill-prefix oldp)))))))

(defun my-cc-do-auto-fill ()
  "Function that handles auto filling comment blocks."
  (let ((here (point))
        (prefix nil)
        (mark (make-marker)))

    ;; Do regular auto fill.
    ;;
    (do-auto-fill)
    (and (not (eq major-mode 'c++-mode))
         (not (= (point) here))
         (setq prefix (my-cc-inside-comment))

         ;; Inside of comment, so remove whatever was previously added,
         ;; and insert the prefix for the comment block.
         ;;
         (progn
           (set-marker mark (point))
           (beginning-of-line 1)
           (if (looking-at "[ \t]+")
               (progn
                 (delete-region (match-beginning 0) (match-end 0))
                 (insert-before-markers prefix)
                 (goto-char mark)
                 (set-marker mark nil)))))))

(defun my-cc-get-proc-name (arg)
  "Extracts the name of the function.
The point is currently in, or ARG previous
functions. Name is returned as a string. Returns NIL if function not found."
  (save-excursion

    ;; Goto beginning of current or ARG previous functions
    ;;
    (and (beginning-of-defun (or arg 1))
         (= 0 (forward-line -1))        ; fail if at beginning of buffer
         (looking-at "^\\<\\((\\sw+\\)\\>") ; are we looking at an identifier?
         (buffer-substring (match-beginning 1) (match-end 1)))))

(defun my-cc-get-proc-args ()
  "Extracts arguments to function the point is current in.
Returns the argument names as a list."
  (save-excursion
    (let ((plist nil)
          (count 0)
          args
          limit)

      (and (search-forward "(" nil t)
	   (re-search-backward "[A-Za-z0-9_:<>]+" nil t)
	   (setq plist (list (buffer-substring (match-beginning 1)
					       (match-end 1)))
		 args (match-beginning 2)
		 limit (match-end 0))


      ;; Look for start of function header. First match set will be the
      ;; function name.
      ;;
      (re-search-backward "\\(\\([A-Za-z0-9:_]+\\)\\|\\(operator [^(]*\\)(\\([^{]*\\)" nil t)


      ;; Get function name.
      ;;
      (setq plist (list (buffer-substring (match-beginning 1)
					  (match-end 1)))
	    args (match-beginning 2)
	    limit (match-end 0))

           ;; Insert t or nil depending on the return value being void
           ;;
           (progn
             (forward-line -1)
             (nconc plist (list (looking-at "^\\(\\(static\\)? *void\\)?$"))))

           ;; Place dummy arg counter in list
           ;;
           (nconc plist (list 0))

           ;; Move to first argument and set limit for following loop
           ;;
           (goto-char args)

           ;; Keep extracting argument names until there are no more left
           ;;
           (while
               (and
                (re-search-forward "\\([A-Za-z0-9_]+\\)[[] ]*\\([,\)]\\)"
                                   limit t)
                (setq count (1+ count))
                (nconc plist (list (buffer-substring (match-beginning 1)
                                                     (match-end 1))))
                (string= "," (buffer-substring (match-beginning 2)
                                               (match-end 2))))))

      ;; Update argument count
      ;;
      (if plist
          (setcar (cdr (cdr plist)) count))

      ;; Return resulting list of argument names
      ;;
      plist)))

(defvar my-cc-header-comment-fields nil
  "Variable containing the field locations for the last comment."
  )

(defvar my-cc-header-comment-field-index 0
  "Variable indicating which field is current."
  )

(make-variable-buffer-local 'my-cc-header-comment-fields)
(make-variable-buffer-local 'my-cc-header-comment-field-index)

(defun my-cc-new-header-comment ()
  "Simple routine for inserting a new C function header comment.
Inserts the function's name it precedes, and individual lines for
each argument it takes."
  (interactive)
  (let (
        (plist (my-cc-get-proc-args))
        (rvalue nil)
        marker
        )
    (insert "/*")                       ; start of C block comment
    (my-cc-newline-and-indent)          ; new line and continued comment
    (insert " Function: ")              ; insert function name
    (setq marker (make-marker))
    (set-marker marker (point))
    (cond (plist
           (insert (car plist))
           (setq plist (cdr plist))
           (setq rvalue (car plist))
           (setq plist (cdr plist))
           ))
    ;;
    ;; Get rid of any old markers
    ;;
    (if my-cc-header-comment-fields
        (mapc (lambda (f)
                (and (markerp f)
                     (set-marker f nil)))
              my-cc-header-comment-fields))

    (setq my-cc-header-comment-fields (list (+ (if plist (car plist) 1) 3)))
    (setq my-cc-header-comment-field-index 1)
    (nconc my-cc-header-comment-fields (list marker))

    (setq plist (cdr plist))
    (my-cc-newline-and-indent)          ; new line and continued comment
    (my-cc-newline-and-indent)          ; ditto
    (insert "Inputs:")                  ; start of argument section
    (my-cc-newline-and-indent)
    (backward-delete-char 1)            ; remove space after '*'...
    (insert "\t")                       ; and replace with a TAB
    (cond (plist                        ; insert argument(s)
           (while plist
             (if (string-equal (car plist) "void") (insert "none")
               (insert (car plist) " - "))
             (setq marker (make-marker))
             (set-marker marker (point))
             (nconc my-cc-header-comment-fields (list marker))
             (setq plist (cdr plist))
             (my-cc-newline-and-indent)))
          (t
           (setq marker (make-marker))
           (set-marker marker (point))
           (nconc my-cc-header-comment-fields (list marker))
           (my-cc-newline-and-indent)))
    (backward-delete-char 1)            ; end of argument section
    (my-cc-newline-and-indent)
    (insert " Outputs:")                ; start of output section
    (my-cc-newline-and-indent)
    (backward-delete-char 1)
    (insert "\t")
    (if rvalue
        (insert "none"))
    (setq marker (make-marker))
    (set-marker marker (point))
    (nconc my-cc-header-comment-fields (list marker))
    (my-cc-newline-and-indent)
    (backward-delete-char 1)            ; start of explanatory text
    (my-cc-newline-and-indent)
    (insert " ")
    (setq marker (make-marker))
    (set-marker marker (point))
    (nconc my-cc-header-comment-fields (list marker))
    (my-cc-newline-and-indent)
    (backward-delete-char 1)
    (insert "/")                        ; end of C block comment
    (newline-and-indent)
    (goto-char (nth 1 my-cc-header-comment-fields))
    )
  )

(defun my-cc-header-comment-next-field ()
  "Go to next header field."
  (interactive)
  (let (
        (next (nth (1+ my-cc-header-comment-field-index)
                   my-cc-header-comment-fields))
        )
    (if next
        (setq my-cc-header-comment-field-index
              (1+ my-cc-header-comment-field-index))
      (setq my-cc-header-comment-field-index 1
            next (nth 1 my-cc-header-comment-fields)))
    (goto-char next)
    ))

(defun my-cc-header-comment-prev-field ()
  "Go to prev header field."
  (interactive)
  (let (
        prev
        )
    (if (= 1 my-cc-header-comment-field-index)
        (setq my-cc-header-comment-field-index
              (car my-cc-header-comment-fields)
              prev (nth my-cc-header-comment-field-index
                        my-cc-header-comment-fields))
      (setq my-cc-header-comment-field-index
            (1- my-cc-header-comment-field-index)
            prev (nth my-cc-header-comment-field-index
                      my-cc-header-comment-fields)))
    (goto-char prev)
    ))

(defun my-cc-new-block-comment ()
  "Simple function for inserting a new C block comment.
I originally had a macro, but that was sloooow."
  (interactive)
  (let (here)
    (cond
     ((eq major-mode 'c++-mode)
      (insert "//")
      (my-cc-newline-and-indent)
      (insert " ")
      (setq here (point))
      (my-cc-newline-and-indent)
      (backward-delete-char 1)
      )
     (t
      (insert "/*")                     ; start of C block comment
      (my-cc-newline-and-indent)        ; new line and continued comment
      (insert " ")
      (setq here (point))               ; remember point to go back to
      (my-cc-newline-and-indent)        ; new line and continued comment
      (backward-delete-char 1)
      (insert "/")                      ; end of C block comment
      ))
    (goto-char here)                    ; go back to place for comment text
    ))

(defun my-cc-block-comment-install ()
  "Set up current C/C++ mode to use block comment functions."

  (auto-fill-mode 1)
  (setq auto-fill-function 'my-cc-do-auto-fill)
  (local-set-key [(meta control \;)] 'my-cc-new-block-comment)
  (local-set-key [(control :)] 'my-cc-new-header-comment)
  (local-set-key [(meta q)] 'my-cc-refill-block-comment))

(provide 'my-cc-block-comment)
;;; my-cc-block-comment.el ends here

;;; my-functions.el --- useful free functions -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;;
;;; Utility functions. For speed, minimize the number of `require' statements; use `autoload' as much as possible.
;;;
;;; Code:

(require 'corfu)
(require 'crm)

;;;###autoload
(defun my/beginning-of-line-text (arg)
  "Augment behavior of `beginning-of-line-text'.

After calling `beginning-of-line' and ARG is 1 (or nil), call
`beginning-of-line-text' again and see if point moved. If not, invoke
`beginning-of-line' to move to start of line."
  (interactive "^p")
  (or arg (setq arg 1))
  (when (/= arg 1)
    (beginning-of-line-text arg))
  (let ((orig (point)))
    (beginning-of-line-text 1)
    (when (= orig (point))
      (beginning-of-line 1))))

(autoload 'compile-multi "compile-multi")

;;;###autoload
(defun my/replace-regionw-with-calc-eval (start end)
  "Treat text in range START END as calc expression.
Replace selection with evaluation result."
  (interactive "r")
  (when (and start end)
    (let* ((eq (buffer-substring-no-properties start end))
           (result (calc-eval eq)))
      (if (stringp result)
          (replace-region-contents start end result)
        (message "%s" (cadr result))))))

;;;###autoload
(defun my/compile ()
  "Invoke `compile-multi' or `compile' depending on context."
  (interactive)
  (if (and (fboundp 'compile-multi--config-tasks)
           (compile-multi--config-tasks))
      (call-interactively #'compile-multi)
    (call-interactively #'compile)))

;;;###autoload
(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (when-let* ((filename (if (equal major-mode 'dired-mode)
                            default-directory
                          (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

;;;###autoload
(defun my/describe-symbol-at-point ()
  "Immediately show help for symbol at point if it exists.
If help buffer is visible and it is showing help for the
symbol, then hide it."
  (interactive)
  (describe-symbol (symbol-at-point) (help-buffer))
  (select-window (get-buffer-window (help-buffer))))

;;;###autoload
(defun my/display-prefix (arg)
  "Display the value of the raw prefix ARG.
For debugging purposes."
  (interactive "P")
  (message "%s" arg))

;;;###autoload
(defun my/dump-hashtable (hashtable)
  "Show the contents of HASHTABLE."
  (interactive "Xhash table: ")
  (when (hash-table-p hashtable)
    (let ((tmp (get-buffer-create "*dump*")))
      (switch-to-buffer-other-window tmp)
      (erase-buffer)
      (maphash (lambda (key value)
                 (insert key " -> " value "\n")) hashtable))))

;;;###autoload
(defun my/goto-mark ()
  "Move back to mark without enabling transient mode."
  (interactive)
  (set-mark-command 4))

(defun my/hms-to-dec (lat-lon &optional allow-neg)
  "Convert latitude and logitude LAT-LON into two decimal values.
A latitude and longitude consists of three (3) components: degrees,
minutes, and seconds. All are floating point values but they have
different suffixes. Degrees end with `°', minutes end with `\'' and
seconds end with `\"' followed by a direction indicator: `N/S' for
latitude and `E/W' for longitude. If ALLOW-NEG is not nil, then
direction indicators are not present; instead the sign of the degrees
value indicates the direction.

Examples:
- (hms-to-dec \"37°26'36.42\"N 06°15'14.28\"W\") -> (37.44345 -6.25396666667).
- (hms-to-dec \"37°26'36.42\" -06°15'14.28\"\", t) -> (37.44345 -6.25396666667)."
  (let* ((args-len (if allow-neg 3 4))
         (args-list (seq-partition (split-string lat-lon "[°'\" ]" t) args-len))
         (len-list (mapcar 'length args-list)))
    (when (or (/= (length len-list) 2)
              (any (lambda (len) (/= len args-len)) len-list))
      (error "Malformed lat-lon value %s" lat-lon))
    (mapcar (lambda (args)
              (seq-let (h m s d) args
                (string-to-number (calc-eval (format "deg(%s@ %s' %s\")*%d" h m s
                                                     (if (member d '("S" "W")) -1 1))))))
            args-list)))

;;;###autoload
(defmacro my/hook (name &rest code)
  "Create a custom hook with NAME and CODE.
Returns NAME as a function symbol.

TODO: intent was to use this in a `:hook' feature of `use-package', but
this does not work right now, and I suspect it is due to the internal
machinery of `use-package'."
  `(progn
     (defun ,name ()
       "Custom hook."
       ,@code)
     (function ,name)))

;;;###autoload
(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun my/is-valid-directory (dir)
  "Check if DIR is valid, returning it if so or nil if not.
Note that `file-directory-p' returns t if the (string) length of DIR is
zero (0), so we detect that and report that as false."
  (and (> (length dir) 0)
       (file-directory-p dir)
       dir))

(autoload 'magit-status-setup-buffer "magit-status")
(autoload 'my/init-files "my-env")

;;;###autoload
(defun my/magit-init-files ()
  "Show magit buffer for `init-files' repo."
  (interactive)
  (magit-status-setup-buffer (my/init-files)))

;;;###autoload
(defun my/mark-line (&optional arg)
  "Blah blah ARG blah."
  (interactive "p")
  (unless mark-active
    (beginning-of-line)
    (push-mark)
    (setq mark-active t))
  (forward-line arg))

;;;###autoload
(defun my/matching-paren ()
  "When point is on a paren-type character, jump to its twin.
If just after closing paren move to it and then jump."
  (interactive)
  (cond ((looking-at " ?[[({]" t)
	 (forward-sexp 1)
	 (forward-char -1))
	((or (looking-at "[]})]" t)
             (save-excursion            ; check one character back
               (forward-char -1)
               (looking-at "[]})]" t)))
	 (forward-char 1)
	 (forward-sexp -1))
	(t
	 nil)))

;;;###autoload
(defun my/reload-buffer ()
  "Reload the current buffer from disk.
Checks to see if buffer needs saving, aborting the reload if changes not saved.
Useful for Elisp content or changes to mode settings."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and (not buffer-read-only)
               filename
               (or (not (buffer-modified-p))
                   (and (string= "yes" (read-answer "Save changes? "
                                                    '(("yes" ?y "save buffer")
                                                      ("quit" ?q "abort"))))
                        (progn (save-buffer) t))))
      (find-alternate-file filename)
      (message "Reloaded."))))

;;;###autoload
(defun my/remove-all-text-properties ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

;;;###autoload
(defun my/set-mark-no-activate ()
  "Push `point' to `mark-ring' but does not activate the region."
  (interactive)
  (push-mark (point) t nil))

;;;###autoload
(defun my/sort-lines-by-integer-key (pattern &optional direction)
  "Sort lines by an integer value that is found via PATTERN in a line.
The sort is in increasing numerical order if DIRECTION is nil; otherwise
it is in descending order."
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr direction
                   #'forward-line       ; NEXTRECFUN
                   #'end-of-line        ; ENDRECFUN
                   (lambda ()                ; STARTKEYFUN -- returns numeric key
                     (when (looking-at pattern)
                       (string-to-number (match-string 0))))
                   nil                  ; ENDKEYFUN
                   nil)))))             ; PREDICATE

;;;###autoload
(defun my/sort-lines-by-leading-integer (arg)
  "Sort lines in current region by extracting integer values from start of line.
If ARG is not nil, sort in descending order.
Nothing fancy about parsing, it just matches any number at the beginning of
the line, ignoring any whitespace characters. If that fails, then the sort
will treat the whole line as a value to compare against."
  (interactive "P")
  (my/sort-lines-by-integer-key "^\\s *[0-9]+" arg))

;;;###autoload
(defun my/trusted-content-p (original-response)
  "Advice for `trusted-content-p' to trust the `*scratch*' buffer.
Honors ORIGINAL-RESPONSE when not nil and then checks the buffer's name
if it is `*scratch*'. This is a loosening of security but the risk is
very small for me, and it remove the obnoxious message at startup about
the buffer having untrusted content."
  (or original-response
      (buffer-name "*scratch*")))

(defun my/launch-ssh-emacs (host)
  "Launch X11 Emacs on remote HOST."
  (interactive)
  (message "Starting Emacs on %s..." host)
  (start-process
   (concat "ssh-" host)
   (concat " *ssh-" host "*")
   "/usr/bin/ssh"
   "-Y"
   host
   "exec /opt/homebrew/bin/emacs"))

(defun my/crm-indicator(args)
  "Custom CRM indicator for ARGS."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                (car args))
        (cdr args)))

(defun my/after-init-emacs-init ()
  "Custom Emacs init."
  (setq read-process-output-max (* 64 1024 1024)
	process-adaptive-read-buffering nil
        ;; debug-on-error t
        frame-title-format (let ((buffer-directory '(:eval (abbreviate-file-name default-directory))))
                             (if (not (display-graphic-p))
                                 (list (concat (system-name) " ") buffer-directory)
                               buffer-directory))
        load-path-filter-function #'load-path-filter-cache-directory-files)
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)
  (abbrev-mode)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror)))

(autoload 'cape-dabbrev "cape")
(autoload 'cape-file "cape")
(autoload 'cape-elisp-block "cape")

;;;###autoload
(defun my/cape-initialize ()
  "Initialize cape."
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(autoload 'global-corfu-mode "corfu")
(autoload 'corfu-popupinfo-mode "corfu-popupinfo")

;;;###autoload
(defun my/corfu-initialize ()
  "Custom corfu initialize."
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(autoload 'diff-hl-flydiff-mode "diff-hl-flydiff")
(autoload 'global-diff-hl-mode "diff-hl")
(autoload 'global-diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk")

;;;###autoload
(defun my/diff-hl-initialize()
  "Custom diff-hl init."
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode t)
  (global-diff-hl-show-hunk-mouse-mode t))

(autoload 'global-ligature-mode "ligature")
(autoload 'ligature-set-ligatures "ligature")

;;;###autoload
(defun my/ligature-initialize ()
  "Custom ligature init."
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://" "www"))
  (global-ligature-mode t))

(autoload 'marginalia-mode "marginalia")
(autoload 'nerd-icons-completion-mode "nerd-icons-completion")

;;;###autoload
(defun my/marginalia-initialize ()
  "Custom marginalia init."
  (marginalia-mode 1)
  (nerd-icons-completion-mode 1))

(autoload 'nerd-icons-corfu-formatter "nerd-icons-corfu")

;;;###autoload
(defun my/nerd-icons-corfu-initialize ()
  "Custom nerd-icons-corfu init."
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(autoload 'popper-mode "popper")
(autoload 'popper-echo-mode "popper-echo")

;;;###autoload
(defun my/popper-initialize ()
  "Custom popper init."
  (popper-mode)
  (popper-echo-mode))

(autoload 'ultra-scroll-mode "ultra-scroll")

;;;###autoload
(defun my/ultra-scroll-initialize ()
  "Custom `ultra-scroll' init."
  (setq scroll-conservatively 3
        scroll-margin 0)
  (ultra-scroll-mode 1))

(provide 'my-functions)

;;; my-functions.el ends here.

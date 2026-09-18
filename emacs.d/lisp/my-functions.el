;;; my-functions.el --- useful free functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'my-customizations)
(require 'my-env)
(require 'my-constants)
(require 'ace-window)
(require 'consult-notes)
(require 'crux)
(require 'emacs-pager)
(require 'popper)
(require 'project)
(require 'wid-edit)

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

(defun my/trusted-content-p (original-response)
  "Advice for `trusted-content-p' to trust the `*scratch*' buffer.
Honors ORIGINAL-RESPONSE when not nil and then checks the buffer's name
if it is `*scratch*'. This is a loosening of security but the risk is
very small for me, and it remove the obnoxious message at startup about
the buffer having untrusted content."
  (or original-response
      (buffer-name "*scratch*")))

(defun my/ace-window-always-dispatch ()
  "Invoke `ace-window' after setting `aw-dispatch-always' to T.
When `aw-dispatch-always' is nil, `ace-window' does not invoke
its dispatching mechanism if there are 2 or fewer windows. This
command guarantees that dispatching will always happen."
  (interactive)
  (let ((current-aw-dispatch-always aw-dispatch-always))
    (unwind-protect
        (let ((aw-dispatch-always t))
          (call-interactively #'ace-window))
      (setq aw-dispatch-always current-aw-dispatch-always))))

(defun my/do-next-window (wins)
  "Jump to next window in WINS after the current one."
  (when-let* ((current-window (get-buffer-window))
              (current-index (seq-position wins current-window #'eq))
              (next-index (and current-index (1+ current-index)))
              (final-index (if my/next-window-wrap-around
                               (% next-index (length wins))
                             (and (length> wins next-index) next-index))))
    (aw-switch-to-window (nth final-index wins))))

(defun my/ace-window-next ()
  "Jump to next window according to `ace-window'."
  (interactive)
  (my/do-next-window (aw-window-list)))

(defun my/ace-window-previous ()
  "Jump to previous window according to `ace-window'."
  (interactive)
  (my/do-next-window (reverse (aw-window-list))))

(defun my/next-buffer-skip-filter (_ buffer bury-or-kill)
  "Return t if BUFFER should be skipped in WINDOW.
This is used by `my/prev-buffer-current-window' and
`my/next-buffer-current-window' methods so that only desired
buffers will be available for changing to in the current
window. If BURY-OR-KILL is not nil then the operation will result
in the buffer being buried or killed, and in this case buffers
are never filtered out. Otherwise, skip buffers that start with
'*' in their name, `dired' buffers, `help' buffers, and buffers
that are already visible somewhere."
  (if bury-or-kill
      nil
    ;; Taken from http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html
    (with-current-buffer buffer
      (cond
       ((string-match "^\*" (buffer-name)) t)
       ((eq major-mode 'dired-mode) t)
       ((eq major-mode 'help-mode) t)
       ((get-buffer-window nil 'visible) t)
       (t nil)))))

(defun my/next-buffer-current-window ()
  "Switch to `next' buffer in current window with filtering.
Only switch to a buffer that passes the filter defined in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (next-buffer)))

(defun my/prev-buffer-current-window ()
  "Switch to `previous' buffer in current window with filtering.
Only switch to a buffer that passes the filter defined in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (previous-buffer)))

(defun my/ace-window-one-command ()
  "Run an action in a chosen window.
Taken from https://karthinks.com/software/emacs-window-management-almanac/#window-magic-with-ace-window-dispatch."
  (interactive)
  (when-let* ((aw-dispatch-always t)
              (win (aw-select " ACE"))
              (windowp win))
    (with-selected-window win
      (let* ((command (key-binding
                       (read-key-sequence
                        (format "Run in %s..." (buffer-name)))))
             (this-command command))
        (call-interactively command)))))

(defun my/display-buffer-pre-func (buffer alist)
  "Method to use for `display-buffer-overriding-action'.
The BUFFER and ALIST are ignored."
  (let* ((_ (cons buffer alist))
         (type 'reuse)
         (aw-dispatch-always t)
         (window (aw-select (propertize " ACE" 'face 'mode-line-highlight))))
    (cons window type)))

(defun my/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command #'my/display-buffer-pre-func nil "[ace-window]")
  (message "Command to execute: "))

(defun crux-find-current-directory-dir-locals-file (find-2)
  "Edit the `.dir-locals.el' file for the current buffer in another window.
If prefix arg FIND-2 is set then edit the `.dir-locals-2.el' file instead
of `.dir-locals.el'. Scans parent directories if the file does not exist in
the default directory of the current buffer. If not found, create a new,
empty buffer in the current buffer's default directory, or if there is no
such directory, in the user's home directory."
  (interactive "P")
  (let* ((prefix (if (eq system-type 'ms-dos) "_" "."))
         (file (concat prefix (if find-2 "dir-locals-2" "dir-locals") ".el"))
         (starting-dir (or (when (and default-directory
                                      (file-readable-p default-directory))
                             default-directory)
                           (file-truename "~/")))
         (found-dir (or (locate-dominating-file starting-dir file) starting-dir))
         (found-file (concat found-dir file)))
    (find-file-other-window found-file)
    (if (file-exists-p found-file)
        (message "Editing existing file %s" found-file)
      (message "Editing new file %s" found-file))))

(defun my/show-project-menu ()
  "Show the menu that is shown when switching to a new project."
  (interactive)
  (call-interactively (project--switch-project-command)))

;; My own version of some `crux` routines that use `find-file` instead of `find-file-other-window`
(defun my/find-user-init-file (arg)
  "Edit the `user-init-file` when ARG is nil.
Otherwise, edit the `early-init.el' file instead, creating it if
necessary."
  (interactive "P")
  (find-file (locate-user-emacs-file (if arg "early-init.el" user-init-file))))

(defun my/find-user-custom-file ()
  "Edit the `custom-file` if it exists."
  (interactive)
  (if custom-file
      (find-file custom-file)
    (message "No custom file defined.")))

(defun my/find-shell-init-file ()
  "Edit a shell init file."
  (interactive)
  (let* ((shell (file-name-nondirectory (getenv "SHELL")))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar #'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file (completing-read "Choose shell init file: " candidates))
      (find-file (car candidates)))))

(defun my/dump-hashtable (hashtable)
  "Show the contents of HASHTABLE."
  (interactive "Xhash table: ")
  (when (hash-table-p hashtable)
    (let ((tmp (get-buffer-create "*dump*")))
      (switch-to-buffer-other-window tmp)
      (erase-buffer)
      (maphash (lambda (key value)
                 (insert key " -> " value "\n")) hashtable)
      (emacs-pager-mode))))

(defun my/reload-buffer ()
  "Reload the current buffer from disk.
Checks to see if buffer needs saving, aborting the reload if changes not saved."
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

(defun my/run-something-in-buffer (name buffer-setup-proc run-proc)
  "Run RUN-PROC after running BUFFER-SETUP-PROC in NAME buffer.
This function receives the buffer to use for the shell. The expectation
is that the function will setup the display environment to host the
buffer."
  (let ((cwd default-directory)
        (tmp (get-buffer-create name)))
    (funcall buffer-setup-proc tmp)
    (cd-absolute cwd)
    (funcall run-proc tmp)))

(defun my/in-current-window (buf)
  "Switch to buffer BUF in current window."
  (switch-to-buffer buf nil t))

(defun my/in-other-window (buf)
  "Switch to buffer BUF in other window."
  (switch-to-buffer-other-window buf))

(defun my/in-other-frame (buf)
  "Switch to buffer BUF in new frame."
  (select-frame (make-frame))
  (switch-to-buffer buf))

(defun my/run-shell (buffer-setup-proc)
  "Run a new `shell' after running BUFFER-SETUP-PROC.
This function receives the buffer to use for the shell. The expectation
is that the function will setup the display environment to host the
buffer."
  (my/run-something-in-buffer "*Shell*"
                              buffer-setup-proc
                              (lambda (buf) (shell buf))))

(defun my/shell ()
  "Start a new shell."
  (interactive)
  (my/run-shell #'my/in-current-window))

(defun my/shell-other-window ()
  "Start a new shell in another window."
  (interactive)
  (my/run-shell #'my/in-other-window))

(defun my/shell-other-frame ()
  "Start a new shell in another frame."
  (interactive)
  (my/run-shell #'my/in-other-frame))

(defun my/bury-or-kill-current-buffer ()
  "Bury or kill the current buffer without asking. (WIP)
Kill buffers that match the pattern '*...*'.
Otherwise just bury them."
  (interactive)
  (if (and (string-match-p "\\*\\(?:help\\|grep\\|Completions\\|Compile-Log\\|Man .*\\|eldoc)\\|shell .*\\*"
                           (buffer-name (current-buffer)))
           (not (get-buffer-process (current-buffer))))
      (progn
        (message "Killed buffer")
        (kill-buffer (current-buffer)))
    (message "Buried buffer")
    (bury-buffer (current-buffer))))

(defun my/bury-current-buffer ()
  "Bury the current buffer without asking."
  (interactive)
  (bury-buffer (current-buffer)))

(defun my/kill-current-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/info-other-frame ()
  "Show Info in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*info*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (info nil tmp)))

(defun my/customize-other-window ()
  "Show Customize in a new window."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (switch-to-buffer-other-window tmp)
    (customize)))

(defun my/consult-notes-other-frame ()
  "Find note to show in a new frame."
  (interactive)
  (select-frame (make-frame))
  (consult-notes))

(defun my/remove-all-text-properties ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(defun my/matching-paren ()
  "When point is on a paren-type character, jump to its twin."
  (interactive)
  (cond ((looking-at "[[({]")
	 (forward-sexp 1)
	 (forward-char -1))
	((looking-at "[]})]")
	 (forward-char 1)
	 (forward-sexp -1))
	(t
	 nil)))

(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (when-let* ((filename (if (equal major-mode 'dired-mode)
                            default-directory
                          (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(defun my/repl ()
  "Simple alias to start ielm."
  (interactive)
  (ielm))

(defun my/repl-other-window ()
  "Start a new repl in another window."
  (interactive)
  (let ((tmp (get-buffer-create "*ielm*")))
    (switch-to-buffer-other-window tmp)
    (ielm)))

(defun my/describe-symbol-at-point ()
  "Immediately show help for symbol at point if it exists.
If help buffer is visible and it is showing help for the
symbol, then hide it."
  (interactive)
  (let ((what (symbol-name (symbol-at-point)))
        (help-window (get-buffer-window (help-buffer))))
    (if (and help-window
             (save-current-buffer
               (set-buffer (help-buffer))
               (goto-char (point-min))
               (looking-at what)))
        (popper--delete-popup help-window)
      (describe-symbol (symbol-at-point) (help-buffer)))))

(defun my/htop ()
  "Run htop in a term buffer."
  (interactive)
  (let ((name "*htop*")
        (cmd (if my/is-macosx "sudo htop" "/bin/htop")))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term cmd name))))

(defun my/top ()
  "Run top in a term buffer."
  (interactive)
  (let ((name "*top*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "/usr/bin/top" name))))

(defun my/git-sync (host path)
  "Execute a git pull on HOST in PATH."
  (interactive)
  (message "Running git-pull on %s:%s..." host path)
  (let* ((git (list "cd" path "&&"
                    "git" "stash" "push" "&&"
                    "git" "pull" "&&"
                    "git" "checkout" "'stash@{0}'" "emacs.d/places" "emacs.d/recentf" "&&"
                    "git" "stash" "drop"))
         (cmd (if host
                  (append (list "/usr/bin/ssh" "-tt" host) git)
                (append '("bash") git)))
         (name (concat "<" (or host "localhost") "|" path ">"))
         (args (append (list name my/git-sync-buffer-name) cmd))
         (proc (apply 'start-process args)))
    (add-function :around (process-filter proc)
                  (lambda (filt proc content)
                    (funcall filt proc (concat (process-name proc) ": " content))))
    proc))

(defun my/all-git-sync ()
  "Sync the configurations repo found in various locations at work."
  (interactive)
  (when-let* ((buf (get-buffer my/git-sync-buffer-name)))
    (kill-buffer buf))
  (let ((local (file-name-concat my/repos "configurations"))
        (home (file-truename "~/configurations")))
    ;; NOTE: treat `(nil local)` as the master and only update via magit
    (dolist (cfg (list (cons nil home)  ; /lxhome/howesbra/configurations
                       (cons "ldzls2164i" home)  ; vnc
                       (cons "ldzls2164i" local) ; vnc
                       (cons "nyzls1514n" local) ; internal ogsd / pickaxe
                       (cons "nyzls1644q" local) ; wolverine QA
                       (cons "nyzls1646q" local) ; raze QA
                       (cons "nyzls2686q" local) ; tcs QA
                       (cons "nyzls105i" local)))  ; logs archive
      (my/git-sync (car cfg) (cdr cfg))))
  (display-buffer my/git-sync-buffer-name))

(defun my/sort-lines-by-leading-integer (arg)
  "Sort lines in current region by extracting integer values from start of line.
If ARG is not nil, sort in descending order.
Nothing fancy about parsing, it just matches any number at the beginning of
the line, ignoring any whitespace characters. If that fails, then the sort
will treat the whole line as a value to compare against."
  (interactive "P")
  (my/sort-lines-by-integer-key "^\\s *[0-9]+" arg))

;; (defun my/launch-qa-emacs (host)
;;   "Launch X11 Emacs on QA HOST."
;;   (interactive)
;;   (message "Starting QA Emacs on %s..." host)
;;   (start-process
;;    "qa-raze"
;;    " *qa-raze*"
;;    "/usr/bin/ssh"
;;    "-Y"
;;    (concat "sp_qa@" host)
;;    ". /apps/home/howesbra/repos/configurations/qa.profile; exec /opt/third/emacs/30.1.1/emacs"))

(defun my/set-mark-deactivate ()
  "Set mark without activating it.
This is just a shortcut for \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (set-mark-command nil)
  (when transient-mark-mode
    (deactivate-mark)))

(defun my/goto-mark ()
  "Move back to mark without enabling transient mode.
This is just a shortcut for \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (set-mark-command 4))

(defun my/display-prefix (arg)
  "Display the value of the raw prefix ARG."
  (interactive "P")
  (message "%s" arg))

(defun my/mark-line (&optional arg)
  "Blah blah ARG blah."
  (interactive "p")
  (unless mark-active
    (beginning-of-line)
    (push-mark)
    (setq mark-active t))
  (forward-line arg))

(defun my/customize-search ()
  "Show the top-level customize screen and move to the search field."
  (interactive)
  (customize)
  (goto-char (point-min))
  (widget-forward 3))

(provide 'my-functions)

;;; my-functions.el ends here.

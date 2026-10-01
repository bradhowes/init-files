;;; my-navigation.el --- custom window/frame navigation routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'ace-window)
(require 'consult-notes)
(require 'my-customizations)
(require 'wid-edit)

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

;;;###autoload
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

;;;###autoload
(defun my/ace-window-next ()
  "Jump to next window according to `ace-window'."
  (interactive)
  (my/do-next-window (aw-window-list)))

;;;###autoload
(defun my/ace-window-one-command ()
  "Run an action in a chosen window.
Taken from https://karthinks.com/software/emacs-window-management-almanac/#window-magic-with-ace-window-dispatch.
NOTE: this seems to be broken now for `M-x' actions."
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

;;;###autoload
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

;;;###autoload
(defun my/ace-window-previous ()
  "Jump to previous window according to `ace-window'."
  (interactive)
  (my/do-next-window (reverse (aw-window-list))))

;;;###autoload
(defun my/bury-current-buffer ()
  "Bury the current buffer without asking."
  (interactive)
  (bury-buffer (current-buffer)))

;;;###autoload
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

;;;###autoload
(defun my/consult-notes-other-frame ()
  "Find note to show in a new frame."
  (interactive)
  (select-frame (make-frame))
  (consult-notes))

;;;###autoload
(defun my/customize-other-window ()
  "Show Customize in a new window."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (switch-to-buffer-other-window tmp)
    (customize)))

;;;###autoload
(defun my/customize-search ()
  "Show the top-level customize screen and move to the search field."
  (interactive)
  (customize)
  (goto-char (point-min))
  (widget-forward 3))

;;;###autoload
(defun my/do-next-window (wins)
  "Jump to next window in WINS after the current one."
  (when-let* ((current-window (get-buffer-window))
              (current-index (seq-position wins current-window #'eq))
              (next-index (and current-index (1+ current-index)))
              (final-index (if my/next-window-wrap-around
                               (% next-index (length wins))
                             (and (length> wins next-index) next-index))))
    (aw-switch-to-window (nth final-index wins))))

;;;###autoload
(defun my/display-buffer-pre-func (buffer alist)
  "Method to use for `display-buffer-overriding-action'.
The BUFFER and ALIST are ignored."
  (let* ((_ (cons buffer alist))
         (type 'reuse)
         (aw-dispatch-always t)
         (window (aw-select (propertize " ACE" 'face 'mode-line-highlight))))
    (cons window type)))

;;;###autoload
(defun my/in-current-window (buf)
  "Switch to buffer BUF in current window."
  (switch-to-buffer buf nil t))

;;;###autoload
(defun my/in-other-frame (buf)
  "Switch to buffer BUF in new frame."
  (select-frame (make-frame))
  (switch-to-buffer buf))

;;;###autoload
(defun my/in-other-window (buf)
  "Switch to buffer BUF in other window."
  (switch-to-buffer-other-window buf))

;;;###autoload
(defun my/info-other-frame ()
  "Show Info in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*info*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (info nil tmp)))

;;;###autoload
(defun my/kill-current-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
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

;;;###autoload
(defun my/next-buffer-current-window ()
  "Switch to `next' buffer in current window with filtering.
Only switch to a buffer that passes the filter defined in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (next-buffer)))

;;;###autoload
(defun my/prev-buffer-current-window ()
  "Switch to `previous' buffer in current window with filtering.
Only switch to a buffer that passes the filter defined in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (previous-buffer)))

;;;###autoload
(defun my/show-messages-buffer ()
  "Quickly show the *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;###autoload
(defun my/show-messages-buffer-other-window ()
  "Quickly show the *Messages* buffer."
  (interactive)
  (switch-to-buffer-other-window "*Messages*"))

(provide 'my-navigation)

;;; my-navigation.el ends here.

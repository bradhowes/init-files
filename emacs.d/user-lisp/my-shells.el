;;; my-shells.el --- custom shell routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-navigation)

;;;###autoload
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

;;;###autoload
(defun my/run-shell (buffer-setup-proc)
  "Run a new `shell' after running BUFFER-SETUP-PROC.
This function receives the buffer to use for the shell. The expectation
is that the function will setup the display environment to host the
buffer."
  (my/run-something-in-buffer "*Shell*"
                              buffer-setup-proc
                              (lambda (buf) (shell buf))))

;;;###autoload
(defun my/shell ()
  "Start a new shell."
  (interactive)
  (my/run-shell #'my/in-current-window))

;;;###autoload
(defun my/shell-other-window ()
  "Start a new shell in another window."
  (interactive)
  (my/run-shell #'my/in-other-window))

;;;###autoload
(defun my/shell-other-frame ()
  "Start a new shell in another frame."
  (interactive)
  (my/run-shell #'my/in-other-frame))

;;;###autoload
(defun my/repl ()
  "Simple alias to start ielm."
  (interactive)
  (ielm))

;;;###autoload
(defun my/repl-other-window ()
  "Start a new repl in another window."
  (interactive)
  (let ((tmp (get-buffer-create "*ielm*")))
    (switch-to-buffer-other-window tmp)
    (ielm)))

(provide 'my-shells)

;;; my-shells.el ends here.

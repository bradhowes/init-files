;;; my-server.el --- custom routines for Emacs server process -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'server)

;;;###autoload
(defun my/start-emacs-server ()
  "Start up an Emacs server to support `emacsclient' connections.
Customize `server-name' so that each Emacs
process has its own server connection."
  (interactive)
  ;; NOTE: `server-running-p` can report `t` even if we are not running it.
  (unless server-process
    ;; Make a unique server connection since I run multiple Emacs instances and I want the emacsclient in a comint
    ;; buffer to connect to the right connection.
    (setq server-name (format "server-%d" (emacs-pid)))
    (setenv "EMACS_SERVER_FILE" server-name)
    (setenv "EMACS_SOCKET_NAME" server-name)
    (server-start)))

(provide 'my-keymaps)

;;; my-server.el ends here.

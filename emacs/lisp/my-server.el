;;; package -- my-server -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Misc functions for managing Emacs server for emasclient access
;;; Code:

(require 'server)

(defun my/start-emacs-server ()
  "Start up an Emacs server to support `emacsclient` connections.
Customize `server-name` so that each Emacs process
has its own server connection."
  (interactive)
  ;; NOTE: `server-running-p` can report `t` even if we are not running it.
  (unless server-process
    ;; Make a unique server connection since I run multiple Emacs instances and I want the emacsclient in a comint
    ;; buffer to connect to the right connection.
    (setq server-name (format "server-%d" (emacs-pid)))
    (setenv "EMACS_SERVER_FILE" server-name)
    (setenv "EMACS_SOCKET_NAME" server-name)
    (server-start)))

(provide 'my-server)
;;; my-server.el ends here

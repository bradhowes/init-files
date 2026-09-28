;;; package -- my-constants -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Runtime constants that describe various properties of the environment.
;;; Code:

(defconst my/is-macosx
  (eq system-type 'darwin)
  "T if running on macOS.
Note that this is also true when running in a terminal window.")

(defconst my/is-linux
  (eq system-type 'gnu/linux)
  "T if running on GNU/Linux system.
Note that this is also true when running in a terminal window.")

(defconst my/is-work
  (string= "bradhowes" user-login-name)
  "This is t if running under work identity.")

(defconst my/font-name
  "Berkeley Mono"
  "The name of the font to use.")

(provide 'my-constants)

;;; my-constants.el ends here

;;; package -- my-shell-mode -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'native-complete)

(defvar my/shell-home-root nil
  "Value to prepend to a directory.")

(defun my/shell-set-home-root ()
  "Locate home root."
  (make-local-variable 'my/shell-home-root)
  (setq my/shell-home-root
        (cond ((> (length (getenv "MSYSTEM")) 0) "C:/msys64")
              (t ""))))

(defun my/shell-mode-hook ()
  "Customize `shell-mode'."

  (message "Loading native-complete-setup-bash")
  (native-complete-setup-bash)

  (rename-uniquely)

  (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8 'utf-8)

  (setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
        shell-dirtrackp nil)            ; disable since using comint-osc-process-output

  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions #'comint-postoutput-scroll-to-bottom))

(provide 'my-shell-mode)
;;; my-shell-mode.el ends here

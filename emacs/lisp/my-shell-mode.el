;;; package -- my-shell-mode -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Code:

(require 'consult)
(require 'corfu)
(require 'shell)
(require 'python)

;; (require 'native-complete)

(defun my/shell-bol ()
  "Move point to the end of a prompt or to beginning of line.
If after a prompt, move to end of it. Otherwise move to
actual beginning of line (same as if there were no prompt)."
  (interactive)
  (comint-bol (= (point) (comint-line-beginning-position))))

(defun my/shell-mode-hook ()
  "Customize `shell-mode'."

  ;; Look for the process that exists for the now-current buffer. Rename buffer to include its process ID.
  (when-let ((buf (current-buffer))
             (found (seq-filter (lambda (p) (eq buf (process-buffer p))) (process-list)))
             (pid (seq-map #'process-id found)))
    (rename-buffer (format "*Shell [%d]*" (car pid))))

  (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8 'utf-8)

  (setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
        comint-process-echoes t
        shell-dirtrackp nil)            ; disable since using comint-osc-process-output

  (ansi-color-for-comint-mode-on)
  (python-pdbtrack-setup-tracking)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions #'comint-postoutput-scroll-to-bottom)

  (buffer-disable-undo)
  
  (keymap-local-set "C-a" #'my/shell-bol)
  (keymap-local-set "M-h" #'consult-history))

(provide 'my-shell-mode)
;;; my-shell-mode.el ends here

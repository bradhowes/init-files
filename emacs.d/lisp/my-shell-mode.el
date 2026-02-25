;;; package -- my-shell-mode -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Code:

(require 'ansi-osc)
(require 'corfu)
(require 'comint)
(require 'consult)
(require 'my-constants)
(require 'python)
(require 'shell)

;; (require 'native-complete)

(defun my/shell-bol ()
  "Move point to the end of a prompt or to beginning of line.
If after a prompt, move to end of it. Otherwise move to
actual beginning of line (same as if there were no prompt)."
  (interactive)
  (comint-bol (= (point) (comint-line-beginning-position))))

(defun my/comint-osc-process-output (_)
  "Interpret OSC escape sequences in comint output.
This function is intended to be added to
`comint-output-filter-functions' in order to interpret escape
sequences of the forms

    ESC ] command ; text BEL
    ESC ] command ; text ESC \\

Specifically, every occurrence of such escape sequences is
removed from the buffer.  Then, if `command' is a key of the
`ansi-osc-handlers' alist, the corresponding value, which
should be a function, is called with `command' and `text' as
arguments, with point where the escape sequence was located."
  (let ((start (1- comint-last-output-start))
        ;; Start one char before last output to catch a possibly stray ESC
        (bound (process-mark (get-buffer-process (current-buffer)))))
    (my/ansi-osc-apply-on-region start bound)))

(defun my/ansi-osc-apply-on-region (begin end)
  "Interpret OSC escape sequences in region between BEGIN and END.
This function searches for escape sequences of the forms

    ESC ] command ; text BEL
    ESC ] command ; text ESC \\

Every occurrence of such escape sequences is removed from the
buffer.  Then, if `command' is a key in the alist that is the
value of the local variable `ansi-osc-handlers', that key's
value, which should be a function, is called with `command' and
`text' as arguments, with point where the escape sequence was
located."
  (save-excursion
    (goto-char (or ansi-osc--marker begin))
    (when (eq (char-before) ?\e) (backward-char))
    (while (re-search-forward "\e]" end t)
      (let ((pos0 (match-beginning 0))
            (code (and (re-search-forward "\\=\\([0-9A-Za-z]*\\);" end t)
                       (match-string 1)))
            (pos1 (point)))
        (if (re-search-forward "\a\\|\e\\\\" end t)
            (let ((text (buffer-substring-no-properties
                         pos1 (match-beginning 0))))
              (setq ansi-osc--marker nil)
              (delete-region pos0 (point))
              (let ((inhibit-message t))
                (message "ansi-osc-apply-on-region - %s %s" code text))
              (when-let ((fun (cdr (assoc-string code ansi-osc-handlers))))
                (funcall fun code text)))
          (put-text-property pos0 end 'invisible t)
          (setq ansi-osc--marker (copy-marker pos0)))))))

(defun my/shell-mode-hook ()
  "Customize `shell-mode'."

  ;; Look for the process that exists for the now-current buffer. Rename buffer to include its process ID.
  (when-let ((buf (current-buffer))
             (found (seq-filter (lambda (p) (eq buf (process-buffer p))) (process-list)))
             (pid (seq-map #'process-id found)))
    (rename-buffer (format "*Shell [%d]*" (car pid))))

  (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8 'utf-8)

  (setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
        comint-process-echoes my/is-macosx
        shell-dirtrackp nil)            ; disable since using comint-osc-process-output

  (ansi-color-for-comint-mode-on)
  (python-pdbtrack-setup-tracking)
  (add-hook 'comint-output-filter-functions #'my/comint-osc-process-output)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions #'comint-postoutput-scroll-to-bottom)

  (keymap-local-set "C-a" #'my/shell-bol)
  (keymap-local-set "M-h" #'consult-history))

(provide 'my-shell-mode)
;;; my-shell-mode.el ends here

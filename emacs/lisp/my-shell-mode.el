;;; package -- my-shell-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'company)
(require 'native-complete)

(defvar my-shell-home-root nil
  "Value to prepend to a directory.")

(defun my-shell-filter-job-control-messages (&optional string)
  "Strip out some warnings from Bash when running under Msys2.
STRING is something."
  (save-excursion
    ;; (message "my-shell-filter-job-control-messages")
    (goto-char (point-min))
    (when (looking-at "bash: .*\nbash: .*\n")
      (replace-match "")
      (remove-hook 'comint-output-filter-functions #'my-shell-filter-job-control-messages t))))

(defun my-shell-get-dir (&optional STRING)
  "Detect control sequence that announces directory change, and strip from output.
STRING is something. The format is ^[]0;USER:PATH^G

where '^[' is the ESC character and ^G is the Control-G character.
The user name is found in USER and the
path of the current directory of the shell follows after a ':' character."
  (save-excursion
    ;; (message "my-shell-get-dir")
    (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
	   (pos 0)
           (content (buffer-substring comint-last-output-start pmark)))
      ;; (message "! %s" content)
      (goto-char comint-last-output-start)
      (while (re-search-forward "\]0;\\(.*:\\([^]*\\)\\)\n" pmark t)
        ;; (message "@ %s" (match-string 1))
	(let ((dir (match-string 2)))
          ;; (message "dir: %s" dir)
	  (replace-match "")
          ;; Handle Cygwin paths. We really should only do this when running on Windows platforms.
	  (if (= (or (string-match "//?\\([a-zA-Z]\\)\\(/.*\\)" dir) -1) 0)
	      (let ((drive (match-string 1 dir))
		    (path (match-string 2 dir)))
                ;; (message "drive: %s path: %s"  drive path)
		(setq dir (concat drive ":"
				  (if (= (length path) 0)
				      "/"
				    path)))))

          ;; Replace escaping of backslash characters with naked backslashes for Windows.
	  (while (setq pos (string-match "\\\\" dir))
	    (setq dir (concat (substring dir 0 pos)
			      (substring dir (+ pos 1) nil))))

          ;; If dir does not exist as-is, see if it does after prepending my-shell-home-root
          ;;
          (when (file-directory-p my-shell-home-root)
            (unless (file-directory-p dir)
              (setq dir (concat my-shell-home-root dir))))

	  ;; Ignore any errors trying to change to the directory we grabbed.
	  ;;
	  (condition-case nil
	      (progn
		;; (message "cd %s" dir)
		(cd dir)
                (when (fboundp 'xterm-title-update)
                  (xterm-title-update))
		(force-mode-line-update))
	    (error nil)))))))

(defun my-remove-command-echo (&optional STRING)
  "Detect control sequence that highlight commands.
STRING is something. The format is

^[]2;...^G^[]1;...^G

where '^[' is the ESC character and ^G is the Control-G character."
  (save-excursion
    (let ((start-marker (if (and (markerp comint-last-output-start)
                                 (eq (marker-buffer comint-last-output-start)
                                     (current-buffer))
                                 (marker-position comint-last-output-start))
                            comint-last-output-start
                          (point-min-marker)))
          (end-marker (process-mark (get-buffer-process (current-buffer)))))
      ;; (message "! %s" (buffer-substring-no-properties start-marker end-marker))
      ;; (message "- %S %S" start-marker end-marker)
      (goto-char start-marker)
      (while (re-search-forward "\][12];[^]*" end-marker t)
        (replace-match "")))))

(defun my-shell-strip-trailing-empty-line (&optional string)
  "Remove duplicate prompt lines.
STRING is something. This is a hack --
should figure out *why* there are duplicate
prompt lines in the first place."
  (save-excursion
    ;; (message "my-shell-strip-trailing-empty-line")
    (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
           (tag (getenv "USER"))
           (prompt (concat tag "% "))
           (colored-prompt (concat "\\(\\(\\[1;32m\\)?" tag "%\\(\\[0m\\)? \\)"))
           (re (concat colored-prompt "\\(\n\\)" colored-prompt))
           (content (buffer-substring comint-last-output-start pmark))
           )
      ;; (message "! %s" content)
      (goto-char comint-last-output-start)
      (when (re-search-forward re pmark t)
        (delete-region (match-beginning 1) (match-end 4))))))

(defun my-shell-set-home-root ()
  "Locate home root."
  (make-local-variable 'my-shell-home-root)
  (setq my-shell-home-root
        (cond ((> (length (getenv "MSYSTEM")) 0) "C:/msys64")
              (t ""))))

(defun my-shell-mode-hook ()
  "Custom shell mode."
  ;; (setq ansi-color-names-vector ; better contrast colors
  ;;       ["black" "red4" "green4" "yellow4"
  ;;        "blue3" "magenta4" "cyan4" "white"])

  (setq ansi-color-names-vector
        ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])

  (ansi-color-for-comint-mode-on)
  (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8 'utf-8)

  (setq shell-dirtrackp nil
        comint-process-echoes t
	comint-buffer-maximum-size (* 1024 100)
        comint-completion-addsuffix t
	comint-eol-on-send t
        comint-output-filter-functions nil)
  (setq-local company-backends #'((company-files company-native-complete)))

  (add-hook 'completion-at-point-functions #'native-complete-at-point nil t)

  (add-hook 'comint-output-filter-functions #'my-shell-get-dir nil t)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output nil t)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t)
  (add-hook 'comint-output-filter-functions #'comint-postoutput-scroll-to-bottom nil t)

  (when (string-equal "windows-nt" system-type)
    (add-hook 'comint-output-filter-functions #'my-shell-filter-job-control-messages nil t)
    (add-hook 'comint-output-filter-functions #'my-shell-strip-trailing-empty-line nil t))

  ;; (add-hook 'comint-output-filter-functions (function comint-watch-for-password-prompt) t t)

  (set-face-attribute 'comint-highlight-prompt nil :inherit nil)

  (make-local-variable 'comint-completion-addsuffix)
  (make-local-variable 'my-shell-home-root)

  ;; (local-set-key [(control meta l)] 'my-start-screen-saver)

  ;; Look for the process that exists for the now current buffer. Rename
  ;; it to include its process ID.
  ;;
  (let ((procs (process-list))
	(aProc nil)
	(buf (current-buffer))
	(pid nil))
    (while procs
      (setq aProc (car procs)
	    procs (cdr procs))
      (if (eq buf (process-buffer aProc))
	  (setq pid (process-id aProc)
		procs nil)))
    (if pid
	(rename-buffer (format "*Shell %d*" pid))
      (rename-buffer "*Shell*" t)))

  (my-shell-set-home-root)

  ;; (local-set-key [(control c)(control a)] 'forward-line)
  ;; (local-set-key [(control a)] 'comint-bol)
  )

(provide 'my-shell-mode)
;;; my-shell-mode.el ends here

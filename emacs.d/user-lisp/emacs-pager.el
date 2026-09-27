;;; emacs-pager.el --- incredibly simple mode for showing data paged by emacs-pager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ansi-color)

(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Emacs pager mode.")

(defcustom emacs-pager-max-line-coloring 2000
  "Maxiumum number of lines to ansi-color.
If performance is bad when loading data, reduce this number."
  :group 'emacs-pager :type '(integer))

(defconst emacs-pager--control-h-dot (concat (make-string 1 ?\b) ".")
  "String containing ^H and '.'.")

(defconst emacs-pager--control-h-dot-re (concat "\\(\\(." emacs-pager--control-h-dot "\\)+\\)")
  "Imprecise regular expression to find sequences of CHAR ^H CHAR.")

(defun emacs-pager--filter-buffer ()
  "Remove occurrences of ^H and the character that follows."
  (goto-char (point-min))
  (while (re-search-forward emacs-pager--control-h-dot-re nil t 1)
    (replace-match (propertize
                    (mapconcat 'identity (save-match-data
                                           (split-string (match-string 1) emacs-pager--control-h-dot)) "")
                    'face 'custom-face-tag))))

;;;###autoload
(define-derived-mode emacs-pager-mode fundamental-mode "Pager"
  "Mode for viewing data pagd by `emacs-pager`."
  ;; Treat buffer as read-only
  (setq-local backup-inhibited t
              view-read-only t)
  (buffer-disable-undo)
  (emacs-pager--filter-buffer)
  (ansi-color-apply-on-region (goto-char (point-min))
                              (save-excursion
                                (forward-line emacs-pager-max-line-coloring)
                                (point)))
  (set-buffer-modified-p nil)
  (read-only-mode t))

;;;###autoload
(defun emacs-pager (path)
  "Show contents of file at PATH."
  (find-file-other-window path)
  (emacs-pager-mode)
  (delete-file path))

(provide 'emacs-pager)

;;; emacs-pager.el ends here

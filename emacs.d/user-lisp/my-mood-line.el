;;; my-notes.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Custom setup for mood-line package.
;;; Note that this file has a strong dependency on `keycast' package, but that may not exist yet.
;;; See the `~/.emacs.d/init.el' file for the keycast `use-package' definition.
;;; Code:

(require 'keycast)
(require 'mood-line)
(require 'nerd-icons)

(defun my/mood-line-segment-keycast ()
  "Return `keycast-format' if keycast mode enabled."
  (and (featurep 'keycast)
       (fboundp 'keycast-invisible-mode)
       keycast-invisible-mode
       (keycast-format keycast-mode-line-format)))

(defun my/mood-line-segment-project ()
  "Return `function/project-mode-line-format' if buffer belongs to a project."
  (or
   (and (fboundp 'project-current)
        (project-current)
        (fboundp 'project-name)
        (concat "«" (project-name (project-current)) "»"))
   (and (fboundp 'projectile-project-name)
        (concat "«" (projectile-project-name) "»"))))

(defun my/mood-line-segment-buffer-status ()
  "Return an indicator representing the status of the current buffer."
  (cond
   (buffer-read-only
    (propertize (mood-line--get-glyph :buffer-read-only)
                'face 'mood-line-buffer-status-read-only))
   ((buffer-narrowed-p)
    (propertize (mood-line--get-glyph :buffer-narrowed)
                'face 'mood-line-buffer-status-narrowed))
   (t
    " ")))

(defun my/mood-line-segment-misc-info ()
  "Return the current value of `mode-line-misc-info'."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (propertize (string-trim misc-info)
                  'face 'mood-line-status-info))))

(defun my/mood-line-when-active (segment)
  "Only show the SEGMENT if the the mode-line is active."
  (if (mode-line-window-selected-p)
      segment
    ""))

(defun my/mood-line-segment-mode-icon ()
  "Return the icon for the major mode."
  (let* ((mode (with-current-buffer (current-buffer)
                 major-mode))
         (icon (nerd-icons-icon-for-mode mode)))
    (concat icon " ")))

(defun my/mood-line-segment-vc ()
  "Return color-coded version control information."
  (concat (propertize (nerd-icons-devicon "nf-dev-git_branch") 'face 'nerd-icons-yellow) " " mood-line-segment-vc--text))

;;;###autoload
(defun my/mood-line-hook ()
  "Startup routine for mood-line."
  (setq-default mood-line-format
                (mood-line-defformat
                 :left
                 (((my/mood-line-when-active (my/mood-line-segment-mode-icon)) . "")
                  ((my/mood-line-when-active (mood-line-segment-modal)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-buffer-status)) . " ")
                  ((my/mood-line-when-active (my/mood-line-segment-project)) . " ")
                  ((mood-line-segment-buffer-name) . " ")
                  ((my/mood-line-when-active (my/mood-line-segment-misc-info)) . " ")
                  (my/mood-line-segment-keycast))
                 :right
                 (((my/mood-line-when-active (mood-line-segment-major-mode)) . " ")
                  ((my/mood-line-when-active (my/mood-line-segment-vc)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-multiple-cursors)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-cursor-position)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-scroll)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-checker)) . " ")
                  ((my/mood-line-when-active (mood-line-segment-process)) . " ")))
                mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-mode 1))

(provide 'my/mood-line)

;;; my-mood-line.el ends here.

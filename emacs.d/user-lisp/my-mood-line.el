;;; my-notes.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Custom setup for mood-line package.
;;; Note that this file has a strong dependency on `keycast' package, but that may not exist yet.
;;; See the `~/.emacs.d/init.el' file for the keycast `use-package' definition.
;;; Code:

(require 'keycast)
(require 'mood-line)

;;;###autoload
(defun my/mood-line-segment-keycast ()
  "Return `keycast-format' if keycast mode enabled."
  (and (featurep 'keycast)
       (fboundp 'keycast-invisible-mode)
       keycast-invisible-mode
       (keycast-format keycast-mode-line-format)))

;;;###autoload
(defun my/mood-line-hook ()
  "Startup routine for mood-line."
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-modal)                  . " ")
          ((or (mood-line-segment-buffer-status) " ") . " ")
          ;; Put prefix buffer name with project + "/" if buffer file belongs to project.
          ((my/mood-line-segment-project)          . "/")
          ((mood-line-segment-buffer-name)            . "  ")
          ((mood-line-segment-multiple-cursors)       . "  ")
          ((mood-line-segment-cursor-position)        . " ")
          ((mood-line-segment-scroll) . " ")
          (my/mood-line-segment-keycast))
         :right
         (((mood-line-segment-vc)         . "  ")
          ((mood-line-segment-major-mode) . " ")
          ((mood-line-segment-misc-info)  . "  ")
          ((mood-line-segment-checker)    . "  ")
          ((mood-line-segment-process)    . "  ")))
        mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-mode 1))

(provide 'my/mood-line)

;;; my-mood-line.el ends here.

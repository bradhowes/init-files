;;; my-doom-modeline.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Custom setup for doom-modeline package.
;;; Code:

(require 'keycast)
(require 'doom-modeline)

(defun my/mood-line-segment-keycast ()
  "Return `keycast-format' if keycast mode enabled."
  (and (featurep 'keycast)
       (fboundp 'keycast-invisible-mode)
       keycast-invisible-mode
       (keycast-format keycast-mode-line-format)))

;;;###autoload
(defun my/doom-modeline-setup ()
  "Startup routine for mood-line."

  (doom-modeline-def-modeline 'main
    '(eldoc bar window-state buffer-info buffer-position)
    '(compilation repl lsp buffer-encoding major-mode process vcs check))

  (doom-modeline-def-modeline 'minimal
    '(bar buffer-info-simple)
    '(major-mode))

  (doom-modeline-def-modeline 'special
    '(eldoc bar window-state buffer-info buffer-position)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'project
    '(bar buffer-default-directory buffer-position)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'dashboard
    '(bar buffer-default-directory-simple)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'vcs
    '(bar window-state buffer-info)
    '(compilation debug buffer-encoding major-mode vcs))

  (doom-modeline-def-modeline 'package
    '(bar package)
    '(compilation major-mode process))

  (doom-modeline-def-modeline 'info
    '(bar buffer-info info-nodes buffer-position)
    '(compilation buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(bar buffer-info)
    '(compilation media-info major-mode process vcs))

  (doom-modeline-def-modeline 'message
    '(eldoc bar buffer-info-simple buffer-position)
    '(compilation buffer-encoding major-mode))

  (doom-modeline-def-modeline 'pdf
    '(bar buffer-info pdf-pages)
    '(compilation major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(eldoc bar window-state buffer-info buffer-position)
    '(compilation debug lsp buffer-encoding major-mode process check))

  (doom-modeline-def-modeline 'helm
    '(bar helm-buffer-id helm-number helm-follow helm-prefix-argument)
    '(helm-help time))

  (doom-modeline-def-modeline 'timemachine
    '(eldoc bar git-timemachine buffer-position)
    '(buffer-encoding major-mode vcs))

  (doom-modeline-def-modeline 'calculator
    '(calc buffer-position)
    '(misc-info major-mode process))

  (doom-modeline-mode 1))

(provide 'my/doom-modeline)

;;; my-doom-modeline.el ends here.

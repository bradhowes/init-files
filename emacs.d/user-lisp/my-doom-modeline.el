;;; my-doom-modeline.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Custom setup for doom-modeline package.
;;; Code:

(require 'doom-modeline)

;;;###autoload
(defun my/doom-modeline-setup ()
  "Startup routine for mood-line."

  (doom-modeline-def-modeline 'main
    '(minibuffer-depth eldoc bar window-state modals buffer-info buffer-position matches follow keycast)
    '(compilation repl lsp buffer-encoding major-mode process vcs check))

  (doom-modeline-def-modeline 'minimal
    '(minibuffer-depth bar buffer-info-simple matches keycast)
    '(major-mode))

  (doom-modeline-def-modeline 'special
    '(minibuffer-depth eldoc bar window-state modals buffer-info buffer-position matches keycast)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'project
    '(minibuffer-depth bar buffer-default-directory buffer-position matches keycast)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'dashboard
    '(minibuffer-depth bar buffer-default-directory-simple keycast)
    '(compilation debug buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'vcs
    '(minibuffer-depth bar window-state buffer-info matches keycast)
    '(compilation debug buffer-encoding major-mode vcs))

  (doom-modeline-def-modeline 'package
    '(minibuffer-depth bar package buffer-position matches keycast)
    '(compilation major-mode process))

  (doom-modeline-def-modeline 'info
    '(minibuffer-depth bar buffer-info info-nodes buffer-position matches keycast)
    '(compilation buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(minibuffer-depth bar buffer-info keycast)
    '(compilation media-info major-mode process vcs))

  (doom-modeline-def-modeline 'message
    '(minibuffer-depth eldoc bar modals buffer-info-simple buffer-position keycast)
    '(compilation buffer-encoding major-mode))

  (doom-modeline-def-modeline 'pdf
    '(minibuffer-depth bar buffer-info pdf-pages keycast)
    '(compilation major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(minibuffer-depth eldoc bar window-state modals buffer-info buffer-position matches keycast)
    '(compilation debug lsp buffer-encoding major-mode process check))

  (doom-modeline-def-modeline 'helm
    '(minibuffer-depth bar helm-buffer-id helm-number helm-follow helm-prefix-argument)
    '(helm-help time))

  (doom-modeline-def-modeline 'timemachine
    '(minibuffer-depth eldoc bar git-timemachine buffer-position keycast)
    '(buffer-encoding major-mode vcs))

  (doom-modeline-def-modeline 'calculator
    '(minibuffer-depth calc buffer-position keycast)
    '(misc-info major-mode process))

  ;; Make prominent when file is modified.
  (set-face-attribute 'doom-modeline-buffer-modified nil :foreground "yellow1" :weight 'bold)
  ;; Always make prominent even if not active window.
  (setopt doom-modeline-always-highlight-modified-buffer-file-name t
          ;; Allow *all* segments to be removed if not in active mode-line
          doom-modeline-check-segment-visible-all t)

  (doom-modeline-mode 1))

(provide 'my/doom-modeline)

;;; my-doom-modeline.el ends here.

;;; package -- my-key-chord-mode-init -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-customizations)

(use-package key-chord
  ;; :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define key-chord-mode)
  :config (key-chord-mode 1))

(defvar my/hyper-keys-map
  (make-sparse-keymap)
  "Keymap for terminal hyper actions.")

(autoload 'my/emacs-make-key-bind "my-functions")
(autoload 'my/emacs-chord-bind "my-keymaps")
(autoload 'my/ace-window-always-dispatch "my-keymaps")
(autoload 'helpful-at-point "helpful")
(autoload 'popper-kill-latest-popup "popper")
(autoload 'my/ace-window-next "my-navigation")
(autoload 'my/ace-window-previous "my-navigation")
(autoload 'diff-hl-show-hunk "diff-hl")

;;;###autoload
(defun my/key-chord-mode-init (hyper-mappings)
  "Custom init of `key-chord' with HYPER-MAPPINGS."
  (if my/enable-key-chords
      (message "Using key-chords")
    (message ""))

  (when my/enable-key-chords
    (key-chord-mode 1)
    (apply #'my/emacs-make-key-bind my/hyper-keys-map (lambda (key) (substring key 2)) hyper-mappings)

    ;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy
    ;; to type with one or two hands. Not so sure about how useful this is -- I keep encountering issues.
    (my/emacs-chord-bind global-map
                         "aa" #'my/ace-window-always-dispatch
                         "hh" my/hyper-keys-map
                         "HH" #'helpful-at-point
                         "hb" #'popper-kill-latest-popup
                         "jn" #'my/ace-window-next
                         "jp" #'my/ace-window-previous
                         "qq" #'undo
                         "sb" #'speedbar
                         "vv" #'diff-hl-show-hunk)))

(provide 'my-key-chord-mode-init)
;;; my-key-chord-mode-init.el ends here

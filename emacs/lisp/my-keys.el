;;; package -- my-keys -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Keymap functions
;;; Code:

(use-package key-chord
  :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define))

(defun my/emacs-key-bind (keymap &rest definitions)
  "Apply key binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence.
There is now `bind-keys' method from `use-package' but my version requires
less typing."
  (unless (zerop (logand (length definitions) 1))
    (error "Uneven number of key+command pairs"))
  (unless (keymapp keymap)
    (error "Expected a `keymap' as first argument"))
  ;; Partition `definitions' into two groups, one with key definitions and another with functions and/or nil values
  (let* ((groups (seq-group-by #'stringp definitions))
         (keys (seq-drop (elt groups 0) 1))
         (commands (seq-drop (elt groups 1) 1))
         (fn (lambda (key command) (define-key keymap (kbd key) command))))
    (seq-mapn fn keys commands)))

(defun my/emacs-chord-bind (keymap &rest definitions)
  "Apply chord binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of chord+command pairs"))
  (unless (keymapp keymap)
    (error "Expected a `keymap' as first argument"))
  ;; Partition `definitions' into two groups, one with chord definitions and another with functions and/or nil values
  (let* ((groups (seq-group-by #'stringp definitions))
         (chords (seq-drop (elt groups 0) 1))
         (commands (seq-drop (elt groups 1) 1))
         (fn (lambda (chord command) (key-chord-define keymap chord command))))
    (seq-mapn fn chords commands)))

(provide 'my-keys)
;;; my-keys.el ends here

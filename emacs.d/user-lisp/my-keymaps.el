;;; my-keymaps.el --- custom keymaps and key configurations -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'generator)
(require 'key-chord)

(iter-defun my/take-two-iterator (values)
  "Iterator that yields a `cons' cell for every 2 items in VALUES."
  (let* ((head values))
    (while head
      (let* ((first (pop head))
             (second (pop head)))
        (iter-yield (cons first second))))))

;;;###autoload
(defun my/emacs-make-key-bind (keymap make-key &rest definitions)
  "Apply key binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence,
where the first element of the pair is a key sequence and the second is the
function or keymap to bind with. The key sequence is passed to MAKE-KEY and
the result of the call is used in the key binding.

There is now `bind-keys' method from `use-package' but my version requires
less typing."
  (unless (zerop (logand (length definitions) 1))
    (error "Uneven number of key+command pairs"))
  (unless (keymapp keymap)
    (error "Expected a `keymap' as first argument"))
  ;; Partition `definitions' into two groups, one with key definitions and another with functions and/or nil values
  (let ((iter (my/take-two-iterator definitions)))
    (iter-do (pair iter)
      (keymap-set keymap (funcall make-key (car pair)) (cdr pair)))))

;;;###autoload
(defun my/emacs-key-bind (keymap &rest definitions)
  "Apply key binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (apply #'my/emacs-make-key-bind keymap (lambda (key) key) definitions))

;;;###autoload
(defun my/emacs-chord-bind (keymap &rest definitions)
  "Apply chord binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of chord+command pairs"))
  (unless (keymapp keymap)
    (error "Expected a `keymap' as first argument"))
  (let ((iter (my/take-two-iterator definitions)))
    (iter-do (pair iter)
      (key-chord-define keymap (car pair) (cdr pair)))))

;;;###autoload
(defvar my/hyper-c-map
  (make-sparse-keymap)
  "Keymap for Hyper-c actions.")

(provide 'my-keymaps)

;;; my-keymaps.el ends here.

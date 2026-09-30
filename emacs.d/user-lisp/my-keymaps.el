;;; my-keymaps.el --- custom keymaps and key configurations -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'consult-register)
(require 'generator)
(require 'key-chord)
(require 'my-env)
(require 'my-finders)

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

;;;###autoload
(defvar my/hyper-n-map
  (make-sparse-keymap)
  "Keymap for Hyper-n actions.")

;; "Jump" to a well-known directory/file (eg "H-c j a" => dired buffer in "auv3-support" repo)
;;;###autoload
(defvar my/dired-jumps-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (tuple)
            (let* ((key (elt tuple 0))
                   (path (elt tuple 1))
                   (tag (elt tuple 2))
                   (name (cond
                          ;; When given a string, intern it use for name of lambda to execute `dired'
                          ((stringp path)
                           (let ((name (intern (concat "my/jmp-" (or tag path)))))
                             (fset name (lambda ()
                                          (interactive)
                                          (dired (if (or (string= "/" (substring path 0 1))
                                                         (string= "~" (substring path 0 1)))
                                                     (file-truename path)
                                                   (files--splice-dirname-file (my/repos) path)))))
                             name))
                          (t
                           path))))
              (message "my/dired-jumps-map: %s -> %s" tuple name)
              (when name
                (keymap-set map key name))))
          ;; Collection of 3-tuples that define a directory to jump to:
          ;; 1 - key to use
          ;; 2 - the directory to jump to (if not absolute then prepend with value from `my/repos')
          ;; 3 - the name to assign to the utility function (if nil make from directory)
          `(("a" "auv3-support" nil)
            ("c" "AUv3Controls" nil)
            ("i" "init-files" nil)
            ("e" "init-files/emacs.d" "emacs.d")
            ("E" ,(expand-file-name user-emacs-directory) "~.emacs.d")
            ("l" my/find-elpa-directory nil)
            ("L" ,(file-name-concat user-emacs-directory "elpa") "elpa")
            ("p" "SoundFontsPlus" nil)
            ("s" "AUv3Support" nil)
            ("u" my/find-user-lisp-file nil)
            ("U" ,(my/user-lisp) "user-lisp")
            ("z" "init-files/shells" "shells")
            ("2" "SF2Lib" nil)))
    map)
  "Keymap for quick Dired jumps.
The map is made up of tiny functions that invoke `dired' on a path.")

;; "Jump" to a saved position -- "H-j"
;;;###autoload
(defvar my/point-jumps-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'consult-register-store)
    (define-key map "j" #'consult-register-load)
    map)
  "Keymap for quick Dired jumps.")

(provide 'my-keymaps)

;;; my-keymaps.el ends here.

;;; package -- my-modes -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Various file modes setup
;;; Code:

(require 'my-constants)

(defun my/autoloads (&rest definitions)
  "Setup autoloads for my mode customizations.
DEFINITIONS is a sequence of string and symbol pairs, where the
string is a filename (without extension), and the following symbol
is either a standalone symbol or a list of symbols that represent
the items to setup for autoloading from the given file."
  (let* ((groups (seq-split definitions 2)))
    (mapc
     (lambda (pair)
       (let ((file (elt pair 0))
             (symbols (elt pair 1)))
         (if (consp symbols)
             (mapcar (lambda (symbol) (autoload symbol file)) symbols)
           (autoload symbols file))))
     groups)))

(my/autoloads
 "emacs-pager" 'emacs-pager
 "my-find-known-bindings" 'my/find-known-bindings
 "my-cmake-mode" 'my/cmake-mode-hook
 "my-c++-mode" 'my/c++-mode-hook
 "my-dired-mode" 'my/dired-mode-hook
 "my-js2-mode" 'my/js2-mode-hook
 "my-json-mode" 'my/json-mode-hook
 "my-lisp-mode" '(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" 'my/makefile-mode-hook
 "my-markdown-mode" 'my/markdown-mode-hook
 "my-python-mode" '(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" 'my/sh-mode-hook
 "my-shell-mode" 'my/shell-mode-hook)

(use-package cc-mode
  :init (add-to-list 'auto-mode-alist '("\\(\\.inl\\|\\.mm\\)\\'" . c++-mode))
  :hook ((c++-mode . my/c++-mode-hook)))

(use-package consult-eglot
  :after (consult eglot))

(use-package cmake-mode
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package diff-hl
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(use-package eglot
  :commands (eglot-ensure)
  :hook ((c++-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :custom
  ((eglot-autoshutdown t)
   (eglot-extend-to-xref t)
   (eglot-ignore-server-capabilities '(:documentFormattingProvider :documentRangeFormattingProvider)))
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c e" . eglot-code-action-extract)
              ("C-c c j" . eglot-code-action-inline)
              ("C-c c f" . eglot-format)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c q" . eglot-code-action-quickfix)
              ("C-c c r" . eglot-rename)
              ("C-c c w" . eglot-code-action-rewrite)))

(use-package flymake
  :commands (flymake-show-buffer-diagnostics)
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)
         (sh-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-json)

(use-package flymake-shellcheck
  :if (executable-find "shellcheck")
  :hook (sh-mode . flymake-shellcheck-load))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (sh-mode . ws-butler-mode)
         (text-mode . flyspell-mode)))

(use-package indent-bars
  ;; :if (not my/is-terminal)
  :hook (prog-mode . indent-bars-mode))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.yagconf\\'" . json-mode))
  :hook ((json-mode . my/json-mode-hook)))

(use-package js
  :config (setq js-indent-level 2))

;; (use-package js2-mode
;;   :hook ((js2-mode . my/js2-mode-hook)))

(use-package lisp-mode
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

(use-package makefile-mode
  :hook ((makefile-mode . my/makefile-mode-hook)
         (makefile-mode . indent-bars-mode)))

(use-package markdown-mode
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package python
  :hook ((python-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

(use-package sh-mode
  :hook ((sh-mode . my/sh-mode-hook)
         (sh-mode . indent-bars-mode)))

(use-package shell-mode
  :defines (explicit-bash-args)
  :init
  ;; Special-case QA env -- we are logged in as `sp_qa' user but we want our custom
  ;; environment. Command `bash' to load our custom settings.
  (let ((rc (file-truename (file-name-concat my/repos "configurations/qa.bashrc"))))
    (if (and my/is-qa
             (file-exists-p rc)
             (string-suffix-p "q" (system-name)))
        (setq explicit-bash-args (list "--noediting" "--rcfile" rc "-i"))
      (setq explicit-bash-args '("--noediting" "-i"))))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)))

(provide 'my-modes)

;;; my-modes.el ends here

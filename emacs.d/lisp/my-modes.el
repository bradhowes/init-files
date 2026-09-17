;;; package -- my-modes -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Various file modes setup
;;; Code:

(require 'cape)
(require 'eglot)
(require 'my-constants)
(require 'project)
(require 'treesit)

(add-to-list 'treesit-language-source-alist
             '(kotlin . ("https://github.com/fwcd/tree-sitter-kotlin")))
(add-to-list 'treesit-language-source-alist
             '(python . ("https://github.com/tree-sitter/tree-sitter-python")))

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
 "my-cmake-mode" 'my/cmake-mode-hook
 "my-c++-mode" 'my/c++-mode-hook
 "my-dired-mode" 'my/dired-mode-hook
 "my-find-known-bindings" 'my/find-known-bindings
 "my-json-mode" 'my/json-mode-hook
 "my-lisp-mode" '(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" 'my/makefile-mode-hook
 "my-markdown-mode" 'my/markdown-mode-hook
 "my-python-mode" '(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" 'my/sh-mode-hook
 "my-shell-mode" 'my/shell-mode-hook)

(use-package cape
  :ensure t)

(use-package cc-mode
  :init (add-to-list 'auto-mode-alist '("\\(\\.inl\\|\\.mm\\)\\'" . c++-mode))
  :hook ((c++-mode . my/c++-mode-hook)))

(use-package cmake-mode
  :ensure t
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(defun my/known-project-eglot-ensure ()
  "Determine if editing file of a known project."
  ;; (vc Git "~/Developer/Mine/sidecar/") -> '("~/Developer/Mine/sidecar/")
  (let ((proj (list (project-root (project-current)))))
    ;; See if '("~/Developer/Mine/sidecar/" is in `project--list` list.
    (if (and proj (member proj project--list))
        (progn
          (message "Found %s in project--list - starting eglot" proj)
          (eglot-ensure))
      (message "Project %s not found project--list - not running eglot" proj))))

(defun my/eglot-configure ()
  "Custom buffer configuration of Eglot."
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'eglot-completion-at-point #'tempel-expand))
              eldoc-documentation-functions (cons #'flymake-eldoc-function
                                                  (remove #'flymake-eldoc-function eldoc-documentation-functions))
              eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eglot
  :ensure t
  :commands (eglot-ensure)
  :defines (eglot-mode-map)
  :hook ((c++-mode . my/known-project-eglot-ensure)
         (c++-ts-mode . my/known-project-eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (kotlin-ts-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (markdown-ts-mode . eglot-ensure)
         (python-base-mode . eglot-ensure)
         (scala-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-configure))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  ((eglot-autoshutdown t)
   (eglot-extend-to-xref t))
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c e" . eglot-code-action-extract)
              ("C-c c f" . eglot-format)
              ("C-c c j" . eglot-code-action-inline)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c q" . eglot-code-action-quickfix)
              ("C-c c r" . eglot-rename)
              ("C-c c w" . eglot-code-action-rewrite)))

;; (keymap-global-set "C-c c" eglot-mode-map)

(with-eval-after-load 'eglot
  (setq eglot-server-programs (assoc-delete-all 'kotlin-ts-mode eglot-server-programs))
  (add-to-list 'eglot-server-programs '(kotlin-ts-mode . ("kotlin-lsp" "--stdio")))
  (setq completion-category-defaults nil))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(use-package flymake
  :commands (flymake-show-buffer-diagnostics)
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; (use-package flymake-json
;;   :ensure t)

(use-package flyspell
  :ensure t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode))

(use-package json-ts-mode
  :ensure t
  :hook ((json-ts-mode . my/json-mode-hook)))

(use-package kotlin-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode)))

(use-package js
  :ensure t
  :config (setq js-indent-level 2))

(use-package kotlin-ts-mode
  :ensure t
  :mode ("\\.kt\\'" "\\.kts\\'"))

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
  :ensure t
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package python
  :hook ((python-ts-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

(use-package sh-mode
  :hook (sh-mode . my/sh-mode-hook))

(use-package shell-mode
  :defines (explicit-bash-args)
  :init
  ;; Special-case QA env -- we are logged in as `sp_qa' user but we want our custom
  ;; environment. Command `bash' to load our custom settings.
  ;; (let ((rc (file-truename (file-name-concat my/repos "configurations/qa.bashrc"))))
  ;;   (if (and my/is-qa
  ;;            (file-exists-p rc)
  ;;            (string-suffix-p "q" (system-name)))
  ;;       (setq explicit-bash-args (list "--noediting" "--rcfile" rc "-i"))
  (setq explicit-bash-args '("--noediting" "-i"))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package tempel
  :ensure t
  :commands (tempel-expand))

(use-package ws-butler
  :ensure t
  :hook ((prog-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)))

(provide 'my-modes)

;;; my-modes.el ends here

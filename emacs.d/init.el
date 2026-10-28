;;; init.el --- -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

;; (setq debug-on-message "Unable to activate package 'lsp-mode'.")

;; Always work in the UTF-8 coding system.
(let ((coding-system 'utf-8))
  (set-charset-priority 'unicode)
  (prefer-coding-system coding-system)

  (dolist (v (list 'locale-coding-system
                   'coding-system-for-read
                   'coding-system-for-write))
    (set v coding-system))

  (dolist (p (list #'set-terminal-coding-system
                   #'set-keyboard-coding-system
                   #'set-selection-coding-system
                   #'prefer-coding-system))
    (funcall p coding-system)))

;; Replicating much of what is in user-lisp-autoloads.el in order to silence warnings from flymake. The file is loaded
;; upon startup but the Elisp bytecompiler does not know about that when run in another Emacs process. I think flycheck
;; does the right thing here…
;;
(autoload 'my/c++-mode-hook "my-c++-mode")
(autoload 'my/c++-mode-remap "my-c++-mode")
(autoload 'my/cmake-mode-hook "my-cmake-mode")
(autoload 'my/consult-init "my-consult")
(autoload 'my/dired-mode-hook "my-dired-mode")
(autoload 'my/doom-modeline-setup "my-doom-modeline")
(autoload 'my/find-elpa-directory "my-finders")
(autoload 'my/find-shell-init-file "my-finders")
(autoload 'my/find-user-custom-file "my-finders")
(autoload 'my/find-user-init-file "my-finders")
(autoload 'my/find-user-lisp-file "my-finders")
(autoload 'my/after-init-emacs-init "my-functions")
(autoload 'my/beginning-of-line-text "my-functions")
(autoload 'my/cape-initialize "my-functions")
(autoload 'my/copy-file-name-to-clipboard "my-functions")
(autoload 'my/corfu-initialize "my-functions")
(autoload 'my/describe-symbol-at-point "my-functions")
(autoload 'my/diff-hl-initialize "my-functions")
(autoload 'my/goto-mark "my-functions")
(autoload 'my/indent-buffer "my-functions")
(autoload 'my/ligature-initialize "my-functions")
(autoload 'my/magit-init-files "my-functions")
(autoload 'my/marginalia-initialize "my-functions")
(autoload 'my/matching-paren "my-functions")
(autoload 'my/popper-initialize "my-functions")
(autoload 'my/nerd-icons-corfu-initialize "my-functions")
(autoload 'my/reload-buffer "my-functions")
(autoload 'my/set-mark-deactivate "my-functions")
(autoload 'my/trusted-content-p "my-functions")
(autoload 'my/ultra-scroll-initialize "my-functions")
(autoload 'my/dired-jumps-bind "my-keymaps")
(autoload 'my/emacs-chord-bind "my-keymaps")
(autoload 'my/emacs-key-bind "my-keymaps")
(autoload 'my/emacs-make-key-bind "my-keymaps")
(autoload 'my/layout-frame-pos-left "my-layout")
(autoload 'my/layout-frame-pos-center "my-layout")
(autoload 'my/layout-frame-pos-right "my-layout")
(autoload 'my/layout-make-frame "my-layout")
(autoload 'my/layout-normal-screen-font-size "my-layout")
(autoload 'my/layout-screen-layout-changed "my-layout")
(autoload 'my/layout-share-screen-font-size "my-layout")
(autoload 'my/lisp-mode-hook "my-lisp-mode")
(autoload 'my/lisp-data-mode-hook "my-lisp-mode")
(autoload 'my/markdown-ts-mode-hook "my-markdown-mode")
(autoload 'my/markdown-ts-setup-hook "my-markdown-mode")
(autoload 'my/project-initialize "my-project")
(autoload 'my/ace-window-always-dispatch "my-navigation")
(autoload 'my/ace-window-next "my-navigation")
(autoload 'my/ace-window-prefix "my-navigation")
(autoload 'my/ace-window-previous "my-navigation")
(autoload 'my/customize-other-window "my-navigation")
(autoload 'my/customize-search "my-navigation")
(autoload 'my/kill-current-buffer "my-navigation")
(autoload 'my/info-other-frame "my-navigation")
(autoload 'my/next-buffer-current-window "my-navigation")
(autoload 'my/prev-buffer-current-window "my-navigation")
(autoload 'my/show-messages-buffer "my-navigation")
(autoload 'my/show-messages-buffer-other-window "my-navigation")
(autoload 'my/notes-consult-notes-hook "my-notes")
(autoload 'my/notes-denote-hook "my-notes")
(autoload 'my/org-filter-buffer-substring "my-org")
(autoload 'my/show-project-menu "my-project")
(autoload 'my/python-mode-hook "my-python-mode")
(autoload 'my/inferior-python-mode-hook "my-python-mode")
(autoload 'my/start-emacs-server "my-server")
(autoload 'my/sh-mode-hook "my-sh-mode")
(autoload 'my/shell-mode-hook "my-shell-mode")
(autoload 'my/repl-other-window "my-shells")
(autoload 'my/shell "my-shells")
(autoload 'my/shell-other-window "my-shells")
(autoload 'my/shell-other-frame "my-shells")
(autoload 'ksh "my-shells")
(autoload 'my/htop "my-tops")
(autoload 'my/yaml-ts-mode-hook "my-yaml-mode")
(autoload 'my/yaml-ts-setup-hook "my-yaml-mode")

;; Load various environmental settings that can be safely determined at this time, and setup various Emacs variables to
;; reflect the environment.
(require 'my-customizations)
(require 'my-env)
(my/env-setup)

;; Show how long it took to load the init.el file -- provides a rough guide when adding new packages. Should strive to stay
;; below 1 second.
(add-hook 'emacs-startup-hook #'my/display-startup-time -90)

;; Adapt to current screen real estate.
(add-hook 'emacs-startup-hook #'my/layout-screen-layout-changed 70)

;; Fire up a companion emacs server
(add-hook 'emacs-startup-hook #'my/start-emacs-server 80)

(defvar my/dired-jumps-map
  (make-sparse-keymap)
  "Keymap for quick Dired jumps or file selection in directory.")

(defvar my/hyper-c-map
  (make-sparse-keymap)
  "Keymap for Hyper-c actions.")

(defvar my/hyper-n-map
  (make-sparse-keymap)
  "Keymap for Hyper-n actions.")

(defvar my/symbol-overlay-map
  (make-sparse-keymap)
  "Keymap for symbol-overlay actions.")

;; To keep this file small, we put all customizations in their own file.
(setq-default custom-file (file-name-concat (expand-file-name user-emacs-directory) "custom.el")
              ;; To keep this file small, minimize `use-package' content.
              use-package-always-defer t
              use-package-always-ensure nil ; change to `t' to force loading if not found.
              use-package-expand-minimally t)

(defalias 'repl 'my/repl
  "Legacy alias to start Elisp read/eval/print loop in current window.")

;; NOTE: for some reason, this may be breaking cape.
;; (advice-add 'trusted-content-p :filter-return #'my/trusted-content-p)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (use-package filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

(use-package package
  :defer nil ;; !!!
  :custom
  (package-archive-priorities '(("melpa" . 10)
                                ("melpa-stable" . 10)
                                ("gnu" . 15)
                                ("nongnu" . 20)))
  :config
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; NOTE: hyper use requires Karabiner-Elements mapping from 'caps_lock' to 'right_control'
;;
;; {
;;   "manipulators": [
;;     {
;;       "description": "Change caps_lock to right_control. In Emacs set 'mac_right_control_modifier' to 'hyper.",
;;       "from": {
;;         "key_code": "caps_lock",
;;         "modifiers": { "optional": ["any"] }
;;       },
;;       "to": [{ "key_code": "right_control" }],
;;       "type": "basic"
;;     }
;;   ]
;; }

(use-package accent
  :bind (:map my/hyper-c-map ("a" . accent-menu)))

;;; ===== ace-window =====

(use-package ace-window
  :config
  (setq aw-make-frame-char ?n)
  (advice-add 'aw-make-frame :override #'my/layout-make-frame)
  :commands (ace-window aw-flip-window))

(use-package bind-key
  :bind (:map help-map ("y" . describe-personal-keybindings)))

(let ((mode (if my/use-c-ts-mode 'c++-ts-mode 'c++-mode)))
  (add-to-list 'auto-mode-alist
               `(,(concat "\\.\\(cc\\|hh\\|ii\\|inl\\|mm\\|"
                          "\\([ch]\\(pp\\|xx\\|\\+\\+\\)\\)\\)\\'")
                 . ,mode)))

(if my/use-c-ts-mode
    (use-package c-ts-mode
      :hook ((c++-ts-mode . my/c++-mode-hook)
             (after-init . my/c++-mmode-remap)))
  (use-package cc-mode
    :hook ((c++-mode . my/c++-mode-hook))))

(use-package char-menu
  :bind (("C-z" . char-menu))
  :custom
  (char-menu '("—" "‘’" "“”" "…" "«»" "–"
               ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
               ("Math" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
               ("Arrows" "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
               ("Greek" "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))))

(use-package cmake-mode
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package compile
  :custom
  (compilation-error-regexp-alist (cons '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2) ; from pyright?
                                        compilation-error-regexp-alist)))

(use-package compile-multi)

(use-package consult-compile-multi
  :hook ((after-init . consult-compile-multi-mode)))

(use-package consult
  :commands (consult--customize-put consult-flymake)
  :bind ( ("M-y" . consult-yank-replace)

          :map ctl-x-map ;; C-x
          ("M-:" . consult-complex-command)
          ("b" . consult-buffer)
          ("f" . consult-recent-file)
          ("4 b" . consult-buffer-other-window)
          ("5 b" . consult-buffer-other-frame)
          ("r b" . consult-bookmark)
          ("r l" . consult-bookmark)

          ;; ("M-#" . consult-register-load)
          ;; ("M-'" . consult-register-store)
          ;; ("C-M-#" . consult-register)

          :map goto-map ;; M-g
          ("g" . consult-goto-line)
          ("M-g" . consult-goto-line)
          ("o" . consult-outline)
          ("m" . consult-mark)
          ("k" . consult-global-mark)
          ("i" . consult-imenu)
          ("I" . consult-imenu-multi)

          :map help-map ;; C-h
          ("M" . consult-man)

          :map search-map ;; M-s
          ("d" . consult-find)
          ("g" . consult-grep)
          ("G" . consult-git-grep)
          ("i" . consult-imenu)      ; Duplicate 'M-g i'
          ("k" . consult-keep-lines)
          ("l" . consult-line)
          ("M-s" . my/consult-line-symbol-at-point)
          ("L" . consult-line-multi)
          ("r" . consult-ripgrep)
          ("u" . consult-focus-lines)
          ;; Isearch integration
          ("e" . consult-isearch-history)

          :map my/hyper-c-map ;; H-c
          ("M-x" . consult-mode-command)
          ("h" . consult-history)
          ;; ("C-h i" . consult-info)
          ("k" . consult-kmacro)
          ("I" . consult-info)

          :map isearch-mode-map
          ("M-e" . consult-isearch-history)
          ("M-s e" . consult-isearch-history)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi)

          :map minibuffer-local-map
          ("C-s" . consult-history)

          :map project-prefix-map
          ("b" . consult-project-buffer))

  :commands (consult-register-format consult-register-window consult-xref consult-register-store consult-register-load)
  :hook ((after-init . my/consult-init)))

;; Use Consult to select xref locations with preview
(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package completion
  :hook ((after-init . dynamic-completion-mode)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)))

(use-package denote
  :hook ((dired-mode . denote-dired-mode))
  :bind ( :map my/hyper-n-map
          ("b" . denote-backlinks)
          ("c" . denote)
          ("d" . denote-dired)
          ("g" . denote-grep)
          ("l" . denote-link)
          ;; ("n" . consult-notes) -- done below
          ("r" . denote-rename-file)))

(use-package consult-notes
  :bind ( :map my/hyper-n-map
          ("n" . consult-notes)
          ("H-n" . consult-notes)
          :map my/hyper-c-map
          ("n" . consult-notes)
          ("H-n" . consult-notes))
  :hook (after-init . my/notes-consult-notes-hook))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))
                                   (minibuffer (orderless initials basic))))
  (completion-category-defaults nil))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ( :map my/hyper-c-map
          ("p" . cape-prefix-map))
  :hook (after-init . my/cape-initialize))

(use-package corfu
  :commands (global-corfu-mode corfu-popupinfo-mode)
  :bind ( :map corfu-map
          ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Enable Corfu only for certain modes.
  :hook ((after-init . my/corfu-initialize)))

(use-package crux
  :commands (crux-find-current-directory-dir-locals-file crux-duplicate-current-line-or-region)
  :bind ( ("C-k" . crux-smart-kill-line)
          ("C-^" . crux-top-join-line)
          :map my/hyper-c-map
          ("d" . consult-dir)
          ("C-i" . crux-indent-defun)
          :map ctl-x-4-map
          ("t" . crux-transpose-windows)))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package diff-hl
  :commands (diff-hl-show-hunk diff-hl-flydiff-mode global-diff-hl-mode global-diff-hl-show-hunk-mouse-mode)
  :hook ((after-init . my/diff-hl-initialize)))

(use-package dired
  :hook (dired-mode . my/dired-mode-hook))

(use-package doom-modeline
  :hook ((after-init . my/doom-modeline-setup)))

(use-package elisp-mode
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

;; (use-package eldoc-box
;; :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

(use-package emacs-pager
  :commands (emacs-pager emacs-pager-mode))

;; FYI: Embark's default action binding of "RET" fails if a mode binds to <return>.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package esup
;;   :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind ( :map flymake-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error)))

(use-package flyover
  :hook ((flymake-mode . flyover-mode)
         (flyover-mode . my/flyover-mode-hook)))

;; Requires:
;; - astyle -- brew install astyle
;; - cmakelang -- pip3 install cmakelang[YAML] (or pip3 install cmakelang pyyaml)
;; - black -- pip3 install black
;; - shfmt -- brew install shftm
(use-package format-all
  :commands (format-all-mode)
  :defines (format-all-formatters)
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("C" (astyle "--mode=c" "--style=1tbs" "--indent=spaces=2"))
                                        ("C++" (astyle "--mode=c"
                                                       "--style=1tbs"
                                                       "--attach-namespaces"
                                                       "--attach-classes"
                                                       "--attach-inlines"
                                                       "--attach-extern-c"
                                                       "--attach-closing-while"
                                                       "--keep-one-line-blocks"
                                                       "--max-code-length=126"
                                                       ;; "--indent-after-parens"
                                                       ;; "--indent-continuation=1"
                                                       "--indent=spaces=2"))
                                        ("CMake" (cmake-format))
                                        ("Python" (black))
                                        ("Shell" (shfmt "-i" "2" "-ci" "-bn")))))

(use-package git-timemachine)

(use-package helpful
  :bind ( :map help-map
          ("a" . helpful-symbol)
          ("f" . helpful-callable)
          ("v" . helpful-variable)
          ("k" . helpful-key)
          ("." . helpful-at-point)
          ("M-f" . helpful-function)
          ("M-c" . helpful-command)))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
(use-package ibuffer
  :config
  (keymap-unset ibuffer-mode-map "M-o" t))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))

(use-package iso-transl
  :bind-keymap ("H-8" . iso-transl-ctl-x-8-map)) ; Enter diacritics using "dead" keys after <H-8> or <C-X 8>

(use-package kkp
  :if (my/is-terminal)
  :commands (global-kkp-mode)
  :hook (tty-setup . (lambda ()
                       ;; (setq kkp-alt-modifier 'alt) ; use to map the Alt keyboard modifier to Alt (and not to Meta)
                       ;; For C-g aborting blocking subprocesses, see "C-g and blocking subprocesses" in the README.
                       (setq kkp-restore-legacy-keys-around-subprocesses t)
                       (global-kkp-mode 1))))

(use-package kotlin-ts-mode
  :mode ("\\.kt\\'"))

(use-package ligature
  :commands (ligature-set-ligatures global-ligature-mode)
  :hook ((after-init . my/ligature-initialize)))

(use-package magit
  :commands (magit-status-setup-buffer magit-status magit-project-status)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :bind ( :map ctl-x-map
          ("g" . magit-status)
          ;; Take over vc-dir
          ("p v" . magit-project-status)
          ;; Take over vc-print-log
          ("v l" . magit-log-buffer-file)
          :map my/hyper-c-map
          ("f" . magit-file-dispatch))
  :custom
  (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package markdown-ts-mode
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :hook ((markdown-ts-mode . my/markdown-ts-mode-hook))
  (after-init . my/markdown-ts-setup-hook))

(use-package yaml-ts-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-ts-mode . my/yaml-ts-mode-hook)))

(use-package mode-line-bell)

(use-package keycast
  :commands (keycast-invisible-mode)
  :vc (:url "https://github.com/tarsius/keycast")
  :custom
  (keycast-mode-line-format "%2s%k%c%r"))

(use-package multiple-cursors
  :bind ( ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          :map my/hyper-c-map
          ("." . mc/mark-all-like-this)))

(use-package my-fontify-braces)

(use-package nerd-icons)

(use-package nerd-icons-completion
  :commands (nerd-icons-completion-mode))

(use-package compile-multi-nerd-icons)

(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :bind ( :map minibuffer-local-map
          ("<f1>" . marginalia-cycle))
  :hook ((after-init . my/marginalia-initialize)))

(use-package nerd-icons-corfu
  :commands (nerd-icons-corfu-formatter)
  :hook ((after-init . my/nerd-icons-corfu-initialize)))

(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package nerd-icons-xref
  :hook ((after-init . nerd-icons-xref-mode)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Emacs 31: partial-completion behaves like substring
  (completion-pcm-leading-wildcard t))

(use-package org
  :commands (org-store-link)
  :config
  (defvar my/org-key-map
    (let ((map (make-sparse-keymap)))
      (define-key map "a" #'org-agenda)
      (define-key map "c" #'org-capture)
      (define-key map "l" #'org-store-link)
      map)
    "Keymap for my org mode access.")
  :bind-keymap ("H-o" . my/org-key-map))

(use-package osx-dictionary
  :if my/is-macosx
  :bind (:map my/hyper-c-map ("l" . osx-dictionary-search-pointer)))

(use-package python
  :hook ((python-ts-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

(use-package pyvenv)

(use-package popper
  :commands (popper-kill-latest-popup popper-mode popper-echo-mode)
  :functions (popper--delete-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle))
  :hook ((after-init . my/popper-initialize)))

(defvar my/project-search-map (make-sparse-keymap)
  "A prefix map like that found in projectile.
Bound to \\`C-x p s'.")

;; Need to do this here due to how `use-package(project)` works:
(keymap-set project-prefix-map "s" my/project-search-map)

(use-package project
  :bind ( :map project-prefix-map
          ("$" . #'project-shell)
          ("m" . #'my/show-project-menu)
          ("H-p" . #'project-switch-project)
          ;; Cannot do this here for some reason -- see note above.
          ;; ("s" . my/project-search-map)))
          )
  :hook ((after-init . my/project-initialize)))

(use-package recentf
  :hook ((after-init . recentf-mode)))

(use-package rg
  :commands (rg-enable-default-bindings rg-project)
  :bind ( :map my/project-search-map
          ("r" . rg-project))
  :hook (after-init . rg-enable-default-bindings))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package saveplace
  :hook ((after-init . save-place-mode)))

(autoload 'my/show-scratch-buffer "my-scratch-plus-mode")
(autoload 'my/scratch-plus-mode-hook "my-scratch-plus-mode")
(autoload 'my/scratch-plus-mode-init "my-scratch-plus-mode")

(use-package scratch-plus
  :custom
  (scratch-plus-save-directory (file-name-concat user-emacs-directory "scratches"))
  (scratch-plus-project-subdir ".scratches")
  (scratch-plus-restore-type t)
  (scratch-plus-force-restore t)
  :bind ( :map my/hyper-c-map
          ("s" . my/show-scratch-buffer))
  :hook ((scratch-plus-mode . my/scratch-plus-mode-hook)
         (after-init . my/scratch-plus-mode-init)))

(use-package sh-script
  :hook (sh-mode . my/sh-mode-hook))

(defalias 'ksh #'shell
  "Alias to satisfy habit from AIX days.")

(use-package shell
  :custom
  (explicit-bash-args '("--noediting" "-i"))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package subword
  :hook ((after-init . global-subword-mode)))

(use-package symbol-overlay
  :bind ( :map my/symbol-overlay-map
          ("s" . symbol-overlay-put)
          ("H-s" . symbol-overlay-put)
          ("n" . symbol-overlay-switch-forward)
          ("p" . symbol-overlay-switch-backward)
          ("c" . symbol-overlay-remove-all)))

(use-package ultra-scroll
  :commands (ultra-scroll-mode)
  :hook ((after-init . my/ultra-scroll-initialize)))

(use-package vertico
  :commands (vertico-mode)
  :bind ( :map vertico-map
          ("H-d" . consult-dir)
          ("H-j" . consult-dir-jump-file)
          :map minibuffer-local-completion-map
          ("H-d" . consult-dir)
          ("H-j" . consult-dir-jump-file))
  :hook ((after-init . vertico-mode)))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package whitespace
  :hook ((after-init . global-whitespace-mode)))

(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)))

(use-package winner
  :hook (after-init . winner-mode)
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         :map my/hyper-c-map
         ("u" . winner-undo)
         ("C-u" . winner-undo)
         ("C-r" . winner-redo)))

(use-package xclip
  :if (my/is-terminal)
  :commands (xclip-mode)
  :hook (tty-setup . (lambda () (xclip-mode 1))))

(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package zoxide)
(use-package consult-zoxide)

;; NOTE: this is setting a global variable, but we should really just do this when operating in an org buffer.
(setq filter-buffer-substring-function #'my/org-filter-buffer-substring)

(defvar ffap-bindings
  '((keymap-global-set "<remap> <find-file>" #'find-file-at-point)
    (keymap-global-set "<remap> <find-file-other-window>" #'ffap-other-window)
    (keymap-global-set "<remap> <find-file-other-frame>" #'ffap-other-frame)
    (keymap-global-set "<remap> <find-file-other-tab>" #'ffap-other-tab)

    (keymap-global-set "<remap> <dired>" #'dired-at-point)
    (keymap-global-set "<remap> <dired-other-window>" #'ffap-dired-other-window)
    (keymap-global-set "<remap> <dired-other-frame>" #'ffap-dired-other-frame)
    (keymap-global-set "<remap> <list-directory>" #'ffap-list-directory))
  "List of binding forms evaluated by function `ffap-bindings'.")

(use-package emacs
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (before-save . copyright-update)
         (after-init . my/after-init-emacs-init)))

(defun my/consult-info-emacs ()
  "Search Emacs info."
  (interactive)
  (consult-info "emacs" "autotype" "cape" "corfu" "denote" "embark" "magit" "marginalia" "orderless" "vertico"))

(use-package window
  :init
  (setq switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t
        window-resize-pixelwise t
        window-sides-slots '(0 0 3 1)
        display-buffer-base-action '((display-buffer-reuse-window ace-display-buffer))
        display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window ace-display-buffer) (reusable-frames . nil))
                               ("\\*R" nil (reusable-frames . nil))
                               ,(cons "\\*helm" display-buffer-fallback-action))))

;; Show log buffer in something other than the current window
;; ("magit-log" nil (inhibit-same-window . t))
;; ("magit-diff:" nil (inhibit-same-window . t))))))

;;; --- Key Bindings
(my/emacs-key-bind my/hyper-c-map
                   "g" #'my/magit-init-files
                   "i" #'my/find-user-init-file
                   "j" my/dired-jumps-map
                   "r" #'ielm
                   "t" #'top-level
                   "D" #'crux-find-current-directory-dir-locals-file
                   "S" #'my/find-shell-init-file
                   "H-l" #'my/find-user-init-file
                   "," #'my/find-user-custom-file
                   "H-c" #'my/copy-file-name-to-clipboard
                   "H-k" #'my/kill-current-buffer)

(my/emacs-key-bind help-map
                   "C-." #'my/describe-symbol-at-point
                   "C-j" #'popper-toggle
                   "a" #'describe-symbol
                   "c" #'describe-char
                   "h" #'ignore         ; show 'Hello' in various fonts
                   "u" #'apropos-user-option
                   "F" #'apropos-function
                   "K" #'describe-keymap
                   "L" #'apropos-library
                   "V" #'apropos-variable)

(my/emacs-key-bind ctl-x-map
                   "C-b" #'ibuffer
                   "C-o" #'other-frame
                   "C-z" #'ignore       ; suspend-frame
                   "C-+" #'ignore       ; text-scale-adjust
                   "C-=" #'ignore       ; text-scale-adjust
                   "C--" #'ignore       ; text-scale-adjust
                   "0" #'delete-other-windows
                   "O" #'other-frame
                   "M-b" #'consult-project-buffer
                   "M-v" #'my/reload-buffer
                   "h" #'ignore)        ; mark-whole-buffer

(my/emacs-key-bind ctl-x-4-map
                   "c" #'my/customize-other-window
                   "k" #'my/shell-other-window
                   "o" #'my/ace-window-prefix
                   "r" #'my/repl-other-window)

(my/emacs-key-bind ctl-x-5-map
                   "i" #'my/info-other-frame
                   "k" #'my/shell-other-frame)

(my/emacs-key-bind global-map
                   "S-<left>" #'my/ace-window-previous
                   "S-<right>" #'my/ace-window-next
                   "S-<up>" #'my/ace-window-previous
                   "S-<down>" #'my/ace-window-next

                   "M-g d" #'dired-jump
                   "M-o" #'other-window
                   ;; "M-O" #'my/ace-window-always-dispatch

                   "C-a" #'my/beginning-of-line-text
                   "C-o" #'aw-flip-window

                   ;; "C-s" #'isearch-forward-regexp
                   ;; "C-M-s" #'isearch-forward-symbol

                   "M-{" #'my/prev-buffer-current-window
                   "M-}" #'my/next-buffer-current-window

                   "M-<f1>" #'my/layout-frame-pos-left
                   "M-<f2>" #'my/layout-frame-pos-center
                   "M-<f3>" #'my/layout-frame-pos-right

                   "C-M-\\" #'my/indent-buffer

                   "<home>" #'beginning-of-buffer
                   "<end>" #'end-of-buffer
                   "<delete>" #'delete-char
                   "S-<f12>" #'package-list-packages
                   "S-<f11>" #'my/layout-screen-layout-changed

                   "M-z" #'zap-up-to-char
                   "M-_" #'join-line

                   "M-P" #'my/ace-window-previous
                   "M-N" #'my/ace-window-next

                   "C-S-p" #'my/ace-window-previous
                   "C-S-n" #'my/ace-window-next

                   "<f1>" #'my/layout-normal-screen-font-size
                   "<f2>" #'my/layout-share-screen-font-size

                   "<insert>" #'ignore  ; disable key for toggling overwrite-mode
                   "<insertchar>" #'ignore  ; disable key for toggling overwrite-mode

                   "<pinch>" #'ignore

                   ;; Disable font size changes via trackpad/scroll-wheel
                   "C-<mouse-4>" #'ignore
                   "C-<mouse-5>" #'ignore
                   "C-<wheel-up>" #'ignore
                   "C-<wheel-down>" #'ignore

                   "C-M-<mouse-4>" #'ignore
                   "C-M-<mouse-5>" #'ignore
                   "C-M-<wheel-up>" #'ignore
                   "C-M-<wheel-down>" #'ignore)

(my/dired-jumps-bind my/dired-jumps-map
                     ;; Collection of 3-tuples that define a directory to jump to:
                     ;; 1 - key to use
                     ;; 2 - the directory to jump to (if not absolute then prepend with value from `my/repos')
                     ;; 3 - the name to assign to the utility function (if nil make from directory)
                     `(("a" "auv3-support" nil)
                       ("A" "AUv3Support" nil)
                       ("c" "AUv3Controls" nil)
                       ("i" "init-files" nil)
                       ("e" "init-files/emacs.d" "emacs.d")
                       ("E" ,(expand-file-name user-emacs-directory) "~.emacs.d")
                       ("l" my/find-elpa-directory nil)
                       ("L" ,(file-name-concat user-emacs-directory "elpa") "elpa")
                       ("p" "SoundFontsPlus" nil)
                       ("s" "init-files/shells" "shells")
                       ("u" my/find-user-lisp-file nil)
                       ("U" ,(my/user-lisp) "user-lisp")
                       ("2" "SF2Lib" nil)))

(defvar my/point-jumps-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'consult-register-store)
    (define-key map "j" #'consult-register-load)
    map)
  "Keymap for quick Dired jumps.")

(when (my/is-graphical)
  (my/emacs-key-bind global-map
                     ;; NOTE: these conflict with terminal escape sequences so only use on graphical displays
                     "M-O" #'my/ace-window-always-dispatch
                     "M-[" #'previous-buffer
                     "M-]" #'next-buffer))

;; Populate two key maps with hyper-key definitions. The first -- global -- holds the mapping that uses the real `Hyper'
;; modifier. The second keymap -- `my/hyper-keys-map` -- holds the mapping that uses a keychord to activate which is
;; useful on terminals that do not offer a `Hyper' modifier.
(autoload 'my/keycast-mode "my-keycast-mode")
(autoload 'my/key-chord-mode-init "my-key-chord-mode-init")
(let ((hyper-mappings (list "H-SPC" #'my/set-mark-deactivate
                            "H-." #'my/goto-mark
                            "H-1" #'delete-other-windows
                            "H-2" #'split-window-below
                            "H-4" #'other-window-prefix ; was ctl-x-4-prefix
                            "H-5" #'other-frame-prefix  ; was ctl-x-5-prefix
                            "H-a" #'my/ace-window-always-dispatch
                            "H-b" #'consult-buffer
                            "H-B" #'consult-project-buffer
                            "H-c" my/hyper-c-map
                            "H-d" #'crux-duplicate-current-line-or-region
                            "H-f" #'consult-flymake
                            "H-g" #'magit-status
                            "H-h" #'my/describe-symbol-at-point
                            "H-j" my/point-jumps-map
                            "H-k" #'bury-buffer
                            "H-K" #'my/shell
                            "H-M-m" #'my/show-messages-buffer
                            "H-n" my/hyper-n-map
                            "H-o" #'popper-kill-latest-popup
                            "H-p" project-prefix-map
                            "H-M-p" #'my/matching-paren
                            "H-r" #'my/reload-buffer
                            "H-s" my/symbol-overlay-map
                            "H-S" #'speedbar
                            "H-t" #'my/htop
                            "H-u" #'undo
                            "H-v" #'diff-hl-show-hunk
                            "H-w" #'my/ace-window-prefix
                            "H-z" #'my/shell
                            "H-," #'my/customize-search
                            "H-<f1>" #'my/keycast-mode
                            "H-;" #'my/matching-paren
                            "H-<left>" #'backward-word
                            "H-<right>" #'forward-word)))

  (apply #'my/emacs-key-bind global-map hyper-mappings)

  ;; Perform key-chord bindings later, after we know that they will be enabled.
  (add-hook 'emacs-startup-hook (lambda ()
                                  (my/key-chord-mode-init hyper-mappings))
            90))

(defvar my/info-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'consult-info)
    (define-key map "e" #'my/consult-info-emacs)
    (define-key map "i" #'info)
    map)
  "Keymap for canned info manual searches.")

(keymap-set help-map "i" my/info-keys-map)

(when (eq system-type 'gnu/linux)
  (set-face-background 'default "undefined"))

(when (eq system-type 'darwin)
  (custom-set-variables
   '(insert-directory-program "gls"))
  (when (display-graphic-p)
    (custom-set-variables
     '(frame-resize-pixelwise t))))

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097 Basically, put backup and autosave files in their
;; own directories inside our own `~/tmp' directory.
(let ((backup-dir (file-name-concat (my/tmp-dir) "emacs_backups"))
      (auto-saves-dir (file-name-concat (my/tmp-dir) "emacs_autosaves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (custom-set-variables
   `(backup-directory-alist '(("." . ,backup-dir)))
   `(auto-save-file-name-transforms '((".*" ,auto-saves-dir t)))
   ;; Tramp as well but note slight change in pattern
   `(tramp-backup-directory-alist '((".*" . ,backup-dir)))
   `(tramp-auto-save-directory ,auto-saves-dir))
  (setq auto-save-list-file-prefix (file-name-concat auto-saves-dir ".saves-")))

(define-skeleton add-message-field
  "Blah."
  "Field name: "
  "<field name='" str "' required='N' />\n")

(define-skeleton add-field-definition
  "Blah."
  "Field name: "
  > str " = \"" _ "\"\n")

(provide 'init)

;;; init.el ends here.

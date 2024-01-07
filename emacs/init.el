;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(require 'seq)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))

;; (debug-on-entry 'file-notify-add-watch)

(defconst my/venv (expand-file-name "~/venv")
  "The Python virtual environment to use for elpy.")

(defconst my/venv-python (concat my/venv "/bin/python")
  "The path to the Python eexecutable to use for elpy.")
(defconst is-macosx (eq system-type 'darwin)
  "T if running on macOS.")
(defconst is-terminal (eq window-system nil)
  "T if running in a terminal.")

(setenv "WORKON_HOME" my/venv)

(setq read-process-output-max (* 1024 1024)
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      load-path (append (list (expand-file-name "~/.emacs.d/lisp")) load-path)
      frame-title-format (list  '(:eval (abbreviate-file-name default-directory)))
      scroll-conservatively 101
      scroll-margin 2)

(defconst my/screen-laptop (intern "my/screen-laptop")
  "Symbol for laptop screen width.")

(defconst my/screen-4k (intern "my/screen-4k")
  "Symbol for 4K screen width.")

(defconst my/screen-laptop-4k (intern "my/screen-laptop-4k")
  "Symbol for laptop + 4K screen width.")

(defconst my/screen-4k-4k (intern "my/screen-4k-4k")
  "Symbol for 4K + 4K screen width.")

(defconst my/screen-laptop-4k-4k (intern "my/screen-laptop-4k-4k")
  "Symbol for laptop + 4K + 4K screen width.")

(defconst my/screen-terminal (intern "my/screen-terminal")
  "Symbol for terminal screen width.")

(defvar my/right-frame-alist default-frame-alist
  "Definition for a frame aligned on right side of display.")

(defun my/screen-layout ()
  "Identify current screen layout.
Uses result from `display-pixel-width' to determine what monitors
there are. Better would be to use `display-monitor-attributes-list'
like done in `my/frame-top'.

Returns one of the follow symbols based on width:

- `my/screen-laptop' -- only laptop screen
- `my/screen-4k' -- only 4K monitor.
- `my/screen-laptop-4k' -- laptop screen + 4K monitor.
- `my/screen-4k-4k' -- two 4K monitors.
- `my/screen-laptop-4k-4k' -- laptop screen + 2 4K monitors.
- `my/screen-terminal' -- unknown screen."
  (let* ((laptop-width 2056)
         (4k-width 3840)
         (width (display-pixel-width nil))
         (value (cond ((= width laptop-width) my/screen-laptop)
                      ((= width 4k-width) my/screen-4k)
                      ((= width (+ laptop-width 4k-width)) my/screen-laptop-4k)
                      ((= width (+ 4k-width 4k-width)) my/screen-4k-4k)
                      ((= width (+ laptop-width 4k-width 4k-width) my/screen-laptop-4k-4k))
                      (t my/screen-terminal))))
    (message "my/screen-layout: %s" value)
    value))

(defun my/is-laptop (layout)
  "T if LAYOUT is laptop."
  (eq layout my/screen-laptop))

(defun my/is-4k (layout)
  "T if LAYOUT is kind with at least 4K area."
  (let ((value (or (eq layout my/screen-4k)
                   (eq layout my/screen-laptop-4k)
                   (eq layout my/screen-4k-4k)
                   (eq layout my/screen-laptop-4k-4k))))
    (message "my/is-4k: %s" value)
    value))

(defun my/font-size (layout)
  "The font size to use based on the LAYOUT."
  (if (my/is-4k layout) 16 12))

(defun my/rows (layout)
  "The number of rows to show in a frame shown on LAYOUT."
  (if (my/is-4k layout) 102 (if (my/is-laptop layout) 88 40)))

(defun my/cols (layout)
  "The number of columns to show in a frame shown on LAYOUT."
  (if (or (my/is-4k layout) (my/is-laptop layout)) 132 80))

(defun my/frame-pixel-width (layout)
  "Width in pixels of a normal frame shown on LAYOUT."
  (if (my/is-4k layout) 1338 944))

(defun my/frame-initial-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT."
  (if (or (eq layout my/screen-laptop-4k) (eq layout my/screen-laptop-4k-4k)) 2056 0))

(defun my/frame-default-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT."
  (+ (my/frame-initial-left layout) (my/frame-pixel-width layout)))

(defun my/frame-third-left (layout)
  "The offset to the `alt' window based on LAYOUT."
  (- (display-pixel-width nil) (my/frame-pixel-width layout)))

(defconst my/font-name "Berkeley Mono"
  "The name of the font to use.")

(defun my/frame-top ()
  "The top of the display area.
NOTE: this assumes that the laptop display if present is al"
  (let ((settings (display-monitor-attributes-list)))
    (nth 2 (if (eq (length settings) 1)
               (car (car settings))
             (car (car (cdr settings)))))))

(defun my/initial-frame-alist (layout)
  "Make alist to use for the initial frame on LAYOUT."
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-initial-left layout))))

(defun my/default-frame-alist (layout)
  "Make alist to use for the default frame on LAYOUT."
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-default-left layout))))

(defun my/align-right-frame-alist (layout)
  "The alist to use extra frame on LAYOUT.
The frame will appear on the far right of the display area."
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-third-left layout))))

(defun my/setup-font (layout)
  "Install the desired font in the default face for LAYOUT."
  (set-face-attribute 'default nil :font (font-spec :family my/font-name :size (my/font-size layout))))

(defun my/update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT."
  (setq initial-frame-alist (my/initial-frame-alist layout)
        default-frame-alist (my/default-frame-alist layout)
        my/right-frame-alist (my/align-right-frame-alist layout))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist)
  (message "  right-frame: %s" my/right-frame-alist))

(defun my/screen-layout-changed ()
  "Recalculate values based on screen layout."
  (let ((layout (my/screen-layout)))
    (message "screen layout: %s" layout)
    (my/setup-font layout)
    (my/update-screen-frame-alists layout)))

(my/update-screen-frame-alists (my/screen-layout))

(add-hook 'after-init-hook (lambda () (my/screen-layout-changed)))

(when scroll-bar-mode
  (scroll-bar-mode -1))

(when is-macosx
  (eval-when-compile
    (require 'ls-lisp))
  (custom-set-variables
   '(ls-lisp-use-insert-directory-program nil))
  (when (not is-terminal)
    (setq frame-resize-pixelwise t)
    (custom-set-variables
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-command-modifier 'super)
     '(mac-right-option-modifier 'hyper))))

(let* ((common-paths (list (expand-file-name "~/bin")
                           (concat my/venv "/bin")))
       (macosx-paths (if is-macosx
                         (list "/opt/homebrew/sqlite/bin"
                               "/opt/homebrew/opt/grep/libexec/gnubin"
                               "/opt/homebrew/bin")
                       '()))
       (additional-paths (seq-filter #'file-directory-p (append common-paths macosx-paths))))
  ;; Set exec-path to contain the above paths
  (setq exec-path (append additional-paths exec-path))
  ;; Same for PATH environment variable
  (setenv "PATH" (concat (mapconcat 'identity (append additional-paths (list (getenv "PATH"))) ":"))))

(when (eq window-system nil)
  (set-face-background 'default "undefined"))

(defvar font-lock-brace-face
  (defface font-lock-brace-face
    '((((class color) (background light))
       (:foreground "red"))
      (((class color) (background dark))
       (:foreground "red")))
    ;; (:foreground "deep pink")))
    "Font Lock mode face used to highlight parentheses, braces, and brackets."
    :group 'font-lock-faces)
  "Font Lock mode face used to highlight parentheses, braces, and brackets.")

(autoload 'emacs-pager "emacs-pager")
(autoload 'my/lisp-mode-hook "my-lisp-mode")
(add-hook 'lisp-mode-hook #'my/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook #'my/lisp-mode-hook)
(add-hook 'scheme-mode-hook #'my/lisp-mode-hook)
(autoload 'my/lisp-data-mode-hook "my-lisp-mode")
(add-hook 'lisp-data-mode-hook #'my/lisp-data-mode-hook)
(autoload 'my/cmake-mode-hook "my-cmake-mode")
(add-hook 'cmake-mode-hook #'my/cmake-mode-hook)
(autoload 'my/c++-mode-hook "my-c++-mode")
(add-hook 'c++-mode-hook #'my/c++-mode-hook)
(autoload 'my/sh-mode-hook "my-sh-mode")
(add-hook 'sh-mode-hook #'my/sh-mode-hook)
(autoload 'my/shell-mode-hook "my-shell-mode")
(add-hook 'shell-mode-hook #'my/shell-mode-hook)
(autoload 'my/makefile-mode-hook "my-makefile-mode")
(add-hook 'makefile-mode-hook #'my/makefile-mode-hook)

;; (unless (daemonp)
;;   (use-package session
;;     :ensure t
;;     :hook (after-init . session-initialize)))

(when (display-graphic-p)
  (use-package doom-themes
    :ensure t
    :defer t
    :functions (doom-themes-visual-bell-config
                doom-themes-neotree-config)
    :config
    (load-theme 'doom-tomorrow-night t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config))

  (use-package all-the-icons
    :ensure t)

  (use-package all-the-icons-completion
    :ensure t
    :commands (all-the-icons-completion-mode)
    :after (marginalia all-the-icons)
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :config (all-the-icons-completion-mode))

  (use-package nerd-icons
    :ensure t)

  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)))

(auto-save-visited-mode t)

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package dired
  :init
  (require 'dired-aux)
  (autoload 'my/dired-mode-hook "my-dired-mode")
  :bind (("C-<up>" . dired-up-directory)
         ("M-u" . dired-up-directory)
         ("C-s" . dired-isearch-filenames)
         ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

(use-package savehist
  :init
  (savehist-mode t))

(use-package ediff
  :defer t
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

;;; Annotate changed files that are under version control.
(use-package diff-hl
  :ensure t
  :defer t
  :commands (global-diff-hl-mode global-diff-hl-show-hunk-mouse-mode diff-hl-flydiff-mode diff-hl-margin-mode)
  :init
  (diff-hl-flydiff-mode t)
  (when is-terminal
    (diff-hl-margin-mode t))
  :hook ((dired-mode . diff-hl-dired-mode)
         (after-init . (lambda ()
                        (diff-hl-flydiff-mode t)
                        (when is-terminal
                          (diff-hl-margin-mode t))
                        (global-diff-hl-mode t)
                        (global-diff-hl-show-hunk-mouse-mode t)
                        ))))

(use-package magit
  :ensure t
  :defer t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))

(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-mode projectile-project-root projectile-register-project-type)
  :bind-keymap ("C-x p" . projectile-command-map)
  :hook (prog-mode . projectile-mode)
  :config
  (projectile-register-project-type 'swift '("Package.swift")
                                    :project-file "Package.swift"
                                    :src-dir "Sources"
                                    :test-dir "Tests"
                                    :compile "swift build"
                                    :test "swift test"
                                    :run "swift run"
                                    :test-suffix ""))

(use-package denote
  :ensure t
  :commands (denote-dired-mode-in-directories)
  :hook ((dired-mode . denote-dired-mode-in-directories))
  :bind (("C-c n n" . denote))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/")
        denote-infer-keywords t
        denote-sort-keywords t
        denote-file-types (cons
                           '(markdown-brh
                             :extension ".md"
                             :date-function (lambda (date) (format-time-string "%F %T"))
                             :front-matter denote-yaml-front-matter
                             :title-key-regexp "^title\\s-*:"
                             :title-value-function denote-trim-whitespace
                             :title-value-reverse-function denote-trim-whitespace
                             :keywords-key-regexp "^tags\\s-*:"
                             :keywords-value-function denote-format-keywords-for-text-front-matter
                             :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                             :link denote-md-link-format
                             :link-in-context-regexp denote-md-link-in-context-regexp)
                           denote-file-types)
        denote-file-type 'markdown-brh))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package embark
  :ensure t
  :commands (embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map`
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c i" . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map`
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ;; ("C-x p b" . consult-project-bookmark)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)

         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map`
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s k" . consult-keep-lines)
         ("M-s l" . consult-line)
         ("M-s M-l" . my/consult-line-symbol-at-point)
         ("M-s L" . consult-line-multi)
         ("M-s r" . consult-ripgrep)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("C-s" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-register-format consult-register-window consult-xref)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (defun my/consult-line-symbol-at-point ()
    "Start `consult-line' with symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (consult-customize
   consult-theme :review-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-a" . marginalia-cycle))
  :init (marginalia-mode))

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init (vertico-mode))

(use-package mode-line-bell
  :ensure t)

(use-package eglot
  :ensure t
  :commands (eglot-ensure)
  :hook ((swift-mode . eglot-ensure)
         (cc-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c r" . eglot-rename)
              ("C-c c f" . eglot-format)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

;; (orderless-define-completion-style orderless+initialism
;;   (orderless-matching-styles '(orderless-initialism
;;                                orderless-literal
;;                                orderless-regexp)))
;; (setq completion-category-overrides
;;       '((command (styles orderless+initialism))
;;         (symbol (styles orderless+initialism))
;;         (variable (styles orderless+initialism)))
;;       orderless-component-separator "[ &]")

(use-package swift-mode
  :ensure t
  :defer t
  :custom ((swift-mode:basic-offset 2))
  :hook (swift-mode . (lambda () (set (make-local-variable 'compile-command) "swift build"))))

(use-package flymake
  :ensure t
  :defer t
  :config (setq elisp-flymake-byte-compile-load-path load-path
                flymake-mode-line-title "FM")
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package cape
  :ensure t
  :commands (cape-file))

(use-package corfu
  :ensure t
  :commands (global-corfu-mode corfu-history-mode)
  :init
  :bind (:map corfu-map
              ("<return>" . corfu-complete))
  :custom ((corfu-cycle t)
           (corfu-auto t)
           (corfu-auto-prefix 1)
           (corfu-popupinfo-mode t)
           (corfu-preselect 'directory))
  :config
  (global-corfu-mode)
  (corfu-history-mode))

(require 'corfu-popupinfo)
(corfu-popupinfo-mode)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package winner
  :hook (after-init . winner-mode))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package server
  :commands (server-running-p)
  :hook (emacs-startup . (lambda () (unless (server-running-p) (server-start)))))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :init (which-key-mode t))

(use-package hl-line
  :commands (global-hl-line-mode)
  :hook (emacs-startup . global-hl-line-mode))

(use-package recentf
  :init (recentf-mode t))

(use-package treesit
  :config
  (setq treesit-language-source-alist '((python "https://github.com/tree-sitter/tree-sitter-python")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (swift "https://github.com/alex-pinkus/tree-sitter-swift"))))

(use-package emacs
  :commands (my/crm-indicator)
  :init
  (defun my/crm-indicator(args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)

  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook ((minibuffer-setup . cursor-intangible-mode)))


(global-prettify-symbols-mode t)

(autoload 'native-complete-setup-bash "native-complete")
(with-eval-after-load 'shell
  (message "Loading native-complete-setup-bash")
  (native-complete-setup-bash))

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(use-package diminish
  :ensure t
  :commands (diminish)
  :config
  (diminish 'abbrev-mode "A")
  (diminish 'auto-fill-function "F")
  (diminish 'subword-mode "S"))

(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Custom functions

(defun my/reset-frame-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/initial-frame-alist layout))))

(defun my/reset-frame-right ()
  "Reset frame size and position for right frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/default-frame-alist layout))))

(defun my/reset-frame-right-display ()
  "Reset frame size and position for right side of display frame."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/align-right-frame-alist layout))))

(defun my/reset-framewidth ()
  "Reset the current frame width to function `my/cols'."
  (interactive)
  (let ((layout (my/screen-layout)))
    (set-frame-width (window-frame (get-buffer-window)) (my/cols layout))))

(defun ksh ()
  "Start a new shell."
  (interactive)
  (let ((tmp (get-buffer-create "*Shell*")))
    (switch-to-buffer tmp nil t)
    (shell tmp)))

(defun my/shell-other-window ()
  "Start a new shell in another window."
  (interactive)
  (let ((tmp (get-buffer-create "*Shell*")))
    (switch-to-buffer-other-window tmp)
    (ksh)))

(defun my/shell-other-frame ()
  "Start a new shell in another frame."
  (interactive)
  (select-frame (make-frame))
  (ksh))

(defun my/kill-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/info-other-frame ()
  "Show Info in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*info*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (info nil tmp)))

(defun my/customize-other-window ()
  "Show Customize in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (switch-to-buffer-other-window tmp)
    (customize)))

(defun my/customize-other-frame ()
  "Show Customize in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (customize)))

(defun my/matching-paren (arg)
  "Locate the matching ARG paren."
  (interactive "P")
  (if arg
      (self-insert-command 1); (insert "%")
    (cond ((looking-at "[[({]")
	   (forward-sexp 1)
	   (forward-char -1))
	  ((looking-at "[]})]")
	   (forward-char 1)
	   (forward-sexp -1))
	  (t
	   (self-insert-command 1)))))

(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defmacro my/emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'copyright-update)

(my/emacs-keybind global-map
  "C-h K" #'describe-keymap
  "C-h u" #'apropos-user-option
  "C-h F" #'apropos-function
  "C-h V" #'apropos-variable
  "C-h L" #'apropos-library
  "C-h c" #'describe-char

  "M-g d" #'dired-jump

  "C-x O" #'other-frame
  "C-x C-o" #'other-frame

  "M-<f1>" #'my/reset-frame-left
  "M-<f2>" #'my/reset-frame-right
  "M-<f3>" #'my/reset-frame-right-display

  "C-x 4 c" #'my/customize-other-window
  "C-x 4 k" #'my/shell-other-window

  "C-x 5 c" #'my/customize-other-frame
  "C-x 5 i" #'my/info-other-frame
  "C-x 5 k" #'my/shell-other-frame

  "C-c C-k" #'my/kill-buffer

  "<f3>" #'eval-last-sexp

  "C-M-\\" #'my/indent-buffer

  "<home>" #'beginning-of-buffer
  "<end>" #'end-of-buffer
  "<delete>" #'delete-char
  "S-<f12>" #'package-list-packages

  ;; Unmap the following
  "<insert>" nil
  "C-z" nil
  "C-x C-z" nil
  "C-x h" nil
  "C-h h" nil

  ;; "M-`" nil

  "C-<mouse-4>" nil
  "C-<mouse-5>" nil

  "C-<double-mouse-4>" nil
  "C-<double-mouse-5>" nil

  "C-<triple-mouse-4>" nil
  "C-<triple-mouse-5>" nil

  "M-[" #'previous-buffer
  "M-]" #'next-buffer
  )

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(provide 'init)

;;; init.el ends here.

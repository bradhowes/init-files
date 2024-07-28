; init.el --- load the full configuration -*- lexical-binding: t; -*-
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
(defconst is-terminal (not (display-graphic-p))
  "T if running in a terminal.")

(setenv "WORKON_HOME" my/venv)

(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil
      custom-file (locate-user-emacs-file "custom.el")
      load-path (append (list (expand-file-name "~/.emacs.d/lisp")
                              (expand-file-name "~/.emacs.d/lisp/consult-notes"))
                        load-path)
      frame-title-format (list  '(:eval (abbreviate-file-name default-directory))))

(defconst my/screen-laptop (intern "my/screen-laptop")
  "Symbol to indicate display is MacBook Pro 16\" laptop screen.")

(defconst my/screen-4k (intern "my/screen-4k")
  "Symbol to indicate display is 4K screen.")

(defconst my/screen-laptop-4k (intern "my/screen-laptop-4k")
  "Symbol to indicate display width is laptop and 1 4K screen.")

(defconst my/screen-4k-4k (intern "my/screen-4k-4k")
  "Symbol to indicate display width is 2 4K screens.")

(defconst my/screen-laptop-4k-4k (intern "my/screen-laptop-4k-4k")
  "Symbol to indicate display width is laptop and 2 4K screens.")

(defconst my/screen-terminal (intern "my/screen-terminal")
  "Symbol to indicate display is a terminal.")

(defconst my/laptop-screen-width 2056
  "MacBook Pro 16\" M1 screen width in pixels.")

(defconst my/4k-screen-width 3840
  "4K external display width in pixels.")

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defcustom my/screen-4k-pick 0
  "The 4K screen to use for Emacs frames."
  :type '(natnum)
  :options '(0 1)
  :group 'my/customizations)

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
  (let* ((width (display-pixel-width nil))
         (value (cond ((= width my/laptop-screen-width) my/screen-laptop)
                      ((= width my/4k-screen-width) my/screen-4k)
                      ((= width (+ my/laptop-screen-width my/4k-screen-width)) my/screen-laptop-4k)
                      ((= width (* my/4k-screen-width 2)) my/screen-4k-4k)
                      ((= width (+ my/laptop-screen-width (* 2 my/4k-screen-width))) my/screen-laptop-4k-4k)
                      (t my/screen-terminal))))
    (message "my/screen-layout: %s" value)
    value))

(defun my/is-laptop (layout)
  "T if LAYOUT is laptop."
  (eq layout my/screen-laptop))

(defun my/is-4k (layout)
  "T if LAYOUT is kind with at least 4K area."
  (memq layout '(my/screen-4k my/screen-laptop-4k my/screen-4k-4k my/screen-laptop-4k-4k)))

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
  "Width in pixels of a normal frame shown on LAYOUT.
These values are hard-coded based on current settings.
Probably a better way to figure this out."
  (if (my/is-4k layout) 1338 944))

(defun my/frame-initial-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be used for the `initial-frame-alist' configuration."
  ;; Use the first external monitor if there is one.
  (if (memq layout '(my/screen-laptop-4k my/screen-laptop-4k-4k))
      (+ 2056 (* my/screen-4k-pick my/4k-screen-width))
    0))

(defun my/frame-default-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be use for the `default-frame-aliat' configuration."
  (+ (my/frame-initial-left layout) (my/frame-pixel-width layout)))

(defun my/frame-third-left (layout)
  "The offset to the `alt' window based on LAYOUT.
This is not used in any particular `*-frame-alist' but it is used
by custom commands that reposition a frame to be flush with the
right-side of the active screen that is being used to host
Emacs frames."
  (- (+ (my/frame-initial-left layout)
        (if (eq layout my/screen-laptop)
            my/laptop-screen-width
          my/4k-screen-width))
     (my/frame-pixel-width layout)))

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
        default-frame-alist (my/default-frame-alist layout))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist))

(defun my/screen-layout-changed ()
  "Recalculate values based on screen layout."
  (interactive)
  (let ((layout (my/screen-layout)))
    (message "screen layout: %s" layout)
    (my/setup-font layout)
    (my/update-screen-frame-alists layout)))

(defun my/pick-screen-4k (screen)
  "Set the 4K SCREEN to use to host future Emacs frames.
It does not affect existing frames."
  (interactive "NScreen:")
  (custom-set-variables (list 'my/screen-4k-pick screen))
  (custom-save-all)
  (my/screen-layout-changed))

;; (my/update-screen-frame-alists (my/screen-layout))

(add-hook 'after-init-hook (lambda () (my/screen-layout-changed)))

(when scroll-bar-mode
  (scroll-bar-mode -1))

(unless tab-bar-mode
  (tab-bar-mode 1))

;;; Set various paths

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

;; (use-package ef-themes
;;   :ensure t)

;; Configure `display-buffer-alist' to manage window placement

(use-package window
  :init
  (let ((window-parameters '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
    (message "%s" window-parameters)
    (setq switch-to-buffer-in-dedicated-window 'pop
          switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          window-sides-slots '(0 0 3 1) ; left top right bottom
          display-buffer-base-action '((display-buffer--maybe-same-window
                                        display-buffer-reuse-window
                                        display-buffer-in-previous-window
                                        display-buffer-reuse-mode-window
                                        display-buffer-pop-up-window
                                        display-buffer-same-window
                                        display-buffer-use-some-window))
          display-buffer-alist nil
          ;; display-buffer-alist `(("\\*\\(?:\\(?:Buffer List\\)\\|Ibuffer\\|\\(?:.* Buffers\\)\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . -2) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*Tags List\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . -1) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*\\(?:Help\\|grep\\|Completions\\|Apropos\\|ripgrep-search\\|\\(?:Customize Option:.*\\)\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . 0) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*\\(?:\\compilation\\|Compile-Log\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . 1) (preserve-size . (t . nil)) ,window-parameters))
          )))

(when is-terminal
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

(defun my/autoloads (&rest definitions)
  "Setup autoloads.
Each value in `DEFINITIONS' is a cons made up of
a filename (without extension) and a symbol or a
list of symbols."
  (let* ((groups (seq-group-by #'stringp definitions))
         (files (seq-drop (elt groups 0) 1))
         (symbols (seq-drop (elt groups 1) 1)))
    (seq-mapn
     (lambda (file symbols)
       (if (consp symbols)
           (mapcar (lambda (symbol) (autoload symbol file)) symbols)
         (autoload symbols file)))
     files symbols)))

;; Modes

(my/autoloads
 "emacs-pager" 'emacs-pager
 "my-cmake-mode" 'my/cmake-mode-hook
 "my-c++-mode" 'my/c++-mode-hook
 "my-dired-mode" 'my/dired-mode-hook
 "my-lisp-mode" '(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" 'my/makefile-mode-hook
 "my-python-mode" '(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" 'my/sh-mode-hook
 "my-shell-mode" 'my/shell-mode-hook)

;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

(use-package lisp-mode
  :defer t
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  :hook ((c++-mode . my/c++-mode-hook)))

(use-package sh-mode
  :defer t
  :hook ((sh-mode . my/sh-mode-hook)))

(use-package shell-mode
  :defer t
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package makefile-mode
  :defer t
  :hook ((makefile-mode . my/makefile-mode-hook)))

(defun my/emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  ;; Partition `definitions' into two groups, one with key definitions
  ;; and another with functions and/or nil values
  (let* ((groups (seq-group-by #'stringp definitions))
         (keys (seq-drop (elt groups 0) 1))
         (commands (seq-drop (elt groups 1) 1)))
    ;; Only execute if given a valid keymap
    (when-let ((keymapp keymap))
      (seq-mapn
       (lambda (key command) (define-key keymap (kbd key) command))
       keys commands))))

;;; PACKAGES

(when (display-graphic-p)
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
    :hook (after-init . doom-modeline-mode))
  )

;; Clean up whitespace at end of lines but only for edited lines.
(use-package ws-butler
  :ensure t
  :defer nil
  :diminish " ~"
  :hook (prog-mode . ws-butler-mode))

(use-package diminish
  :ensure t
  :commands (diminish)
  :config
  (diminish 'abbrev-mode " A")
  (diminish 'isearch-mode " ?")
  (diminish 'overwrite-mode "*"))

(use-package eldoc)
;; :bind ("C-h ." . eldoc-print-current-symbol-info))

(use-package eldoc-box
  :ensure t
  :diminish (eldoc-box-hover-mode . " eB")
  :commands (eldoc-box-hover-mode)
  ;; :hook (eldoc-mode-hook . (lambda () (eldoc-box-hover-mode t)))
  :config
  ;; (setq eldoc-box-at-point-position-function eldoc-box-position-function)
  :bind ("C-h ." . eldoc-box-help-at-point))

(use-package dired
  :defer t
  :init
  (require 'dired-aux)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

;; Save minibuffer histories
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
  :hook ((after-init . (lambda ()
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
  :bind (("C-c f" . magit-file-dispatch)
         ("C-x g" . magit-status)))

(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-mode projectile-project-name projectile-register-project-type)
  :bind-keymap (("C-x p" . projectile-command-map)
                ("M-s-p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :config
  (setq projectile-mode-line-function (lambda () (format " [%s] " (projectile-project-name))))
  (projectile-register-project-type 'swift '("Package.swift")
                                    :project-file "Package.swift"
                                    :src-dir "Sources"
                                    :test-dir "Tests"
                                    :compile "swift build"
                                    :test "swift test"
                                    :run "swift run"
                                    :test-suffix ""))

;; Magit-like interface for ripgrep
(use-package rg
  :ensure t
  :after (projectile)
  :commands (rg-enable-default-bindings)
  :bind (:map projectile-command-map
              ("s r" . rg-project))
  :config
  (rg-enable-default-bindings))

(use-package denote
  :ensure t
  :defer t
  :commands (denote-dired-mode-in-directories)
  :hook (dired-mode . denote-dired-mode-in-directories)
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
  ;; :commands (embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  ;; :init
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  :after (projectile)
  :bind (;; C-c bindings in `mode-specific-map`
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-h i" . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ;; ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map`
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x f" . consult-recent-file)

         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("C-x r l" . consult-bookmark)

         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)
         ;; ("C-M-#" . consult-register)

         ("M-y" . consult-yank-replace)

         ;; M-g bindings for goto
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
         ("M-s i" . consult-info)
         ("M-s k" . consult-keep-lines)
         ("M-s l" . consult-line)
         ("M-s M-s" . my/consult-line-symbol-at-point)
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
         ("C-s" . consult-history)

         :map projectile-command-map
         ("b" . consult-project-buffer))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-register-format consult-register-window consult-xref projectile-project-root)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (defun my/consult-line-symbol-at-point ()
    "Start `consult-line' with symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-bookmark consult--source-file-register
                     consult--source-recent-file consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-notes
  :defer f
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  (consult-notes-denote-mode)
  (setq consult-notes-denote-files-function #'denote-directory-files)
  :bind (("C-c n b" . consult-notes)))

(use-package embark-consult
  :ensure t
  :after (consult embark)
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
  :init (vertico-mode t))

(use-package mode-line-bell
  :ensure t)

(use-package eglot
  :ensure t
  :commands (eglot-ensure)
  :hook ((swift-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c r" . eglot-rename)
              ("C-c c f" . eglot-format)))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

(use-package python
  :ensure t
  :hook ((python-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

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
  ;; :bind (:map corfu-map
  ;;             ("<return>" . corfu-complete))
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
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo))
  :hook (after-init . winner-mode))

(use-package cmake-mode
  :ensure t
  :defer t
  :hook ((cmake-mode . my/cmake-mode-hook)))

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

(use-package ispell
  :config (setq-default ispell-local-dictionary "english"))

(use-package flyspell
  :defer t
  :config (setq flyspell-mode-line-string "s")
  :hook (prog-mode . flyspell-prog-mode))

(use-package ibuffer
  :config
  ;; Unbind the ibuffer use of "M-o"
  (my/emacs-keybind ibuffer-mode-map
		    "M-o" nil))

(use-package windmove
  :bind (("A-M-<left>" . windmove-left)
         ("A-M-<right>" . windmove-right)
         ("A-M-." . windmove-up)
         ("A-M-<up>" . windmove-up)
         ("A-M-," . windmove-down)
         ("A-M-<down>" . windmove-down)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package popper
  :ensure t
  :defines (popper-reference-buffers)
  :commands (popper-mode popper-echo-mode)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package char-menu
  :ensure t
  :defines (char-menu)
  :bind (("C-z" . char-menu))
  :init
  (setq char-menu
        '("—" "‘’" "“”" "…" "«»" "–"
          ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
          ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
          ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
          ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))))

(use-package emacs
  :commands (my/crm-indicator)
  :init
  ;; (auto-save-visited-mode t)
  (ffap-bindings)
  (setq-default abbrev-mode t)

  ;; Forgot why this was done -- most likely for macOS
  ;; (put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  (defun my/crm-indicator(args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)

  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (before-save . copyright-update)))

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

(defun my/remove-all-text-properties ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

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

(defun repl ()
  "Simple alias to start ielm."
  (interactive)
  (ielm))

(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(my/emacs-keybind global-map
                  "C-h K" #'describe-keymap
                  "C-h u" #'apropos-user-option
                  "C-h F" #'apropos-function
                  "C-h V" #'apropos-variable
                  "C-h L" #'apropos-library
                  "C-h c" #'describe-char

                  "C-c r" #'ielm

                  "M-g d" #'dired-jump

                  "C-o" #'other-window

                  "C-x 0" #'delete-other-windows
                  "C-x O" #'other-frame
                  "C-x C-o" #'other-frame
                  ;; "C-x k" #'bury-buffer

                  "C-x C-b" #'ibuffer

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

                  "M-[" #'previous-buffer
                  "M-]" #'next-buffer

                  "%" #'my/matching-paren

                  ;; Unmap the following
                  "<insert>" nil
                  "C-z" nil
                  "C-x C-z" nil
                  "C-x h" nil
                  "C-h h" nil

                  ;; Disable font size changes via trackpad
                  "C-<wheel-up>" #'ignore
                  "C-<wheel-down>" #'ignore

                  ;; "M-`" nil

                  ;; "C-<mouse-4>" nil
                  ;; "C-<mouse-5>" nil

                  ;; "C-<double-mouse-4>" nil
                  ;; "C-<double-mouse-5>" nil

                  ;; "C-<triple-mouse-4>" nil
                  ;; "C-<triple-mouse-5>" nil
                  )

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(defun my/set-bound-var (symbol value)
  "Set SYMBOL with VALUE if symbol exists."
  (when (boundp symbol)
    (set symbol value)))

(if is-macosx
    (progn
      (eval-when-compile
        (require 'ls-lisp))
      (custom-set-variables
       '(ls-lisp-use-insert-directory-program nil))
      (when (not is-terminal)
        (custom-set-variables
         '(frame-resize-pixelwise t)
         '(mac-command-modifier 'meta)
         '(mac-option-modifier 'alt)
         '(mac-right-command-modifier 'super)
         '(mac-right-option-modifier 'hyper))))
  (when (eq window-system 'x)
    (my/set-bound-var 'x-alt-keysym 'alt)
    (my/set-bound-var 'x-meta-keysym 'meta)
    (my/set-bound-var 'x-super-keysym 'super)
    (my/set-bound-var 'x-hyper-keysym 'hyper)))

(provide 'init)

;;; init.el ends here.

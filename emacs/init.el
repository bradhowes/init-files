;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defconst my/venv (expand-file-name "~/venv")
  "The Python virtual environment to use for elpy.")

(setenv "WORKON_HOME" my/venv)

(defconst my/venv-python (concat my/venv "/bin/python")
  "The path to the Python executable to use for elpy.")

(defconst my/is-macosx (eq system-type 'darwin)
  "T if running on macOS.
Note that this is also true when running in a terminal window.")

(defconst my/is-linux (eq system-type 'gnu/linux)
  "T if running on GNU/Linux system.
Note that this is also true when running in a terminal window.")

(defconst my/is-terminal (not (display-graphic-p))
  "T if running in a terminal.")

(defconst my/is-x-windows (eq window-system 'x)
  "T if running in an X windows environment.")

(defconst my/is-x-windows-on-win (and my/is-x-windows (getenv "XTERM_SHELL"))
  "T if running in VcXsrv on Windows.
Hacky but for now it works since we are always starting up an initial xterm.")

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

(defconst my/workspace-name (or (getenv "WORKSPACE_NAME") "")
  "The value of WORKSPACE_NAME environment variable.")

(defconst my/font-name "Berkeley Mono"
  "The name of the font to use.")

(defcustom my/screen-4k-pick 0
  "The 4K screen to use for Emacs frames."
  :type '(natnum)
  :options '(0 1)
  :group 'my/customizations)

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

(defun my/screen-layout ()
  "Identify current screen layout.
Uses result from `display-pixel-width' to determine what monitors
there are.  Better would be to use `display-monitor-attributes-list'
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

(defun my/rows (layout)
  "The number of rows to show in a frame shown on LAYOUT."
  (if (my/is-4k layout) 104 (if (my/is-laptop layout) 88 40)))

(defun my/cols (layout)
  "The number of columns to show in a frame shown on LAYOUT."
  (if (or (my/is-4k layout) (my/is-laptop layout)) 132 80))

(defun my/frame-pixel-width (layout)
  "Width in pixels of a normal frame shown on LAYOUT.
These values are hard-coded based on current settings.
Probably a better way to figure this out."
  (if (my/is-4k layout) (if my/is-macosx 1338 1338) 944))

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

(defun my/update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT."
  (setq initial-frame-alist (my/initial-frame-alist layout)
        default-frame-alist (my/default-frame-alist layout))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist))

(defun my/font-size (layout)
  "The font size to use based on the LAYOUT."
  (if (my/is-4k layout) 16 12))

(defun my/setup-font (layout)
  "Install the desired font in the default face for LAYOUT."
  (set-face-attribute 'default nil :font (font-spec :family my/font-name :size (my/font-size layout))))

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

(add-hook 'after-init-hook (lambda () (my/screen-layout-changed)))

;;; Set various paths
(require 'seq)

(push (file-truename "~/.emacs.d/lisp") load-path)
(let* ((common-paths (list (file-truename "~/bin")
                           (concat my/venv "/bin")))
       (macosx-paths (if my/is-macosx
                         (list "/opt/homebrew/sqlite/bin"
                               "/opt/homebrew/opt/grep/libexec/gnubin"
                               "/opt/homebrew/bin")
                       '()))
       (additional-paths (seq-filter #'file-directory-p (append common-paths macosx-paths))))
  ;; Set exec-path to contain the above paths
  (setq exec-path (append additional-paths exec-path))
  ;; Same for PATH environment variable
  (setenv "PATH" (concat (string-join additional-paths ":") ":" (getenv "PATH"))))

(if (or my/is-macosx (string= (system-name) "ldzls1144d"))
    (use-package package
      :custom
      (package-archive-priorities '(("melpa" . -5)
                                    ("melpa-stable" . 10)
                                    ("gnu" . 15)
                                    ("nongnu" . 20)))
      :config
      (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
      (package-initialize))
  (use-package package
    :init
    (package-initialize)))

;; Configure `display-buffer-alist' to manage window placement

(use-package ace-window
  :commands (aw-window-list aw-switch-to-window aw-select aw-flip-window ace-display-buffer ace-window)
  :defines (aw-dispatch-always)
  :config
  (setq aw-make-frame-char ?n))

(use-package window
  :init
  (let ((window-parameters '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
    (message "%s" window-parameters)
    (setq switch-to-buffer-in-dedicated-window 'pop
          switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          window-sides-slots '(0 0 3 1) ; left top right bottom
          display-buffer-base-action '((display-buffer-reuse-window ace-display-buffer))
          display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window ace-display-buffer) (reusable-frames . nil))
                                 ("\\*R" nil (reusable-frames . nil))
                                 ,(cons "\\*helm" display-buffer-fallback-action)
                                 ("magit-log" nil (inhibit-same-window . t))
                                 ("magit-diff:" nil (inhibit-same-window . t))))))

(when my/is-terminal
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
  "Setup autoloads for my mode customizations.
DEFINITIONS is a sequence of string and symbol pairs, where the
string is a filename (without extension), and the following symbol
is either a standalone symbol or a list of symbols that represent
the items to setup for autoloading from the given file."
  (let* ((groups (seq-group-by #'stringp definitions))
         (files (seq-drop (elt groups 0) 1))
         (symbols (seq-drop (elt groups 1) 1)))
    (seq-mapn
     (lambda (file symbols)
       (if (consp symbols)
           (mapcar (lambda (symbol) (autoload symbol file)) symbols)
         (autoload symbols file)))
     files symbols)))

(my/autoloads
 "emacs-pager" 'emacs-pager
 "my-cmake-mode" 'my/cmake-mode-hook
 "my-c++-mode" 'my/c++-mode-hook
 "my-dired-mode" 'my/dired-mode-hook
 "my-lisp-mode" '(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" 'my/makefile-mode-hook
 "my-markdown-mode" 'my/markdown-mode-hook
 "my-python-mode" '(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" 'my/sh-mode-hook
 "my-json-mode" 'my/json-mode-hook
 "my-js2-mode" 'my/js2-mode-hook
 "my-shell-mode" 'my/shell-mode-hook)

(use-package key-chord
  :commands (key-chord-define))

(use-package org
  :commands (org-store-link)
  :config
  (defvar my/org-key-map (make-sparse-keymap)
    "Keymap for my org mode access.")
  (keymap-set my/org-key-map "a" #'org-agenda)
  (keymap-set my/org-key-map "c" #'org-capture)
  (keymap-set my/org-key-map "l" #'org-store-link)
  :bind-keymap ("H-o" . my/org-key-map))

(use-package tempo
  :commands (tempo-define-template))

(defun tempo-template-my/org-emacs-lisp-source ()
  "Define empty function to satisfy flymake/byte-compile.")

(tempo-define-template "my/org-emacs-lisp-source" '("#+begin_src emacs-lisp" & r % "#+end_src")
                       "<m"
                       "Insert an Emacs Lisp source block in an org document.")

(defun my/org-emacs-lisp-source-with-indent ()
  "Execute `my/org-emacs-lisp-source' and then indent block."
  (interactive)
  (tempo-template-my/org-emacs-lisp-source)
  (forward-line -1)
  (org-cycle))

(defun my/filter-buffer-substring (start end delete)
  "Custom filter on buffer text from START to END.
When DELETE is t, delte the contents in the range.
Otherwise, remove all properties from a span in a buffer.
Useful when copying code into Org blocks so that the copy does not contain any
artifacts such as indentation bars."
  (if delete
      (delete-and-extract-region start end)
    (buffer-substring-no-properties start end)))

(setq filter-buffer-substring-function #'my/filter-buffer-substring)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))

(use-package consult-notes
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  :bind (("C-c n b" . consult-notes)))

(defun my/emacs-key-bind (keymap &rest definitions)
  "Apply key binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
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

(use-package lisp-mode
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

(use-package cc-mode
  :init (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  :hook ((c++-mode . my/c++-mode-hook)))

(use-package sh-mode
  :hook ((sh-mode . my/sh-mode-hook)
         (sh-mode . indent-bars-mode)))

(use-package shell-mode
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.yagconf\\'" . json-mode))
  :hook ((json-mode . my/json-mode-hook)))

(use-package js2-mode
  :hook ((js2-mode . my/js2-mode-hook)))

(use-package winner
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)))

(use-package server
  :commands (server-running-p)
  :defines (server-name)
  :hook (after-init . (lambda () (unless (server-running-p)
                              ;; Make a unique server connection since I run multiple Emacs instances and I want the
                              ;; emacsclient in a comint buffer to connect to the right connection.
                              (setq server-name (format "server-%d" (emacs-pid)))
                              (setenv "EMACS_SERVER_FILE" server-name)
                              (server-start)))))

(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode))

(use-package ibuffer
  :config
  ;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
  (keymap-unset ibuffer-mode-map "M-o" t))

(use-package python
  :hook ((python-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

(use-package makefile-mode
  :hook ((makefile-mode . my/makefile-mode-hook)
         (makefile-mode . indent-bars-mode)))

;;; -- ADDED PACKAGES

(use-package projectile
  :commands (projectile-mode projectile-compile-project)
  :bind-keymap (("C-x p" . projectile-command-map)
                ("H-p" . projectile-command-map))
  :hook (after-init . projectile-mode))

(use-package marginalia
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(when (display-graphic-p)
  (use-package nerd-icons)

  (use-package nerd-icons-completion
    :after (marginalia)
    :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
    :hook ((after-init . nerd-icons-completion-mode)
           (marginalia-mode nerd-icons-completion-marginalia-setup)))

  (use-package mood-line
    :commands (mood-line-mode)
    :hook (after-init . mood-line-mode)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package diff-hl
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(use-package magit
  :after (projectile)
  :commands (magit-status-setup-buffer magit-status)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-x g" . magit-status)
         ("C-x v l" . magit-log-buffer-file)
         ("C-c f" . magit-file-dispatch))
  :config
  (setq magit-repository-directories projectile-known-projects))

(use-package rg
  :after (projectile)
  :commands (rg-enable-default-bindings)
  :bind (:map projectile-command-map
              ("s r" . rg-project))
  :hook (after-init . rg-enable-default-bindings))

(use-package denote
  :commands (denote-dired-mode-in-directories)
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("C-c n n" . denote))
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/"))
  :config
  (setq denote-file-types (cons
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
                           denote-file-types)))

(defun my/aw-make-frame ()
  "Make a new frame using layout settings for the current display.
The first frame always takes on `initial-frame-alist', and subsequent frames
use `default-frame-alist' by default. If there are already two frames active
then subsequent ones will be at `my/align-right-frame-alist' which aligns with
the right-edge of the screen, but may overlap with the middle frame."
  (let ((num-frames (length (visible-frame-list))))
    (if (< num-frames 2)
        (make-frame)
      (make-frame (my/align-right-frame-alist (my/screen-layout))))))

(advice-add 'aw-make-frame :override #'my/aw-make-frame)

;; (setq aw-dispatch-alist
;;       '((?k aw-delete-window "Delete Window")
;;         (?s aw-swap-window "Swap Windows")
;;         (?M aw-move-window "Move Window")
;;         (?c aw-copy-window "Copy Window")
;;         (?j aw-switch-buffer-in-window "Select Buffer")
;;         (?n aw-flip-window)
;;         (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
;;         (?e aw-execute-command-other-window "Execute Command Other Window")
;;         (?F aw-split-window-fair "Split Fair Window")
;;         (?v aw-split-window-vert "Split Vert Window")
;;         (?b aw-split-window-horz "Split Horz Window")
;;         (?o delete-other-windows "Delete Other Windows")
;;         (?T aw-transpose-frame "Transpose Frame")
;;         (?? aw-show-dispatch-help)))

(defun my/ace-window-always-dispatch ()
  "Invoke `ace-window' after setting `aw-dispatch-always' to T.
When `aw-dispatch-always' is nil, `ace-window' does not invoke
its dispatching mechanism if there are 2 or fewer windows. This
command guarantees that dispatching will always happen."
  (interactive)
  (let ((current-aw-dispatch-always aw-dispatch-always))
    (unwind-protect
        (let ((aw-dispatch-always t))
          (call-interactively #'ace-window))
      (setq aw-dispatch-always current-aw-dispatch-always))))

(use-package consult
  :after (projectile)
  :commands (consult--customize-put)    ; silence flymake warning
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-h i" . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)

         ;; C-x bindings in `ctl-x-map`
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x f" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
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
  (setq register-preview-function #'consult-register-format)
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
  (setq consult-project-function (lambda (_) (projectile-project-root))))

;; FYI: Embark's default action binding of "RET" fails if a mode binds to <return>.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package mode-line-bell)

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

(use-package consult-eglot
  :after (consult eglot))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-k" . crux-smart-kill-line)
         ("C-c C-i" . crux-indent-defun)
         ("C-^" . crux-top-join-line)))

(defun my/find-user-init-file ()
  "Edit the `user-init-file`."
  (interactive)
  (find-file user-init-file))

(defun my/find-user-custom-file ()
  "Edit the `custom-file` if it exists."
  (interactive)
  (if custom-file
      (find-file custom-file)
    (message "No custom file defined.")))

(defun my/find-shell-init-file()
  "Edit a shell init file."
  (interactive)
  (let* ((shell (file-name-nondirectory (getenv "SHELL")))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar #'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file (completing-read "Choose shell init file: " candidates))
      (find-file (car candidates)))))

(use-package orderless)

(use-package flymake
  :commands (flymake-show-buffer-diagnostics)
  :config (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-json)

(use-package cape
  :commands (cape-file))

(use-package corfu)

(unless (display-graphic-p)
  (use-package corfu-terminal
    :commands (corfu-terminal-mode)
    :hook (after-init . corfu-terminal-mode)))

(use-package markdown-mode
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package cmake-mode
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package which-key)

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package hl-line)

(use-package popper
  :commands (popper-kill-latest-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type)))

(use-package char-menu
  :defines (char-menu)
  :bind (("C-z" . char-menu))
  :config
  (setq char-menu
        '("—" "‘’" "“”" "…" "«»" "–"
          ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
          ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
          ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
          ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))))

(when my/is-macosx
  (use-package osx-dictionary
    :bind (("C-c l" . osx-dictionary-search-pointer))))

(use-package scratch
  :bind (("C-c s" . scratch)))

(use-package esup
  :custom
  (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist
               '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2))) ; I think this is from pyright

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook (compile-mode . fancy-compilation-mode))

(use-package iso-transl
  :bind-keymap ("H-8" . iso-transl-ctl-x-8-map)) ; Enter diacritics using "dead" keys after <H-8> or <C-X 8>

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
  :commands (my/crm-indicator)
  :config
  (setq read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil
        custom-file (locate-user-emacs-file "custom.el")
        frame-title-format (list my/workspace-name
                                 " "
                                 '(:eval (abbreviate-file-name default-directory))))
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (defun my/crm-indicator(args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (before-save . copyright-update)
         (after-init . abbrev-mode)
         (after-init . (lambda ()
                         (when (file-exists-p custom-file)
                           (load custom-file 'noerror))))))

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

(defun my/reset-frame-width ()
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

(defun my/bury-or-kill-current-buffer ()
  "Bury or kill the current buffer without asking. (WIP)
Kill buffers that match the pattern '*...*'.
Otherwise just bury them."
  (interactive)
  (if (and (string-match-p "\\*\\(?:help\\|grep\\|Completions\\|Compile-Log\\|Man .*\\|eldoc)\\|shell .*\\*"
                           (buffer-name (current-buffer)))
           (not (get-buffer-process (current-buffer))))
      (progn
        (message "Killed buffer")
        (kill-buffer (current-buffer)))
    (message "Buried buffer")
    (bury-buffer (current-buffer))))

(defun my/bury-current-buffer ()
  "Bury the current buffer without asking."
  (interactive)
  (bury-buffer (current-buffer)))

(defun my/kill-current-buffer ()
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

(defun my/consult-notes-other-frame ()
  "Find note to show in a new frame."
  (interactive)
  (select-frame (make-frame))
  (consult-notes))

(defun my/remove-all-text-properties ()
  "Remove all text properties from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(defun my/matching-paren ()
  "When point is on a paren-type character, jump to its twin."
  (interactive)
  (cond ((looking-at "[[({]")
	 (forward-sexp 1)
	 (forward-char -1))
	((looking-at "[]})]")
	 (forward-char 1)
	 (forward-sexp -1))
	(t
	 nil)))

(defun my/indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (when-let ((filename (if (equal major-mode 'dired-mode)
                           default-directory
                         (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(defun repl ()
  "Simple alias to start ielm."
  (interactive)
  (ielm))

(defun my/repl-other-window ()
  "Start a new repl in another window."
  (interactive)
  (let ((tmp (get-buffer-create "*ielm*")))
    (switch-to-buffer-other-window tmp)
    (ielm)))

(defun my/do-next-window (wins)
  "Jump to next window in WINS after the current one."
  (when-let ((current-window (get-buffer-window))
             (current-index (seq-position wins current-window #'eq))
             (next-index (and current-index (1+ current-index)))
             (final-index (if my/next-window-wrap-around
                              (% next-index (length wins))
                            (and (length> wins next-index) next-index))))
    (aw-switch-to-window (nth final-index wins))))

(defun my/ace-window-next ()
  "Jump to next window according to `ace-window'."
  (interactive)
  (my/do-next-window (aw-window-list)))

(defun my/ace-window-previous ()
  "Jump to previous window according to `ace-window'."
  (interactive)
  (my/do-next-window (reverse (aw-window-list))))

(defun my/ace-window-one-command ()
  "Run an action in a chosen window.
Taken from https://karthinks.com/software/emacs-window-management-almanac/#window-magic-with-ace-window-dispatch."
  (interactive)
  (when-let ((aw-dispatch-always t)
             (win (aw-select " ACE"))
             (windowp win))
    (with-selected-window win
      (let* ((command (key-binding
                       (read-key-sequence
                        (format "Run in %s..." (buffer-name)))))
             (this-command command))
        (call-interactively command)))))

(defun my/display-buffer-pre-func (buffer alist)
  "Method to use for `display-buffer-overriding-action'.
The BUFFER and ALIST are ignored."
  (let* ((_ (cons buffer alist))
         (type 'reuse)
         (aw-dispatch-always t)
         (window (aw-select (propertize " ACE" 'face 'mode-line-highlight))))
    (cons window type)))

(defun my/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command #'my/display-buffer-pre-func nil "[ace-window]")
  (message "Command to execute: "))

(keymap-global-set "C-x 4 o" #'my/ace-window-prefix)

(defun my/describe-symbol-at-point ()
  "Immediately show help for symbol at point if it exists."
  (interactive)
  (when-let ((v-or-f (symbol-at-point))
             (found (cl-some (lambda (x) (funcall (nth 1 x) v-or-f)) describe-symbol-backends)))
    (describe-symbol v-or-f (help-buffer))))

(defun crux-find-current-directory-dir-locals-file ()
  "Edit the current directory's `.dir-locals.el' file in another window."
  (interactive)
  (find-file-other-window
   (expand-file-name ".dir-locals.el")))

;;; --- Key Bindings

(my/emacs-key-bind global-map
                   "S-<left>" #'my/ace-window-previous
                   "S-<right>" #'my/ace-window-next

                   "C-c r" #'ielm
                   "C-c D" #'crux-find-current-directory-dir-locals-file

                   "C-c i" #'my/find-user-init-file
                   "C-c ," #'my/find-user-custom-file
                   "C-c S" #'my/find-shell-init-file

                   ;; NOTE: do not bind RET or <return> -- that breaks Embark maps
                   "C-h C-h" #'my/describe-symbol-at-point
                   "C-h C-j" #'popper-toggle
                   "C-h a" #'describe-symbol
                   "C-h c" #'describe-char
                   "C-h u" #'apropos-user-option
                   "C-h F" #'apropos-function
                   "C-h K" #'describe-keymap
                   "C-h L" #'apropos-library
                   "C-h M" #'consult-man
                   "C-h V" #'apropos-variable

                   "M-g d" #'dired-jump
                   "M-o" #'other-window
                   "M-O" #'my/ace-window-always-dispatch

                   "C-o" #'aw-flip-window

                   "C-s" #'isearch-forward
                   "C-M-s" #'isearch-forward-symbol

                   "C-x 0" #'delete-other-windows
                   "C-x O" #'other-frame
                   "C-x C-o" #'other-frame

                   "C-x C-b" #'ibuffer
                   "C-x M-b" #'consult-project-buffer

                   "M-<f1>" #'my/reset-frame-left
                   "M-<f2>" #'my/reset-frame-right
                   "M-<f3>" #'my/reset-frame-right-display

                   "C-x 4 c" #'my/customize-other-window
                   "C-x 4 k" #'my/shell-other-window
                   "C-x 4 r" #'my/repl-other-window

                   "C-x 5 c" #'my/customize-other-frame
                   "C-x 5 i" #'my/info-other-frame
                   "C-x 5 k" #'my/shell-other-frame

                   "C-c C-c" #'my/copy-file-name-to-clipboard
                   "C-c C-k" #'my/kill-current-buffer

                   "<f3>" #'eval-last-sexp

                   "C-M-\\" #'my/indent-buffer

                   "<home>" #'beginning-of-buffer
                   "<end>" #'end-of-buffer
                   "<delete>" #'delete-char
                   "S-<f12>" #'package-list-packages

                   "M-z" #'zap-up-to-char
                   "M-[" #'previous-buffer ; NOTE: this conflicts with terminal escape sequences (see below)
                   "M-]" #'next-buffer
                   "M-_" #'join-line

                   "C-S-p" #'my/ace-window-previous
                   "C-S-n" #'my/ace-window-next

                   ;; --- Hyper-key Bindings
                   "H-SPC" #'my/ace-window-always-dispatch
                   "H-1" #'delete-other-windows
                   "H-2" #'split-window-below
                   "H-4" ctl-x-4-map
                   "H-5" ctl-x-5-map
                   "H-a" #'ace-window
                   "H-b" #'consult-project-buffer
                   "H-B" #'consult-buffer
                   "H-c" #'projectile-compile-project
                   "H-g" #'magit-status
                   "H-h" #'my/describe-symbol-at-point
                   "H-k" #'bury-buffer
                   "H-m" #'consult-bookmark
                   "H-p" #'projectile-command-map
                   "H-u" #'undo
                   "H-w" #'my/ace-window-prefix
                   "H-;" #'my/matching-paren

                   "<insert>" #'ignore  ; disable key for toggling overwrite mode
                   "C-x C-z" #'ignore   ; suspend-frame

                   "C-x C-+" #'ignore   ; text-scale-adjust
                   "C-x C-=" #'ignore   ; text-scale-adjust
                   "C-x C--" #'ignore   ; text-scale-adjust

                   "C-x h" #'ignore      ; mark-whole-buffer
                   "C-h h" #'ignore      ; show 'Hello' in various fonts

                   ;; Disable font size changes via trackpad
                   "C-<mouse-4>" #'ignore
                   "C-<mouse-5>" #'ignore
                   "C-<wheel-up>" #'ignore
                   "C-<wheel-down>" #'ignore

                   "C-M-<mouse-4>" #'ignore
                   "C-M-<mouse-5>" #'ignore
                   "C-M-<wheel-up>" #'ignore
                   "C-M-<wheel-down>" #'ignore)

;;; --- Key Chords

;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy to type with
;; one or two hands.
(my/emacs-chord-bind global-map
                     "qq" #'undo
                     "aa" #'my/ace-window-always-dispatch

                     "JJ" #'my/ace-window-previous
                     "KK" #'my/ace-window-next
                     "kk" #'my/kill-current-buffer
                     "hh" #'my/describe-symbol-at-point
                     "hb" #'popper-kill-latest-popup
                     "sb" #'speedbar
                     "vv" #'diff-hl-show-hunk
                     "fm" #'flymake-show-buffer-diagnostics)

(use-package s
  :commands (s-ends-with-p s-repeat))

(defun my/find-known-bindings (key)
  "Find all key bindings for KEY.
Reads in KEY if not provided. Format is what would be
seen in `describe-key' output (e.g. `C-c' or `C-M-S-u')."
  (interactive "sList known bindings for key sequence: ")
  (let ((parsed (key-parse key)))
    (with-current-buffer (get-buffer-create "*known bindings*")
      (erase-buffer)
      (mapatoms (lambda (sym)
                  (when (or (eq sym 'global-map)
                            (and (boundp sym)
                                 (symbol-value sym)
                                 (s-ends-with-p "-mode-map" (symbol-name sym))
                                 (keymapp (symbol-value sym))))
                    (let ((binding (lookup-key (symbol-value sym) parsed t)))
                      (when (and binding
                                 (not (numberp binding)))
                        (insert (format "%-40s %s\n" sym (if (keymapp binding) "KEYMAP" binding))))))))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (insert
       (format "Known bindings for key: %s\n\n" (key-description key))
       (format "%-40s %s" "Map" "Binding\n")
       (s-repeat 40 "-") " " (s-repeat 30 "-") "\n")
      (display-buffer (current-buffer)))))

(defvar my/info-keys-map (make-sparse-keymap)
  "Keymap for canned info manual searches.")

(defun my/consult-info-emacs ()
  "Search Emacs info."
  (interactive)
  (consult-info "emacs" "cape" "corfu" "denote" "embark" "magit" "marginalia" "orderless" "vertico"))

(keymap-set my/info-keys-map "c" #'consult-info)
(keymap-set my/info-keys-map "e" #'my/consult-info-emacs)
(keymap-set my/info-keys-map "i" #'info)
(keymap-set help-map "i" my/info-keys-map)

(defun my/set-bound-var (symbol value)
  "Set SYMBOL with VALUE if SYMBOL exists."
  (when (boundp symbol)
    (set symbol value)))

(if my/is-terminal
    (when my/is-linux
      ;; Undo the mapping for ESC [ so it does not take over defined xterm sequences
      (define-key (current-global-map) (kbd "M-[") nil)
      (defvar arrow-keys-map (make-sparse-keymap) "Keymap for arrow keys")
      (define-key esc-map "O" arrow-keys-map)
      (define-key arrow-keys-map "A" #'previous-line)
      (define-key arrow-keys-map "B" #'next-line)
      (define-key arrow-keys-map "C" #'forward-char)
      (define-key arrow-keys-map "D" #'backward-char))
  (when my/is-macosx
    (custom-set-variables
     '(insert-directory-program "gls")
     '(frame-resize-pixelwise t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-control-modifier 'hyper))))

(provide 'init)

;;; init.el ends here.

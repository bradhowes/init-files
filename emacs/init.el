;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(require 'seq)

(push (file-truename "~/.emacs.d/lisp") load-path)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defconst my/repos (file-name-as-directory (file-truename "~/src/Mine"))
  "Location of root of my source repositories.")

(defconst my/venv (file-truename "~/venv")
  "The Python virtual environment to use for elpy.")

(setenv "WORKON_HOME" my/venv)

(defconst my/venv-python (concat my/venv "/bin/python")
  "The path to the Python executable to use for elpy.")

(defconst my/is-work (or (string= "howesbra" user-login-name)
                         (string= "sp_qa" user-login-name))
  "This is t if running at work.")

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

(defconst my/is-dev (and my/is-work (string-suffix-p "d" (system-name)))
  "T if running on dev box at work.")

(defconst my/is-qa (and my/is-work (string-suffix-p "q" (system-name)))
  "T if running on QA box at work.")

(defconst my/dev-tmp "/apps/home/howesbra/tmp"
  "The directory to use as temporary storage.")

(defun my/valid-directory (dir)
  "Check if DIR is valid, returning it if so or nil if not."
  (and (file-directory-p dir) dir))

(defconst my/tmp-dir
  (let* ((work-tmp (my/valid-directory my/dev-tmp))
         (home-tmp (file-truename "~/tmp"))
         (tmp (or work-tmp home-tmp)))
    (unless (file-directory-p tmp)
      (make-directory tmp t))
    (file-name-as-directory tmp))
  "The directory to use for temporary purposes - usually $HOME/tmp.")

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
  (declare (side-effect-free t))
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
  (declare (side-effect-free t))
  (eq layout my/screen-laptop))

(defun my/is-4k (layout)
  "T if LAYOUT is kind with at least 4K area."
  (declare (side-effect-free t))
  (memq layout '(my/screen-4k my/screen-laptop-4k my/screen-4k-4k my/screen-laptop-4k-4k)))

(defun my/rows (layout)
  "The number of rows to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (my/is-4k layout) 103 (if (my/is-laptop layout) 88 40)))

(defun my/cols (layout)
  "The number of columns to show in a frame shown on LAYOUT."
  (declare (side-effect-free t))
  (if (or (my/is-4k layout) (my/is-laptop layout)) 132 80))

(defun my/frame-pixel-width (layout)
  "Width in pixels of a normal frame shown on LAYOUT.
These values are hard-coded based on current settings.
Probably a better way to figure this out."
  (declare (side-effect-free t))
  (if (my/is-4k layout) (if my/is-macosx 1338 1338) 944))

(defun my/frame-initial-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be used for the `initial-frame-alist' configuration."
  (declare (side-effect-free t))
  ;; Use the first external monitor if there is one.
  (if (memq layout '(my/screen-laptop-4k my/screen-laptop-4k-4k))
      (+ 2056 (* my/screen-4k-pick my/4k-screen-width))
    (if my/is-x-windows-on-win 0 0)))

(defun my/frame-default-left (layout)
  "Pixels to use for the `left' of a frame on LAYOUT.
This is to be use for the `default-frame-aliat' configuration."
  (declare (side-effect-free t))
  (+ (my/frame-initial-left layout) (my/frame-pixel-width layout)))

(defun my/frame-third-left (layout)
  "The offset to the `alt' window based on LAYOUT.
This is not used in any particular `*-frame-alist' but it is used
by custom commands that reposition a frame to be flush with the
right-side of the active screen that is being used to host
Emacs frames."
  (declare (side-effect-free t))
  (- (+ (my/frame-initial-left layout)
        (if (eq layout my/screen-laptop)
            my/laptop-screen-width
          my/4k-screen-width))
     (my/frame-pixel-width layout)))

(defun my/frame-top ()
  "The top of the display area.
NOTE: this assumes that the laptop display if present is,
is on the left of any monitors."
  (declare (side-effect-free t))
  (let ((settings (display-monitor-attributes-list)))
    (+ (nth 2 (if (eq (length settings) 1)
                  (car (car settings))
                (car (car (cdr settings)))))
       ;; Apply an offset to show the title bar when running under VcXsrv on Windows
       (if my/is-x-windows-on-win 30 0))))

(defun my/initial-frame-alist (layout)
  "Make alist to use for the initial frame on LAYOUT.
Frame's left side is flush with the left side of the main display."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-initial-left layout))))

(defun my/default-frame-alist (layout)
  "Make alist to use for the default frame on LAYOUT.
Frame's left side is next to the right side of the initial frame."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-default-left layout))))

(defun my/align-right-frame-alist (layout)
  "The alist to use extra frame on LAYOUT.
Frame's right side is flush with the right side of the main display."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (my/frame-third-left layout))))

(defun my/alt-default-frame-alist (layout)
  "Make alt alist to use for the default frame on LAYOUT.
Frame's right side is next to the left side of the `alt' frame."
  (declare (side-effect-free t))
  (list (cons 'width (my/cols layout))
        (cons 'height (my/rows layout))
        (cons 'top (my/frame-top))
        (cons 'left (- (my/frame-third-left layout) (my/frame-pixel-width layout) 16))))

(defun my/update-screen-frame-alists (layout)
  "Update frame alists for current LAYOUT."
  (setq initial-frame-alist (my/initial-frame-alist layout)
        default-frame-alist (my/default-frame-alist layout))
  (message "initial-frame: %s" initial-frame-alist)
  (message "default-frame: %s" default-frame-alist))

(defun my/font-size (layout)
  "The font size to use based on the LAYOUT."
  (declare (side-effect-free t))
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

(add-hook 'after-init-hook #'my/screen-layout-changed)

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

;; Configure `display-buffer-alist` to manage window placement

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
          window-sides-slots '(0 0 3 1)
          display-buffer-base-action '((display-buffer-reuse-window ace-display-buffer))
          display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window ace-display-buffer) (reusable-frames . nil))
                                 ("\\*R" nil (reusable-frames . nil))
                                 ,(cons "\\*helm" display-buffer-fallback-action)))))
                                 ;; Show log buffer in something other than the current window
                                 ;; ("magit-log" nil (inhibit-same-window . t))
                                 ;; ("magit-diff:" nil (inhibit-same-window . t))))))

(when my/is-terminal
  (set-face-background 'default "undefined"))

(use-package my-fontify-braces)

(defun my/autoloads (&rest definitions)
  "Setup autoloads for my code customizations.
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
 "my-find-known-bindings" 'my/find-known-bindings
 "my-cmake-mode" 'my/cmake-mode-hook
 "my-c++-mode" 'my/c++-mode-hook
 "my-dired-mode" 'my/dired-mode-hook
 "my-js2-mode" 'my/jsd-mode-hook
 "my-json-mode" 'my/json-mode-hook
 "my-lisp-mode" '(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" 'my/makefile-mode-hook
 "my-markdown-mode" 'my/markdown-mode-hook
 "my-python-mode" '(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" 'my/sh-mode-hook
 "my-shell-mode" 'my/shell-mode-hook)

(use-package key-chord
  :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define))

(use-package accent
  :bind ("C-c a" . accent-menu))

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

(defun my/org-filter-buffer-substring (start end delete)
  "Custom filter on buffer text from START to END.
When DELETE is t, delte the contents in the range.
Otherwise, remove all properties from a span in a buffer.
Useful when copying code into Org blocks so that the copy does not contain any
artifacts such as indentation bars."
  (if delete
      (delete-and-extract-region start end)
    (buffer-substring-no-properties start end)))

;; NOTE: this is setting a global variable, but we should really just do this when operating in an org buffer.
(setq filter-buffer-substring-function #'my/org-filter-buffer-substring)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(unless my/is-terminal
  (use-package indent-bars
    :hook (prog-mode . indent-bars-mode)))

(use-package consult-notes
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  :bind (("H-n b" . consult-notes)))

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

(if (executable-find "shellcheck")
    (use-package flymake-shellcheck
      :hook
      (sh-mode . flymake-shellcheck-load))
  (message "Not using flymake-shellcheck -- shellcheck executable not found."))

(use-package shell-mode
  :defines (explicit-bash-args)
  :init
  ;; Special-case QA env -- we are logged in as `sp_qa` user but we want our custom
  ;; environment. Command `bash` to load our custom settings.
  (let ((rc (file-truename (file-name-concat my/repos "configurations/qa.bashrc"))))
    (if (and my/is-qa
             (file-exists-p rc)
             (string-suffix-p "q" (system-name)))
        (setq explicit-bash-args (list "--no-editing" "--rcfile" rc "-i"))
      (setq explicit-bash-args '("--noediting" "-i"))))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.yagconf\\'" . json-mode))
  :hook ((json-mode . my/json-mode-hook)))

(use-package js2-mode
  :hook ((js2-mode . my/js2-mode-hook)))

(use-package winner
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         ("C-c u" . winner-undo)
         ("C-c C-u" . winner-undo)
         ("C-c C-r" . winner-redo)))

(require 'server)

(defun my/start-emacs-server ()
  "Start up an Emacs server to support `emacsclient' connections.
Customize `server-name' so that each Emacs
process has its own server connection."
  (interactive)
  ;; NOTE: `server-running-p` can report `t` even if we are not running it.
  (unless server-process
    ;; Make a unique server connection since I run multiple Emacs instances and I want the emacsclient in a comint
    ;; buffer to connect to the right connection.
    (setq server-name (format "server-%d" (emacs-pid)))
    (setenv "EMACS_SERVER_FILE" server-name)
    (setenv "EMACS_SOCKET_NAME" server-name)
    (server-start)))

(add-hook 'after-init-hook #'my/start-emacs-server)

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (sh-mode . ws-butler-mode)
         (text-mode . flyspell-mode)))

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

(use-package hl-line)

(use-package project
  :commands (project--switch-project-command)
  :bind (("C-x p $" . project-shell)))

(defun my/show-project-menu ()
  "Show the menu that is shown when switching to a new project."
  (interactive)
  (call-interactively (project--switch-project-command)))

(keymap-set project-prefix-map "m" #'my/show-project-menu)

(defvar my/project-search-map (make-sparse-keymap)
  "A prefix map like that found in projectile.
Bound to \\`C-x p s'.")
(keymap-set project-prefix-map "s" my/project-search-map)

(use-package marginalia
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook
  (after-init . marginalia-mode))

(when (display-graphic-p)
  (use-package nerd-icons)

  (use-package nerd-icons-completion
    :after (marginalia)
    :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
    :hook ((after-init . nerd-icons-completion-mode)
           (marginalia-mode nerd-icons-completion-marginalia-setup)))

  (use-package mood-line
    :commands (mood-line-mode)
    :hook
    (after-init . mood-line-mode)))

(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)))

(use-package dired
  :config
  :bind (:map dired-mode-map
              ("M-{" . nil)
              ("M-}" . nil)
              ("c" . dired-do-copy)
              ("C" . dired-do-compress)
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

(use-package diff-hl
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(use-package password-cache
  :defines (password-cache password-cache-expiry)
  :commands (password-cache-add password-read password-read-from-cache)
  :custom
  (password-cache t)
  (password-cache-expiry nil))

(defun my/read-gitlab-password (host)
  "Inject password into `password-cache` for HOST."
  (if-let ((password (password-read-from-cache host)))
      password
    (let ((password (password-read "Gitlab password: " host)))
      (password-cache-add host password)
      password)))

;; Work QA hosts have a Git v2.18.0 and latest Magit now yells about it. In QA, fetch the last Magit version that does not care
;; about this. Unfortunately, I do not see a way to do this conditionally inside `use-package`.
(when (and my/is-work my/is-qa)
  (unless (package-installed-p 'magit)
    (package-vc-install '(magit
                          :url "https://github.com/magit/magit.git"
                          :branch "v4.1.3"
                          :lisp-dir "lisp"
                          :make '("lisp" "info")
                          :doc "docs/magit.texi")))
  ;; For some reason this is still needed even after using `package-vc-install'
  (push (concat user-emacs-directory "elpa/magit/lisp") load-path))

(use-package magit
  :ensure t
  :commands (magit-status-setup-buffer magit-status magit-project-status)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-x g" . magit-status)
         ;; Take over vc-dir
         ("C-x p v" . magit-project-status)
         ;; Take over vc-print-log
         ("C-x v l" . magit-log-buffer-file)
         ("C-c f" . magit-file-dispatch))
  :custom
  (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package rg
  :after (project)
  :commands (rg-enable-default-bindings rg-project)
  :bind ("C-x p s r" . #'rg-project)
  :hook (after-init . rg-enable-default-bindings))

(use-package denote
  :commands (denote-dired-mode-in-directories)
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("H-n n" . denote))
  :custom
  (denote-directory (file-truename "~/Documents/notes/"))
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
  :after (project)
  :commands (consult--customize-put consult-flymake)
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-h i" . consult-info)
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

         :map project-prefix-map
         ("b" . consult-project-buffer))

  :commands (consult-register-format consult-register-window consult-xref)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (not lazy).
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store', and the built-in commands. This improves the
  ;; register formatting, adds thin separate lines, register sorting and hides
  ;; the window mode line.
  ;; (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (defun my/consult-line-symbol-at-point ()
    "Start `consult-line' with symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep consult-man
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-bookmark consult--source-file-register
                     consult--source-recent-file consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  )

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

(unless my/is-terminal
  (use-package eldoc-box))
;; :hook (prog-mode . eldoc-box-hover-mode)))

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
  :defer nil
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-k" . crux-smart-kill-line)
         ("C-c C-i" . crux-indent-defun)
         ("C-^" . crux-top-join-line)))

;; My own version of some `crux` routines that use `find-file` instead of `find-file-other-window`
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
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)
         (sh-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-json)

(use-package cape
  :commands (cape-file))

(use-package corfu
  :bind (:map corfu-map ("C-SPC" . corfu-insert-separator)))

(when my/is-terminal
  (use-package corfu-terminal
    :commands (corfu-terminal-mode)
    :hook (after-init . (lambda () (corfu-terminal-mode +1)))))

(use-package markdown-mode
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package cmake-mode
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package which-key)

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package popper
  :defer nil                            ; load now due to dependencies below
  :commands (popper-kill-latest-popup)
  :functions (popper--delete-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)))

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

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

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

(use-package crm)

(use-package emacs
  :commands (my/crm-indicator)
  :config
  (setq read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil
        custom-file (file-truename (locate-user-emacs-file "custom.el"))
        frame-title-format (let ((buffer-directory '(:eval (abbreviate-file-name default-directory))))
                             (if my/is-terminal (list (concat (system-name) " ") buffer-directory)
                               buffer-directory)))
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (defun my/crm-indicator(args)
    "Custom CRM indicator for ARGS."
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

(require 'emacs-pager)

(defun my/dump-hashtable (hashtable)
  "Show the contents of HASHTABLE."
  (interactive "Xhash table: ")
  (when (hash-table-p hashtable)
    (let ((tmp (get-buffer-create "*dump*")))
      (switch-to-buffer-other-window tmp)
      (erase-buffer)
      (maphash (lambda (key value)
                 (insert key " -> " value "\n")) hashtable)
      (emacs-pager-mode))))

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

(defun my/reset-frame-alt-right ()
  "Reset frame size and position for alternative right position.
This will shift the frame to the left so that it does not overlap with any
frame that is abutting the right edge of the display."
  (interactive)
  (let ((layout (my/screen-layout)))
    (modify-frame-parameters (window-frame (get-buffer-window)) (my/alt-default-frame-alist layout))))
(keymap-global-set "C-M-<f2>" #'my/reset-frame-alt-right)

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

(defun my/reload-buffer ()
  "Reload the current buffer from disk.
Checks to see if buffer needs saving first, aborting if changes not saved."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and (not buffer-read-only)
               filename
               (or (not (buffer-modified-p))
                   (and (string= "yes" (read-answer "Save changes? "
                                                    '(("yes" ?y "save buffer")
                                                      ("quit" ?q "abort"))))
                        (progn (save-buffer) t))))
      (find-alternate-file filename)
      (message "Reloaded."))))

(defun my/run-something-in-buffer (name buffer-setup-proc run-proc)
  "Run RUN-PROC after running BUFFER-SETUP-PROC in NAME buffer.
This function receives the buffer to use for the shell. The expectation
is that the function will setup the display environment to host the
buffer."
  (let ((cwd default-directory)
        (tmp (get-buffer-create name)))
    (funcall buffer-setup-proc tmp)
    (cd-absolute cwd)
    (funcall run-proc tmp)))

(defun my/in-current-window (buf)
  "Switch to buffer BUF in current window."
  (switch-to-buffer buf nil t))

(defun my/in-other-window (buf)
  "Switch to buffer BUF in other window."
  (switch-to-buffer-other-window buf))

(defun my/in-other-frame (buf)
  "Switch to buffer BUF in new frame."
  (select-frame (make-frame))
  (switch-to-buffer buf))

(defun my/run-shell (buffer-setup-proc)
  "Run a new `shell' after running BUFFER-SETUP-PROC.
This function receives the buffer to use for the shell. The expectation
is that the function will setup the display environment to host the
buffer."
  (my/run-something-in-buffer " *Shell*"
                              buffer-setup-proc
                              (lambda (buf) (shell buf))))

(defun my/shell ()
  "Start a new shell."
  (interactive)
  (my/run-shell #'my/in-current-window))

(defalias 'ksh 'my/shell
  "Legacy alias to start shell in current window.")

(defun my/shell-other-window ()
  "Start a new shell in another window."
  (interactive)
  (my/run-shell #'my/in-other-window))

(defun my/shell-other-frame ()
  "Start a new shell in another frame."
  (interactive)
  (my/run-shell #'my/in-other-frame))

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

(defun my/next-buffer-skip-filter (_ buffer bury-or-kill)
  "Return t if BUFFER should be skipped in WINDOW.
This is used by `my/prev-buffer-current-window' and
`my/next-buffer-current-window' methods so that only desired
buffers will be available for changing to in the current
window. If BURY-OR-KILL is not nil then the operation will result
in the buffer being buried or killed, and in this case buffers
are never filtered out. Otherwise, skip buffers that start with
'*' in their name, `dired' buffers, `help' buffers, and buffers
that are already visible somewhere."
  (if bury-or-kill
      nil
    ;; Taken from http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html
    (with-current-buffer buffer
      (cond
       ((string-match "^\*" (buffer-name)) t)
       ((eq major-mode 'dired-mode) t)
       ((eq major-mode 'help-mode) t)
       ((get-buffer-window nil 'visible) t)
       (t nil)))))

(defun my/next-buffer-current-window ()
  "Switch to `next' buffer in current window with filtering.
Only switch to a buffer that passes the filter define in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (next-buffer)))

(defun my/prev-buffer-current-window ()
  "Switch to `previous' buffer in current window with filtering.
Only switch to a buffer that passes the filter define in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (previous-buffer)))

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
  "Immediately show help for symbol at point if it exists.
If help buffer is visible and it is showing help for the
symbol, then hide it."
  (interactive)
  (let ((what (symbol-name (symbol-at-point)))
        (help-window (get-buffer-window (help-buffer))))
    (if (and help-window
             (save-current-buffer
               (set-buffer (help-buffer))
               (goto-char (point-min))
               (looking-at what)))
        (popper--delete-popup help-window)
      (describe-symbol (symbol-at-point) (help-buffer)))))

(defun my/htop ()
  "Run htop in a term buffer."
  (interactive)
  (let* ((name "*htop*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "sudo htop" name))))

(defun my/top ()
  "Run top in a term buffer."
  (interactive)
  (let* ((name "*top*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "top" name))))

(defun crux-find-current-directory-dir-locals-file (find-2)
  "Edit the `.dir-locals.el' file for the current buffer in another window.
If prefix arg FIND-2 is set then edit the `.dir-locals-2.el' file instead
of `.dir-locals.el'. Scans parent directories if the file does not exist in
the default directory of the current buffer. If not found, create a new,
empty buffer in the current buffer's default directory, or if there is no
such directory, in the user's home directory."
  (interactive "P")
  (let* ((prefix (if (eq system-type 'ms-dos) "_" "."))
         (file (concat prefix (if find-2 "dir-locals-2" "dir-locals") ".el"))
         (starting-dir (or (when (and default-directory
                                      (file-readable-p default-directory))
                             default-directory)
                           (file-truename "~/")))
         (found-dir (or (locate-dominating-file starting-dir file) starting-dir))
         (found-file (concat found-dir file)))
    (find-file-other-window found-file)
    (if (file-exists-p found-file)
        (message "Editing existing file %s" found-file)
      (message "Editing new file %s" found-file))))

(defun my/dired-raze ()
  "Open Dired on Raze repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "raze")))

(defun my/dired-x23 ()
  "Open Dired on x23 repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "x23")))

(defun my/dired-tcs ()
  "Open Dired on tcs repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "tcs_hft")))

(defun my/dired-wolverine-config ()
  "Open Dired on wolveriine-config repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "wolverine-config")))

(defun my/dired-wolverine-config-qa ()
  "Open Dired on wolveriine-config-qa repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "wolverine-config-qa")))

(defun my/dired-yabgrat ()
  "Open Dired on yagbrat repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "yagbrat")))

(defun my/dired-configurations ()
  "Open Dired on configurations repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "configurations")))

(defcustom my/git-sync-buffer-name " *my/git-sync"
  "The name of the buffer to use to hold output from my/git-sync func."
  :type '(string)
  :group 'my/customizations
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (set-default-toplevel-value 'popper-reference-buffers (append (list value) popper-reference-buffers))))

(defun my/git-sync (host path)
  "Execute a git pull on HOST in PATH."
  (interactive)
  (message "Running git-pull on %s:%s..." host path)
  (let* ((git (list "cd" path "&&" "git" "pull"))
         (cmd (if host
                  (append (list "/usr/bin/ssh" "-tt" host) git)
                (append '("bash") git)))
         (name (concat "<" (or host "localhost") "|" path ">"))
         (args (append (list name my/git-sync-buffer-name) cmd))
         (proc (apply 'start-process args)))
    (add-function :around (process-filter proc)
                  (lambda (filt proc content)
                    (funcall filt proc (concat (process-name proc) ": " content))))
    proc))

(defun my/all-git-sync ()
  "Sync the configurations repo found in various locations at work."
  (interactive)
  (when-let ((buf (get-buffer my/git-sync-buffer-name)))
    (kill-buffer buf))
  (let ((local (file-name-concat my/repos "configurations"))
        (home (file-truename "~/configurations")))
    (dolist (cfg (list (cons nil local)
                       (cons nil home)
                       (cons "nyzls1644q" local)
                       (cons "nyzls1646q" local)
                       (cons "nyzls2686q" local)
                       (cons "ldzls2164i" local)
                       (cons "nyzls1514n" local)
                       (cons "nyzls105i" local)
                       (cons "ldzls2164i" home)))
      (my/git-sync (car cfg) (cdr cfg))))
  (display-buffer my/git-sync-buffer-name))

(defun my/sort-lines-by-integer-key (pattern &optional direction)
  "Sort lines by an integer value that is found via PATTERN in a line.
The sort is in increasing numerical order if DIRECTION is nil; otherwise
it is in descending order."
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr direction
                   #'forward-line       ; NEXTRECFUN
                   #'end-of-line        ; ENDRECFUN
                   (lambda ()                ; STARTKEYFUN -- returns numeric key
                     (when (looking-at pattern)
                       (string-to-number (match-string 0))))
                   nil                  ; ENDKEYFUN
                   nil)))))             ; PREDICATE

(defun my/sort-lines-by-leading-integer (arg)
  "Sort lines in current region by extracting integer values from start of line.
If ARG is not nil, sort in descending order.
Nothing fancy about parsing, it just matches any number at the beginning of
the line, ignoring any whitespace characters. If that fails, then the sort
will treat the whole line as a value to compare against."
  (interactive "P")
  (my/sort-lines-by-integer-key "^\\s *[0-9]+" arg))

(defun my/launch-qa-emacs (host)
  "Launch X11 Emacs on QA HOST."
  (interactive)
  (message "Starting QA Emacs on %s..." host)
  (start-process
   "qa-raze"
   " *qa-raze*"
   "/usr/bin/ssh"
   "-Y"
   (concat "sp_qa@" host)
   ". /apps/home/howesbra/repos/configurations/qa.profile; exec /opt/third/emacs/30.1.1/emacs"))

(defun my/qa-emacs-raze ()
  "Launch X11 Emacs on nyzls1646q."
  (interactive)
  (my/launch-qa-emacs "nyzls1646q"))

(defun my/qa-emacs-tcs ()
  "Launch X11 Emacs on nyzls2686q."
  (interactive)
  (my/launch-qa-emacs "nyzls2686q"))

(defun my/dired-qa-og ()
  "Open Dired on og log directory in qa."
  (interactive)
  (dired (file-truename "/apps/sp/logs/sp_qa/og")))

(defun my/dired-qa-tcs ()
  "Open Dired on tcs_hft log directory in qa."
  (interactive)
  (dired (file-truename "/apps/sp/logs/sp_qa/tcs_hft")))

(defun my/set-mark-deactivate ()
  "Set mark without activating it.
This is just a shortcut for \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (set-mark-command nil)
  (when transient-mark-mode
    (deactivate-mark)))

(defun my/goto-mark ()
  "Move back to mark without enabling transient mode.
THis is just a shortcut for \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (set-mark-command 4))

(defun my/display-prefix (arg)
  "Display the value of the raw prefix ARG."
  (interactive "P")
  (message "%s" arg))

(defun my/mark-line (&optional arg)
  "Blah blah ARG blah."
  (interactive "p")
  (unless mark-active
    (beginning-of-line)
    (push-mark)
    (setq mark-active t))
  (forward-line arg))

;; "Jump" to a directory -- "C-c j r" => dired buffer in Raze repo
(defvar my/dired-jumps-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'my/dired-qa-og)
    (define-key map "c" #'my/dired-configurations)
    (define-key map "r" #'my/dired-raze)
    (define-key map "t" (if my/is-qa #'my/dired-qa-tcs #'my/dired-tcs))
    (define-key map "w" #'my/dired-wolverine-config)
    (define-key map "x" #'my/dired-x23)
    map)
  "Keymap for quick Dired jumps.")

(keymap-set mode-specific-map "j" my/dired-jumps-map)

;;; --- Key Bindings

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

(defun my/share-screen-font-size (&optional arg)
  "Set font scaling to ARG when sharing screen.
ARG is an optional integer which defaults to 2."
  (interactive "P")
  (text-scale-set (or arg 2)))

(defun my/normal-screen-font-size ()
  "Remove any font scaling."
  (interactive)
  (text-scale-set 0))

(my/emacs-key-bind global-map
                   "S-<left>" #'my/ace-window-previous
                   "S-<right>" #'my/ace-window-next
                   "S-<up>" #'my/ace-window-previous
                   "S-<down>" #'my/ace-window-next

                   "C-c r" #'ielm
                   "C-c D" #'crux-find-current-directory-dir-locals-file

                   "C-c i" #'my/find-user-init-file
                   "C-c ," #'my/find-user-custom-file
                   "C-c S" #'my/find-shell-init-file

                   ;; NOTE: do not bind RET or <return> -- that breaks Embark maps
                   "C-h C-h" #'my/describe-symbol-at-point
                   "C-h C-j" #'popper-kill-latest-popup
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

                   ;; "C-s" #'isearch-forward
                   ;; "C-M-s" #'isearch-forward-symbol

                   "C-x 0" #'delete-other-windows
                   "C-x O" #'other-frame
                   "C-x C-o" #'other-frame

                   "C-x C-b" #'ibuffer
                   "C-x M-b" #'consult-project-buffer
                   "C-x M-v" #'my/reload-buffer

                   "M-{" #'my/prev-buffer-current-window
                   "M-}" #'my/next-buffer-current-window

                   "M-<f1>" #'my/reset-frame-left
                   "M-<f2>" #'my/reset-frame-right
                   "M-<f3>" #'my/reset-frame-right-display

                   "C-x 4 c" #'my/customize-other-window
                   "C-x 4 k" #'my/shell-other-window
                   "C-x 4 r" #'my/repl-other-window

                   "C-x 5 i" #'my/info-other-frame
                   "C-x 5 k" #'my/shell-other-frame

                   "C-c C-c" #'my/copy-file-name-to-clipboard
                   "C-c C-k" #'my/kill-current-buffer

                   "C-M-\\" #'my/indent-buffer

                   "<home>" #'beginning-of-buffer
                   "<end>" #'end-of-buffer
                   "<delete>" #'delete-char
                   "S-<f12>" #'package-list-packages


                   "M-z" #'zap-up-to-char
                   "M-[" #'previous-buffer ; NOTE: this conflicts with terminal escape sequences (see below)
                   "M-]" #'next-buffer
                   "M-_" #'join-line

                   "M-P" #'my/ace-window-previous
                   "M-N" #'my/ace-window-next

                   "C-S-p" #'my/ace-window-previous
                   "C-S-n" #'my/ace-window-next

                   ;; --- Hyper-key Bindings

                   "H-SPC" #'my/set-mark-deactivate
                   "H-j" #'my/goto-mark
                   
                   "H-1" #'delete-other-windows
                   "H-2" #'split-window-below
                   "H-4" #'other-window-prefix ; was ctl-x-4-map
                   "H-5" #'other-frame-prefix  ; was ctl-x-5-map
                   "H-a" #'ace-window
                   "H-b" #'consult-project-buffer
                   "H-B" #'consult-buffer
                   "H-c" #'project-compile
                   "H-f" #'consult-flymake
                   "H-g" #'magit-status
                   "H-h" #'my/describe-symbol-at-point
                   "H-k" #'bury-buffer
                   "H-m" #'consult-bookmark

                   "H-p" project-prefix-map
                   "H-r" #'speedbar
                   "H-s" #'my/shell
                   "H-t" #'my/htop
                   "H-u" #'undo
                   "H-w" #'my/ace-window-prefix
                   "H-v" #'my/reload-buffer
                   "H-;" #'my/matching-paren

                   "<f1>" #'my/normal-screen-font-size
                   "<f2>" #'my/share-screen-font-size
                   
                   "<insert>" #'ignore  ; disable overwrite-mode
                   "<insertchar>" #'ignore  ; disable overwrite-mode
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

(defvar my/hyper-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'magit-status)
    (define-key map "b" #'consult-buffer)
    (define-key map "k" #'my/bury-or-kill-current-buffer)
    (define-key map "p" project-prefix-map)
    map)
  "Keymap for terminal hyper actions.")

;;; --- Key Chords

;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy to type with
;; one or two hands.
(my/emacs-chord-bind global-map
                     "qq" #'undo
                     "aa" #'my/ace-window-always-dispatch
                     "JJ" #'my/ace-window-previous
                     "KK" #'my/ace-window-next
                     "kk" #'my/kill-current-buffer
                     "hh" my/hyper-keys-map
                     "HH" #'my/describe-symbol-at-point
                     "hb" #'popper-kill-latest-popup
                     "sb" #'speedbar
                     "vv" #'diff-hl-show-hunk
                     "fm" #'flymake-show-buffer-diagnostics
                     "jn" #'my/ace-window-next
                     "jp" #'my/ace-window-previous)

(defun my/consult-info-emacs ()
  "Search Emacs info."
  (interactive)
  (consult-info "emacs" "autotype" "cape" "corfu" "denote" "embark" "magit" "marginalia" "orderless" "vertico"))

(defvar my/info-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'consult-info)
    (define-key map "e" #'my/consult-info-emacs)
    (define-key map "i" #'info)
    map)
  "Keymap for canned info manual searches.")

(keymap-set help-map "i" my/info-keys-map)

(if my/is-terminal
    (when my/is-linux
      ;; Undo the mapping for ESC [ so it does not take over defined xterm sequences
      (keymap-set (current-global-map) "M-[" nil)
      (defvar my/arrow-keys-map
        (let ((map (make-sparse-keymap)))
          (define-key map "A" #'previous-line)
          (define-key map "B" #'next-line)
          (define-key map "C" #'forward-char)
          (define-key map "D" #'backward-char)
          map)
        "Keymap for arrow keys")
      (define-key esc-map "O" my/arrow-keys-map))
  (when my/is-macosx
    (custom-set-variables
     '(insert-directory-program "gls")
     '(frame-resize-pixelwise t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-command-modifier 'super)
     '(mac-right-control-modifier 'hyper))))

;; Custom dir-locals
(dir-locals-set-class-variables 'raze-variables '((nil . ((compile-command . "./builds.sh -m Debug ")))))
(dir-locals-set-directory-class (file-truename "~/repos/raze") 'raze-variables)

(dir-locals-set-class-variables 'x23-variables '((nil . ((compile-command . "cmake -S . -B build && cd build && make tests ")))))
(dir-locals-set-directory-class (file-truename "~/repos/x23") 'x23-variables)

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097
;;
(let ((backup-dir (file-name-concat my/tmp-dir "emacs-backups"))
      (auto-saves-dir (file-name-concat my/tmp-dir "emacs_autosaves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (file-name-concat auto-saves-dir ".saves-")
        ;; Tramp as well but note slight change in pattern
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

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

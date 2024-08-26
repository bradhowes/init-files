;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)

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
  "The path to the Python executable to use for elpy.")
(defconst my/is-macosx (eq system-type 'darwin)
  "T if running on macOS.
Note that this is also true when running in a terminal window.")
(defconst my/is-terminal (not (display-graphic-p))
  "T if running in a terminal.")
(defconst my/is-x-windows (eq window-system 'x)
  "T if running in an X windows environment.")

(setenv "WORKON_HOME" my/venv)

(push (expand-file-name "~/.emacs.d/lisp") load-path)

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

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

(defun my/add-event-modifier (mod-prefix event)
  "Add a modifier MOD-PREFIX to EVENT."
  (let* ((mods (if (symbolp event) event (car event)))
         (new-mods (intern (concat mod-prefix (symbol-name mods)))))
    (if (symbolp event)
        new-mods
      (cons new-mods (cdr event)))))

(defun my/apply-mod-to-next-event (mod-symbol mod-bit mod-prefix)
  "Read the next event and apply the MOD-BIT or MOD-PREFIX to it.
Only adds MOD-BIT/MOD-SYMBOL if not already part of the event."
  (let ((event (read-event)))
    (vector (if (numberp event)
                (logior (ash 1 mod-bit) event)
              (if (memq mod-symbol (event-modifiers event))
                  event
                (my/add-event-modifier mod-prefix event))))))

(defun my/hyperify (_)
  "Read the next event and apply the `hyper' modifier to it.
Returns a new vector holding the hyper-ed event."
  (my/apply-mod-to-next-event 'hyper 24 "H-"))

(defun my/superify (_)
  "Read the next event and apply the `super' modifier to it.
Returns a new vector holding the super-ed event."
  (my/apply-mod-to-next-event 'super 23 "s-"))

;; Translate a "special" key into a hyper prefix. Here, "C-M-<menu>" was setup in Windows PowerToys utility.
;; I was unable to locate a key that does not repeat, so to use as a modifier just press CAPS LOCK once and then
;; another key -- don't hold it down. Here's a snippet from the PowerToys configuration file:
;;
;; "remapKeys": {
;;     "inProcess": [
;;         {
;;             "originalKeys": "20","newRemapKeys": "17;18;93"
;;         }
;;     ]
;; },
;;
;; The numbers above are the USB keycodes involved in the transformation, where 20 is the CAPS LOCK key,
;; keycode 93 is the "menu" key, and 17 and 18 are the mods to set.
;;
(when my/is-x-windows
  (keymap-set local-function-key-map "C-M-<menu>" #'my/hyperify))

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

(defun my/font-size (layout)
  "The font size to use based on the LAYOUT."
  (if (my/is-4k layout) 16 12))

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

(add-hook 'after-init-hook (lambda () (my/screen-layout-changed)))

;;; Set various paths

(let* ((common-paths (list (expand-file-name "~/bin")
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

;; Configure `display-buffer-alist' to manage window placement

(use-package window
  :init
  (let ((window-parameters '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
    (message "%s" window-parameters)
    (setq switch-to-buffer-in-dedicated-window 'pop
          switch-to-buffer-obey-display-actions t
          window-resize-pixelwise t
          window-sides-slots '(0 0 3 1) ; left top right bottom
          display-buffer-base-action '((display-buffer-reuse-window
                                        ace-display-buffer))
                                        ;; display-buffer-in-previous-window
                                        ;; display-buffer-reuse-mode-window
                                        ;; display-buffer-pop-up-window
                                        ;; display-buffer-same-window
                                        ;; display-buffer-use-some-window))
          display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window
                                                 ace-display-buffer)
                                  (reusable-frames . nil))
                                 ("\\*R" nil (reusable-frames . nil))
                                 ,(cons "\\*helm" display-buffer-fallback-action)
                                 ("magit-diff:" nil
                                  (inhibit-same-window . t))))))
          ;; display-buffer-alist `(("\\*\\(?:\\(?:Buffer List\\)\\|Ibuffer\\|\\(?:.* Buffers\\)\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . -2) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*Tags List\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . -1) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*\\(?:Help\\|grep\\|Completions\\|Apropos\\|ripgrep-search\\|\\(?:Customize Option:.*\\)\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . 0) (preserve-size . (t . nil)) ,window-parameters)
          ;;                        ("\\*\\(?:\\compilation\\|Compile-Log\\)\\*"
          ;;                         display-buffer-in-side-window (side . right) (slot . 1) (preserve-size . (t . nil)) ,window-parameters))
          ;; )))

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
 "emacs-pager" #'emacs-pager
 "my-cmake-mode" #'my/cmake-mode-hook
 "my-c++-mode" #'my/c++-mode-hook
 "my-dired-mode" #'my/dired-mode-hook
 "my-lisp-mode" #'(my/lisp-mode-hook my/lisp-data-mode-hook)
 "my-makefile-mode" #'my/makefile-mode-hook
 "my-markdown-mode" #'my/markdown-mode-hook
 "my-python-mode" #'(my/python-mode-hook my/inferior-python-mode-hook)
 "my-sh-mode" #'my/sh-mode-hook
 "my-json-mode" #'my/json-mode-hook
 "my-js2-mode" #'my/js2-mode-hook
 "my-shell-mode" #'my/shell-mode-hook)

(use-package key-chord
  :ensure t
  :vc (:url "https://github.com/emacsorphanage/key-chord"))
(require 'key-chord)

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
         (commands (seq-drop (elt groups 1) 1)))
    ;; Only execute if given a valid keymap
    (seq-mapn
     (lambda (key command) (define-key keymap (kbd key) command))
     keys commands)))

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
         (commands (seq-drop (elt groups 1) 1)))
    (seq-mapn
     (lambda (chord command) (key-chord-define keymap chord command))
     chords commands)))

;;; -- BUILT-IN PACKAGES

;; (use-package project
;;   :bind ([remap project-vc-dir] . #'magit-status))

;; (advice-add #'project-root :filter-return (lambda (path) (expand-file-name path)))

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
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.yagconf\\'" . json-mode))
  :hook ((json-mode . my/json-mode-hook)))

(use-package js2-mode
  :ensure t
  :hook ((js2-mode . my/js2-mode-hook)))

(use-package winner
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)))

(use-package server
  :commands (server-running-p)
  :hook (after-init . (lambda () (unless (server-running-p) (server-start)))))

(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode))

(use-package ibuffer
  :config
  ;; Unbind the ibuffer use of "M-o"
  (my/emacs-key-bind ibuffer-mode-map
		     "M-o" nil))

(use-package python
  :hook ((python-mode . my/python-mode-hook)
         (inferior-python-mode . my/inferior-python-mode-hook)))

(use-package makefile-mode
  :hook ((makefile-mode . my/makefile-mode-hook)
         (makefile-mode . indent-bars-mode)))

;;; -- ADDED PACKAGES

(use-package projectile
  :ensure t
  :defer nil
  :commands (projectile-mode)
  :bind-keymap (("C-x p" . projectile-command-map)
                ("H-p" . projectile-command-map))
  :config (projectile-mode))

(use-package indent-bars
  :vc (:url "https://github.com/jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode))

(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :commands (corfu-terminal-mode)
    :hook (after-init . corfu-terminal-mode)))

(when (display-graphic-p)
  (use-package all-the-icons
    :ensure t)

  (use-package all-the-icons-completion
    :ensure t
    :commands (all-the-icons-completion-mode)
    :after (marginalia all-the-icons)
    :hook ((marginalia-mode . all-the-icons-completion-marginalia-setup)))

  (use-package doom-modeline
    :ensure t))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle)))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

;; (use-package eldoc-box
;;   :ensure t
;;   :commands (eldoc-box-hover-mode)
;;   :bind ("C-h ." . eldoc-box-help-at-point))

(use-package dired
  :config
  (require 'dired-aux)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(use-package magit
  :ensure t
  :commands (magit-status-setup-buffer)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c f" . magit-file-dispatch)))

(use-package rg
  :ensure t
  :after (projectile)
  :commands (rg-enable-default-bindings)
  ;; :bind (:map projectile-command-map
  ;;             ("s r" . rg-project))
  :config
  (rg-enable-default-bindings))

(use-package denote
  :ensure t
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

(use-package ace-window
  :ensure t
  :commands (aw-window-list aw-switch-to-window aw-select aw-flip-window) ; Used in custom functions below
  :defines (aw-dispatch-always)
  :config
  (setq aw-make-frame-char ?n)
  :bind (("M-o" . ace-window)))

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

(setq aw-dispatch-alist
      '((?k aw-delete-window "Delete Window")
        (?s aw-swap-window "Swap Windows")
        (?M aw-move-window "Move Window")
        (?c aw-copy-window "Copy Window")
        (?j aw-switch-buffer-in-window "Select Buffer")
        (?n aw-flip-window)
        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?e aw-execute-command-other-window "Execute Command Other Window")
        (?F aw-split-window-fair "Split Fair Window")
        (?v aw-split-window-vert "Split Vert Window")
        (?b aw-split-window-horz "Split Horz Window")
        (?o delete-other-windows "Delete Other Windows")
        (?T aw-transpose-frame "Transpose Frame")
        (?? aw-show-dispatch-help)))

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
  :ensure t
  :after (projectile)
  :commands (consult--customize-put)    ; silence flymake warning
  :bind (("C-c M-x" . consult-mode-command)
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
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-notes
  :vc (:url "https://github.com/mclear-tools/consult-notes")
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  :bind (("C-c n b" . consult-notes)))

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package mode-line-bell
  :ensure t)

(use-package eglot
  :ensure t
  :commands (eglot-ensure)
  :hook ((swift-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
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
  :ensure t
  :after (consult eglot))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-k" . crux-smart-kill-line)
         ("C-c C-i" . crux-indent-defun)
         ("C-c i" . crux-find-user-init-file)
         ("C-c ," . crux-find-user-custom-file)
         ("C-^" . crux-top-join-line)))

(use-package orderless
  :ensure t)

(use-package swift-mode
  :ensure t
  :custom ((swift-mode:basic-offset 2))
  :hook (swift-mode . (lambda () (set (make-local-variable 'compile-command) "swift build"))))

(use-package flymake
  :ensure t
  :commands (flymake-show-buffer-diagnostics)
  :config (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package cape
  :ensure t
  :commands (cape-file))

(use-package corfu
  :ensure t)

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . my/markdown-mode))

(use-package cmake-mode
  :ensure t
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package which-key
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-\\" . er/expand-region))

(use-package hl-line
  :ensure t)

(use-package popper
  :commands (popper-kill-latest-popup)
  :ensure t
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type)))

(use-package char-menu
  :ensure t
  :defines (char-menu)
  :bind (("C-z" . char-menu))
  :config
  (setq char-menu
        '("—" "‘’" "“”" "…" "«»" "–"
          ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
          ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
          ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
          ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))))

(use-package osx-dictionary
  :when my/is-macosx
  :bind (("C-c l" . osx-dictionary-search-pointer)))

(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist
               '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2))) ; I think this is from pyright

(use-package fancy-compilation
  :hook (compile-mode . fancy-compilation-mode))

(defvar ffap-bindings
  '((keymap-global-set "<remap> <find-file>" #'find-file-at-point)
    (keymap-global-set "<remap> <find-file-other-window>" #'ffap-other-window)
    (keymap-global-set "<remap> <find-file-other-frame>" #'ffap-other-frame)
    (keymap-global-set "<remap> <find-file-other-tab>" #'ffap-other-tab)

    (keymap-global-set "<remap> <dired>" #'dired-at-point)
    (keymap-global-set "<remap> <dired-other-window>" #'ffap-dired-other-window)
    (keymap-global-set "<remap> <dired-other-frame>" #'ffap-dired-other-frame)
    (keymap-global-set "<remap> <list-directory>" #'ffap-list-directory))
  "List of binding forms evaluated by function `ffap-bindings'.
A reasonable ffap installation needs just this one line:
  (ffap-bindings)
Of course if you do not like these bindings, just roll your own!")

(use-package emacs
  :commands (my/crm-indicator)
  :config
  (setq read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil
        custom-file (locate-user-emacs-file "custom.el")
        frame-title-format (list  '(:eval (abbreviate-file-name default-directory))))
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
         (after-init . abbrev-mode)))

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

(defun my/toggle-show-minor-modes ()
  "Toggle the display of minor mode elements on the mode line."
  (interactive)
  (setq doom-modeline-minor-modes (not doom-modeline-minor-modes)))

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
  (message "Use `ace-window' to display next command buffer..."))

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
                   "C-c r" #'ielm
                   "C-c D" #'crux-find-current-directory-dir-locals-file

                   ;; NOTE: do not bind RET or <return> -- that breaks Embark maps
                   "C-h C-SPC"  #'my/toggle-show-minor-modes
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
                   "M-O" #'my/ace-window-always-dispatch

                   "C-o" #'aw-flip-window

                   "C-s" #'isearch-forward-regexp
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
                   "M-[" #'previous-buffer
                   "M-]" #'next-buffer

                   "C-S-p" #'my/ace-window-previous
                   "C-S-n" #'my/ace-window-next

                   "M-P" #'my/ace-window-previous
                   "M-N" #'my/ace-window-next

                   ;; --- Hyper-key Bindings
                   "H-SPC" #'my/ace-window-always-dispatch
                   "H-1" #'delete-other-windows
                   "H-2" #'split-window-below
                   "H-b" #'consult-buffer
                   "H-g" #'magit-status
                   "H-h" #'my/describe-symbol-at-point
                   "H-k" #'bury-buffer
                   "H-m" #'consult-bookmark
                   "H-p" #'projectile-command-map
                   "H-u" #'undo
                   "H-;" #'my/matching-paren

                   "H-M-k" #'my/kill-current-buffer

                   ;; Unmap the following
                   "<insert>" #'ignore   ; disable key for toggling overwrite mode
                   "C-x C-z" #'ignore    ; suspend-frame
                   "C-x h" #'ignore      ; mark-whole-buffer
                   "C-h h" #'ignore      ; show 'Hello' in various fonts

                   ;; Disable font size changes via trackpad
                   "C-<wheel-up>" #'ignore
                   "C-<wheel-down>" #'ignore)

;;; --- Key Chords

;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy to type with
;; one or two hands.
(my/emacs-chord-bind global-map
                     "qq" #'undo
                     "ww" #'my/ace-window-always-dispatch
                     "yy" #'consult-yank-replace

                     "hb" #'consult-buffer

                     "UU" #'my/ace-window-previous
                     "II" #'my/ace-window-next

                     "hh" #'my/describe-symbol-at-point
                     "hb" #'popper-kill-latest-popup

                     "jk" #'consult-project-buffer
                     "vv" #'diff-hl-show-hunk
                     "fm" #'flymake-show-buffer-diagnostics
                     "kk" #'my/kill-current-buffer
                     )

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(defun my/set-bound-var (symbol value)
  "Set SYMBOL with VALUE if SYMBOL exists."
  (when (boundp symbol)
    (set symbol value)))

(use-package term/xterm
  :commands (xterm--init-modify-other-keys my/init-xterm)
  :when my/is-terminal
  :init
  (defun my/init-xterm ()
    ;; Courtesy https://emacs.stackexchange.com/a/13957, modified per
    ;; https://gitlab.com/gnachman/iterm2/-/issues/8382#note_365264207
    (defun my/character-apply-modifiers (c &rest modifiers)
      "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
      (if (memq 'control modifiers) (setq c (if (and (<= ?a c) (<= c ?z))
                                                (logand c ?\x1f)
                                              (logior (ash 1 26) c))))
      (if (memq 'meta modifiers) (setq c (logior (ash 1 27) c)))
      (if (memq 'shift modifiers) (setq c (logior (ash 1 25) c)))
      (vector c))
    (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
      (let ((c 32))
        (while (<= c 126)
          (mapc (lambda (x)
	          (define-key xterm-function-map (format (car x) c)
			      (apply 'my/character-apply-modifiers c (cdr x))))
	        '(;; with ?.VT100.formatOtherKeys: 0
	          ("\e\[27;3;%d~" meta)
	          ("\e\[27;5;%d~" control)
	          ("\e\[27;6;%d~" control shift)
	          ("\e\[27;7;%d~" control meta)
	          ("\e\[27;8;%d~" control meta shift)
	          ;; with ?.VT100.formatOtherKeys: 1
	          ("\e\[%d;3u" meta)
	          ("\e\[%d;5u" control)
	          ("\e\[%d;6u" control shift)
	          ("\e\[%d;7u" control meta)
	          ("\e\[%d;8u" control meta shift)))
          (setq c (1+ c))))))
  (add-hook 'after-init-hook #'my/init-xterm))

    ;; ;; Modified from https://gist.github.com/gnachman/b4fb1e643e7e82a546bc9f86f30360e4
    ;; (unless (display-graphic-p)
    ;;   ;; Take advantage of iterm2's CSI u support (https://gitlab.com/gnachman/iterm2/-/issues/8382).
    ;;   (xterm--init-modify-other-keys)
    ;;   (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    ;;     ;; fix M-S-ret for org mode
    ;;     (define-key xterm-function-map "\e\[13;4u" [(control meta shift ?\r)])
    ;;     (define-key xterm-function-map "\e\[27;4;13~" [(control meta shift ?\r)])
    ;;     (let ((c 32))
    ;;       (while (<= c 126)
    ;;         (mapc (lambda (x)
    ;;                 ;; define-key can take a vector that wraps a list of
    ;;                 ;; events, e.g. [(control shift ?a)] for C-S-a
    ;;                 (define-key xterm-function-map (format (car x) c)
    ;;                             (vector (append (cdr x) (cons c '())))))
    ;;               '(;; with ?.VT100.formatOtherKeys: 0
    ;;                 ("\e\[27;3;%d~" meta)
    ;;                 ("\e\[27;4;%d~" meta shift)
    ;;                 ("\e\[27;5;%d~" control)
    ;;                 ("\e\[27;6;%d~" control shift)
    ;;                 ("\e\[27;7;%d~" control meta)
    ;;                 ("\e\[27;8;%d~" control meta shift)
    ;;                 ;; with ?.VT100.formatOtherKeys: 1
    ;;                 ("\e\[%d;3u" meta)
    ;;                 ("\e\[%d;4u" meta shift)
    ;;                 ("\e\[%d;5u" control)
    ;;                 ("\e\[%d;6u" control shift)
    ;;                 ("\e\[%d;7u" control meta)
    ;;                 ("\e\[%d;8u" control meta shift)))
    ;;         (setq c (1+ c)))))))

(unless my/is-terminal
  (cond
   (my/is-macosx
    (custom-set-variables
     '(insert-directory-program "gls")
     '(frame-resize-pixelwise t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-control-modifier 'hyper)))
   (my/is-x-windows
    (my/set-bound-var 'x-alt-keysym 'alt)
    (my/set-bound-var 'x-meta-keysym 'meta)
    (my/set-bound-var 'x-super-keysym 'super)
    (my/set-bound-var 'x-hyper-keysym 'hyper))))

(provide 'init)

;;; init.el ends here.

;; (define-key local-function-key-map (kbd "<rcontrol>") 'event-apply-hyper-modifier)

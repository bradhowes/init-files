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

(require 'my-constants)
(require 'my-display)
(require 'my-keys)
(require 'my-org)
(require 'my-server)

(require 'consult-notes-denote)
(require 'dired-aux)

(add-hook 'after-init-hook #'my/start-emacs-server)
(add-hook 'after-init-hook #'my/screen-layout-changed)

(autoload 'my/cmake-mode-hook "my-cmake-mode")
(autoload 'my/c++-mode-hook "my-c++-mode")
(autoload 'my/dired-mode-hook "my-dired-mode")
(autoload 'my/js2-mode-hook "my-js2-mode")
(autoload 'my/json-mode-hook "my-json-mode")
(autoload 'my/lisp-mode-hook "my-lisp-mode")
(autoload 'my/lisp-data-mode-hook "my-lisp-mode")
(autoload 'my/makefile-mode-hook "my-makefile-mode")
(autoload 'my/markdown-mode-hook "my-markdown-mode")
(autoload 'my/python-mode-hook "my-python-mode")
(autoload 'my/inferior-python-mode-hook "my-python-mode")
(autoload 'my/sh-mode-hook "my-sh-mode")
(autoload 'my/shell-mode-hook "my-shell-mode")
(autoload 'emacs-pager-mode "emacs-pager")

(autoload 'my/htop "my-top" "Run htop in window." t)
(autoload 'my/top "my-top" "Run top in window." t)
(autoload 'my/find-known-bindings "my-find-known-bindings" "Show listing of known bindings" t)
(autoload 'my/find-user-init-file "my-utils" "Edit the `user-init-file`." t)
(autoload 'my/find-user-custom-file "my-utils" "Edit the `custom-file` if it exists." t)
(autoload 'my/find-shell-init-file "my-utils" "Edit a shell init file." t)

(autoload 'my/ace-window-always-dispatch "my-display" "" t)
(autoload 'my/normal-screen-font-size "my-display" "" t)
(autoload 'my/share-screen-font-size "my-display" "" t)

;;; Set various paths

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

(use-package accent
  :bind ("C-c a" . accent-menu))

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
  :bind (("C-c n b" . consult-notes)))

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
  :defines (explicit-bash-args)
  :init
  ;; Special-case QA env -- we are logged in as `sp_qa` user but we want our custom
  ;; environment. Command `bash` to load our custom settings.
  (let ((rc (file-truename "~/configurations/qa.bashrc")))
    (if (and (file-exists-p rc)
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
         ("C-<right>" . winner-redo)))

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

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package dired
  :config
  :bind (:map dired-mode-map
              ("M-{" . nil)
              ("M-}" . nil)
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

(use-package diff-hl
  :commands (diff-hl-show-hunk diff-hl-margin-mode)
  :hook (after-init . (lambda ()
                        (when my/is-terminal
                          (diff-hl-margin-mode t))
                        )))

(use-package magit
  :after (project)
  :commands (magit-status-setup-buffer magit-status magit-project-status)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-x g" . magit-status)
         ;; Take over vc-dir
         ("C-x p v" . magit-project-status)
         ;; Take over vc-print-log
         ("C-x v l" . magit-log-buffer-file)
         ("C-c f" . magit-file-dispatch)))

(use-package rg
  :after (projectile)
  :commands (rg-enable-default-bindings rg-project)
  :bind ("C-x p s r" . #'rg-project)
  :hook
  (after-init . rg-enable-default-bindings))

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

(use-package consult
  :after (projectile)
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
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-register-format consult-register-window consult-xref)
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
                     :preview-key '(:debounce 0.4 any)))

(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (consult-project-buffer "Project buffers" ?b)
        (rg-project "Search w/ rg" ?r)
        (magit-project-status "Magit" ?m)
        (project-shell "Shell" ?$)))

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

(use-package orderless)

(use-package flymake
  :commands (flymake-show-buffer-diagnostics)
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-json)

(use-package cape
  :commands (cape-file))

(use-package corfu
  :commands (global-corfu-mode corfu-send)
  :bind
  (:map corfu-map ("C-SPC" . corfu-insert-separator))
  :config
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))
  :init
  (global-corfu-mode))

;; (use-package completion-preview
;;   :ensure nil
;;   :commands (completion-preview-next-candidate completion-preview-prev-candidate)
;;   :custom
;;   (completion-preview-minimum-symbol-length 2)
;;   :bind (:map completion-preview-active-mode-map
;;               ("M-n" . #'completion-preview-next-candidate)
;;               ("M-p" . #'completion-preview-prev-candidate))
;;   :init
;;   (global-completion-preview-mode))

;; (use-package corfu-terminal
;;   :if my/is-terminal
;;   :hook (after-init . corfu-terminal-mode))
(use-package markdown-mode
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package cmake-mode
  :hook ((cmake-mode . my/cmake-mode-hook)))

(use-package which-key)

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

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

(use-package crm)

(use-package emacs
  :commands (my/crm-indicator)
  :config
  (setq read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil
        custom-file (locate-user-emacs-file "custom.el")
        frame-title-format (list ;; my/workspace-name
                                 ;; " "
                                 '(:eval (abbreviate-file-name default-directory))))
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

(use-package magit-process
  :custom
  (magit-process-find-password-functions '(my/read-gitlab-password)))

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
  "Immediately show help for symbol at point if it exists."
  (interactive)
  (when-let ((v-or-f (symbol-at-point))
             (found (cl-some (lambda (x) (funcall (nth 1 x) v-or-f)) describe-symbol-backends)))
    (describe-symbol v-or-f (help-buffer))))

;; NOTE: this is now part of crux repo but not yet released.
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

(defun my/dired-configurations ()
  "Open Dired on configurations repo."
  (interactive)
  (dired (file-truename "~/configurations")))

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
  (when (not mark-active)
    (beginning-of-line)
    (push-mark)
    (setq mark-active t))
  (forward-line arg))

;;; --- Key Bindings

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
                   "C-M-s" #'isearch-forward-symbol

                   "C-x 0" #'delete-other-windows
                   "C-x O" #'other-frame
                   "C-x C-o" #'other-frame

                   "C-x C-b" #'ibuffer
                   "C-x M-b" #'consult-project-buffer

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
                   "H-;" #'my/matching-paren

                   "<f1>" #'my/normal-screen-font-size
                   "<f2>" #'my/share-screen-font-size

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

(defvar my/hyper-keys-map (make-sparse-keymap)
  "Keymap for terminal hyper actions.")

(keymap-set my/hyper-keys-map "g" #'magit-status)
(keymap-set my/hyper-keys-map "b" #'consult-buffer)
(keymap-set my/hyper-keys-map "k" #'my/bury-or-kill-current-buffer)
(keymap-set my/hyper-keys-map "p" project-prefix-map)

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
                     "ml" #'my/mark-line
                     "jn" #'my/ace-window-next
                     "jp" #'my/ace-window-previous)

(defun my/consult-info-emacs ()
  "Search Emacs info."
  (interactive)
  (consult-info "emacs" "autotype" "cape" "corfu" "denote" "embark" "magit" "marginalia" "orderless" "vertico"))

(defvar my/info-keys-map (make-sparse-keymap)
  "Keymap for canned info manual searches.")

(keymap-set my/info-keys-map "c" #'consult-info)
(keymap-set my/info-keys-map "e" #'my/consult-info-emacs)
(keymap-set my/info-keys-map "i" #'info)
(keymap-set help-map "i" my/info-keys-map)

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097
;;
(let ((backup-dir (concat my/tmp-dir "/emacs-backups"))
      (auto-saves-dir (concat my/tmp-dir "/emacs_autosaves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        ;; Tramp as well but note slight change in pattern
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(provide 'init)

;;; init.el ends here.

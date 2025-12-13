,;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(require 'seq)
(require 'my-constants)
(require 'my-customizations)
(require 'my-env)
(require 'my-functions)
(require 'my-layout)
(require 'my-modes)

(defun my/trusted-content-p (original-response)
  "Advice for `trusted-content-p' to trust the `*scratch*' buffer.
Honors ORIGINAL-RESPONSE when not nil and then checks the buffer's name
if it is `*scratch*'. This is a loosening of security but the risk is
very small for me, and it remove the obnoxious message at startup about
the buffer having untrusted content."
  (or original-response
      (buffer-name "*scratch*")))

(advice-add 'trusted-content-p :filter-return #'my/trusted-content-p)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

(if (or my/is-macosx (string= (system-name) "ldzls1144d"))
    (use-package package
      :custom
      (package-archive-priorities '(("melpa" . 10)
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

(defvar my/hyper-c-map
  (make-sparse-keymap)
  "Keymap for Hyper-c actions.")
(keymap-set global-map "H-c" my/hyper-c-map)

(use-package accent
  :bind ("H-c a" . accent-menu))

(use-package ace-window
  :commands (aw-window-list aw-switch-to-window aw-select aw-flip-window ace-display-buffer ace-window)
  :defines (aw-dispatch-always)
  :config (setq aw-make-frame-char ?n))

(use-package cape
  :commands (cape-file))

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

(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist
               '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2))) ; I think this is from pyright

(use-package xref
  :defines (xref-show-xrefs-function xref-show-definitions-function))

(use-package consult
  :after (project xref)
  :commands (consult--customize-put consult-flymake)
  :bind (("H-c M-x" . consult-mode-command)
         ("H-c h" . consult-history)
         ;; ("C-h i" . consult-info)
         ("H-c k" . consult-kmacro)
         ("H-c m" . consult-man)
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
         ("M-s i" . consult-imenu)      ; Duplicate 'M-g i'
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

  :commands (consult-register-format consult-register-window consult-xref consult-register-store consult-register-load)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (not lazy).
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store', and the built-in commands. This improves the
  ;; register formatting, adds thin separate lines, register sorting and hides
  ;; the window mode line.
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
  (setq consult-narrow-key "<"))

(use-package consult-notes
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  :bind (("H-n b" . consult-notes)))

(use-package corfu
  :bind (:map corfu-map ("C-SPC" . corfu-insert-separator)))

(use-package corfu-terminal
  :if my/is-terminal
  :functions (corfu-terminal-mode)
  :hook (after-init . (lambda () (corfu-terminal-mode +1))))

(use-package crm)

(use-package crux
  :defer nil                            ; load now due to dependencies below
  :bind (("C-a" . crux-move-beginning-of-line)
         ("H-c d" . crux-duplicate-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-k" . crux-smart-kill-line)
         ("H-c C-i" . crux-indent-defun)
         ("C-^" . crux-top-join-line)))

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

(use-package dired
  :config
  :bind (:map dired-mode-map
              ("M-{" . nil)
              ("M-}" . nil)
              ("c" . dired-do-copy)     ; swap copy and compress
              ("C" . dired-do-compress)
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp))
  :hook (dired-mode . my/dired-mode-hook))

(use-package eldoc-box
  :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

(use-package emacs-pager
  :commands (emacs-pager emacs-pager-mode))

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

(use-package esup
  :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

(use-package hl-line)

;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
(use-package ibuffer
  :config (keymap-unset ibuffer-mode-map "M-o" t))

(use-package iso-transl
  :bind-keymap ("H-8" . iso-transl-ctl-x-8-map)) ; Enter diacritics using "dead" keys after <H-8> or <C-X 8>

(use-package key-chord
  :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define))

(use-package magit
  :commands (magit-status-setup-buffer magit-status magit-project-status)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-x g" . magit-status)
         ;; Take over vc-dir
         ("C-x p v" . magit-project-status)
         ;; Take over vc-print-log
         ("C-x v l" . magit-log-buffer-file)
         ("H-c f" . magit-file-dispatch))
  :custom (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package marginalia
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package mode-line-bell)

(use-package mood-line
  :if (display-graphic-p)
  :commands (mood-line-mode)
  :hook (after-init . mood-line-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("H-c ." . mc/mark-all-like-this)))

(use-package my-fontify-braces)

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after (marginalia)
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :hook ((after-init . nerd-icons-completion-mode)
         (marginalia-mode nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package orderless)

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
  :bind (("H-c l" . osx-dictionary-search-pointer)))

(use-package password-cache
  :defines (password-cache password-cache-expiry)
  :commands (password-cache-add password-read password-read-from-cache)
  :custom
  (password-cache t)
  (password-cache-expiry nil))

(defun my/read-gitlab-password (host)
  "Inject password into `password-cache' for HOST."
  (if-let ((password (password-read-from-cache host)))
      password
    (let ((password (password-read "Gitlab password: " host)))
      (password-cache-add host password)
      password)))

(use-package popper
  :defer nil                            ; load now due to dependencies below
  :commands (popper-kill-latest-popup)
  :functions (popper--delete-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)))

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

(use-package rg
  :after (project)
  :commands (rg-enable-default-bindings rg-project)
  :bind ("C-x p s r" . #'rg-project)
  :hook (after-init . rg-enable-default-bindings))

(use-package scratch
  :bind (("H-c s" . scratch)))

(use-package tempo
  :commands (tempo-define-template))

(defun tempo-template-my/org-emacs-lisp-source (&optional _)
  "Define empty function to satisfy flymake/byte-compile (ARG is ignored).")

(tempo-define-template "my/org-emacs-lisp-source" '("#+begin_src emacs-lisp" & r % "#+end_src")
                       "<m"
                       "Insert an Emacs Lisp source block in an org document.")

(use-package vertico
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package which-key)

(use-package winner
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         ("H-c u" . winner-undo)
         ("H-c C-u" . winner-undo)
         ("H-c C-r" . winner-redo)))

(defun my/org-emacs-lisp-source-with-indent ()
  "Execute `my/org-emacs-lisp-source' and then indent block."
  (interactive)
  (tempo-template-my/org-emacs-lisp-source)
  (forward-line -1)
  (org-cycle))

(defun my/org-filter-buffer-substring (start end delete)
  "Custom filter on buffer text from START to END.
When DELETE is t, delete the contents from the range.
Otherwise, removes all properties from a span in a buffer.
Useful when copying code into Org blocks so that the copy does not contain any
artifacts such as indentation bars."
  (if delete
      (delete-and-extract-region start end)
    (buffer-substring-no-properties start end)))

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
  (put 'scroll-left 'disabled nil)
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

;; My own version of some `crux` routines that use `find-file` instead of `find-file-other-window`
(defun my/find-user-init-file (arg)
  "Edit the `user-init-file` when ARG is nil.
Otherwise, edit the `early-init.el' file instead, creating it if
necessary."
  (interactive "P")
  (find-file (locate-user-emacs-file (if arg "early-init.el" user-init-file))))

(defun my/find-user-custom-file ()
  "Edit the `custom-file` if it exists."
  (interactive)
  (if custom-file
      (find-file custom-file)
    (message "No custom file defined.")))

(defun my/find-shell-init-file ()
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

(defun my/reload-buffer ()
  "Reload the current buffer from disk.
Checks to see if buffer needs saving, aborting the reload if changes not saved."
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
  (my/run-something-in-buffer "*Shell*"
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
  "Show Customize in a new window."
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

(defun my/aw-make-frame ()
  "Make a new frame using layout settings for the current display.
The first frame always takes on `initial-frame-alist', and subsequent frames
use `default-frame-alist' by default. If there are already two frames active
then subsequent ones will be at `my/align-right-frame-alist' which aligns with
the right-edge of the screen, but may overlap with the middle frame."
  (let ((num-frames (length (visible-frame-list))))
    (if (< num-frames 2)
        (make-frame)
      (make-frame (my/layout--frame-right-alist (my/layout--active-screens) (my/layout--which-4k-display))))))

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
Only switch to a buffer that passes the filter defined in
`my/next-buffer-skip-filter'."
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/next-buffer-skip-filter))
    (next-buffer)))

(defun my/prev-buffer-current-window ()
  "Switch to `previous' buffer in current window with filtering.
Only switch to a buffer that passes the filter defined in
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
  (let ((name "*htop*")
        (cmd (if my/is-macosx "sudo htop" "/bin/htop")))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term cmd name))))

(defun my/top ()
  "Run top in a term buffer."
  (interactive)
  (let ((name "*top*"))
    (if (get-buffer name)
        (switch-to-buffer name)
      (ansi-term "/usr/bin/top" name))))

;; "Jump" to a well-known directory (eg "H-c j r" => dired buffer in Raze repo)
(defvar my/dired-jumps-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (tuple)
            (let* ((key (elt tuple 0))
                   (path (elt tuple 1))
                   (name (intern (or (elt tuple 3)
                                     (concat "my/jmp-" path)))))
              (fset name (lambda ()
                           (interactive)
                           (dired (if (string= "/" (substring path 0 1))
                                      (file-truename path)
                                    (files--splice-dirname-file my/repos path)))))
              (keymap-set map key name)))
          ;; Collection of 3-tuples that define a directory to jump to:
          ;; 1 - key to use
          ;; 2 - the directory to jump to (if not absolute then prepend with value from `my/repos')
          ;; 3 - the name to assign to the utility function (if nil make from directory)
          `(("a" "auv3-support" nil)
            ("i" "init-files" nil)
            ("c" "AUv3Controls" nil)
            ("l" "init-files/emacs.d/lisp" "emacs-lisp")
            ("p" "SoundFontsPlus" nil)
            ("s" "AUv3Support" nil)
            ("2" "SF2Lib" "my/jmp-qa")))
    map)
  "Keymap for quick Dired jumps.
The map is made up of tiny functions that invoke `dired' on a path.")

(defun my/git-sync (host path)
  "Execute a git pull on HOST in PATH."
  (interactive)
  (message "Running git-pull on %s:%s..." host path)
  (let* ((git (list "cd" path "&&"
                    "git" "stash" "push" "&&"
                    "git" "pull" "&&"
                    "git" "checkout" "'stash@{0}'" "emacs.d/places" "emacs.d/recentf" "&&"
                    "git" "stash" "drop"))
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
    ;; NOTE: treat `(nil local)` as the master and only update via magit
    (dolist (cfg (list (cons nil home)  ; /lxhome/howesbra/configurations
                       (cons "ldzls2164i" home)  ; vnc
                       (cons "ldzls2164i" local) ; vnc
                       (cons "nyzls1514n" local) ; internal ogsd / pickaxe
                       (cons "nyzls1644q" local) ; wolverine QA
                       (cons "nyzls1646q" local) ; raze QA
                       (cons "nyzls2686q" local) ; tcs QA
                       (cons "nyzls105i" local)))  ; logs archive
      (my/git-sync (car cfg) (cdr cfg))))
  (display-buffer my/git-sync-buffer-name))

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

(defun my/qa-emacs-wolv ()
  "Launch X11 Emacs on nyzls1644q."
  (interactive)
  (my/launch-qa-emacs "nyzls1644q"))

(defun my/qa-emacs-raze ()
  "Launch X11 Emacs on nyzls1646q."
  (interactive)
  (my/launch-qa-emacs "nyzls1646q"))

(defun my/qa-emacs-tcs ()
  "Launch X11 Emacs on nyzls2686q."
  (interactive)
  (my/launch-qa-emacs "nyzls2686q"))

(defun my/set-mark-deactivate ()
  "Set mark without activating it.
This is just a shortcut for \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (set-mark-command nil)
  (when transient-mark-mode
    (deactivate-mark)))

(defun my/goto-mark ()
  "Move back to mark without enabling transient mode.
This is just a shortcut for \\[universal-argument] \\[set-mark-command]."
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

;; "Jump" to a saved position -- "H-j"
(defvar my/point-jumps-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'consult-register-store)
    (define-key map "j" #'consult-register-load)
    map)
  "Keymap for quick Dired jumps.")

;;; --- Key Bindings

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
  (mapc (lambda (pair) (keymap-set keymap (funcall make-key (elt pair 0)) (elt pair 1))) (seq-split definitions 2)))

(defun my/emacs-key-bind (keymap &rest definitions)
  "Apply key binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (apply #'my/emacs-make-key-bind keymap (lambda (key) key) definitions))

(defun my/emacs-chord-bind (keymap &rest definitions)
  "Apply chord binding DEFINITIONS in the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs given as a sequence."
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of chord+command pairs"))
  (unless (keymapp keymap)
    (error "Expected a `keymap' as first argument"))
  (mapc (lambda (pair) (key-chord-define keymap (elt pair 0) (elt pair 1))) (seq-split definitions 2)))

(my/emacs-key-bind global-map
                   "S-<left>" #'my/ace-window-previous
                   "S-<right>" #'my/ace-window-next
                   "S-<up>" #'my/ace-window-previous
                   "S-<down>" #'my/ace-window-next

                   "H-c r" #'ielm
                   "H-c D" #'crux-find-current-directory-dir-locals-file

                   "H-c i" #'my/find-user-init-file
                   "H-c H-l" #'my/find-user-init-file
                   "H-c ," #'my/find-user-custom-file
                   "H-c S" #'my/find-shell-init-file
                   "H-c j" my/dired-jumps-map
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

                   ;; "C-s" #'isearch-forward-regexp
                   ;; "C-M-s" #'isearch-forward-symbol

                   "C-x 0" #'delete-other-windows
                   "C-x O" #'other-frame
                   "C-x C-o" #'other-frame

                   "C-x C-b" #'ibuffer
                   "C-x M-b" #'consult-project-buffer
                   "C-x M-v" #'my/reload-buffer

                   "M-{" #'my/prev-buffer-current-window
                   "M-}" #'my/next-buffer-current-window

                   "M-<f1>" #'my/layout-frame-pos-left
                   "M-<f2>" #'my/layout-frame-pos-center
                   "M-<f3>" #'my/layout-frame-pos-right

                   "C-x 4 c" #'my/customize-other-window
                   "C-x 4 k" #'my/shell-other-window
                   "C-x 4 r" #'my/repl-other-window

                   "C-x 5 i" #'my/info-other-frame
                   "C-x 5 k" #'my/shell-other-frame

                   "H-c H-c" #'my/copy-file-name-to-clipboard
                   "H-c H-k" #'my/kill-current-buffer

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

                   "<f1>" #'my/layout-normal-screen-font-size
                   "<f2>" #'my/layout-share-screen-font-size
                   
                   "<insert>" #'ignore  ; disable key for toggling overwrite-mode
                   "<insertchar>" #'ignore  ; disable key for toggling overwrite-mode
                   "C-x C-z" #'ignore   ; suspend-frame

                   "C-x C-+" #'ignore   ; text-scale-adjust
                   "C-x C-=" #'ignore   ; text-scale-adjust
                   "C-x C--" #'ignore   ; text-scale-adjust

                   "C-x h" #'ignore     ; mark-whole-buffer
                   "C-h h" #'ignore     ; show 'Hello' in various fonts

                   ;; Disable font size changes via trackpad/scroll-wheel
                   "C-<mouse-4>" #'ignore
                   "C-<mouse-5>" #'ignore
                   "C-<wheel-up>" #'ignore
                   "C-<wheel-down>" #'ignore

                   "C-M-<mouse-4>" #'ignore
                   "C-M-<mouse-5>" #'ignore
                   "C-M-<wheel-up>" #'ignore
                   "C-M-<wheel-down>" #'ignore)

(defvar my/hyper-keys-map
  (make-sparse-keymap)
  "Keymap for terminal hyper actions.")

;; Populate two key maps with hyper-key definitions. The first -- global -- holds the mapping that uses the real `Hyper'
;; modifier. The second keymap -- my/hyper-keys-map -- holds the mapping that uses a keychord to activitate which is
;; useful on terminals that do not offer a `Hyper' modifier.
(let ((hyper-mapping (list "H-SPC" #'my/set-mark-deactivate
                           "H-." #'my/goto-mark
                           "H-1" #'delete-other-windows
                           "H-2" #'split-window-below
                           "H-4" #'other-window-prefix ; was ctl-x-4-prefix
                           "H-5" #'other-frame-prefix  ; was ctl-x-5-prefix
                           "H-a" #'ace-window
                           "H-b" #'consult-project-buffer
                           "H-B" #'consult-buffer
                           ;; "H-c" my/hyper-c-map
                           "H-f" #'consult-flymake
                           "H-g" #'magit-status
                           "H-h" #'my/describe-symbol-at-point
                           "H-j" my/point-jumps-map
                           "H-k" #'bury-buffer
                           "H-m" #'consult-bookmark
                           "H-p" project-prefix-map
                           "H-r" #'speedbar
                           "H-s" #'my/shell
                           "H-t" #'my/htop
                           "H-u" #'undo
                           "H-v" #'my/reload-buffer
                           "H-w" #'my/ace-window-prefix
                           "H-;" #'my/matching-paren)))
  (apply #'my/emacs-key-bind global-map hyper-mapping)
  (apply #'my/emacs-make-key-bind my/hyper-keys-map (lambda (key) (substring key 2)) hyper-mapping))

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
      (set-face-background 'default "undefined")
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
      (keymap-set esc-map "O" my/arrow-keys-map))
  (when my/is-macosx
    (custom-set-variables
     '(insert-directory-program "gls")
     '(frame-resize-pixelwise t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)

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
     '(mac-right-control-modifier 'hyper))))

;; Custom dir-locals
(dir-locals-set-class-variables 'raze-variables '((nil . ((compile-command . "./build.sh -m Debug ")))))
(dir-locals-set-directory-class (file-truename "~/repos/raze") 'raze-variables)

(dir-locals-set-class-variables 'x23-variables '((nil . ((compile-command . "cmake -S . -B build && cd build && make tests ")))))
(dir-locals-set-directory-class (file-truename "~/repos/x23") 'x23-variables)

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097
;;
(let ((backup-dir (file-name-concat my/tmp-dir "emacs_backups"))
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

(provide 'init)

;;; init.el ends here.

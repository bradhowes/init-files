;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(require 'seq)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))

;; (debug-on-entry 'file-notify-add-watch)

(defconst my-venv (expand-file-name "~/venv")
  "The Python virtual environment to use for elpy.")
(defconst my-venv-python (concat my-venv "/bin/python")
  "The path to the Python eexecutable to use for elpy.")
(defconst is-macosx (eq system-type 'darwin)
  "T if running on macOS.")
(defconst is-terminal (eq window-system nil)
  "T if running in a terminal.")
(defconst is-laptop (eq (display-pixel-height nil) 1329)
  "T if the current display is on laptop display.")
(defconst is-4k (eq (display-pixel-height nil) 2160)
  "T if the current display is 4K.")
(defconst my-rows (if (or is-4k is-laptop) 88 40)
  "The number of rows to show in a frame based on display height.")
(defconst my-cols (if (or is-4k is-laptop) 132 80)
  "The number of columns to show in a frame based on display height.")
(defconst my-window-offset (if is-4k (+ 960 264) 960)
  "The offset to the `alt' window based on display height.")
(defconst my-window-right-offset (if is-4k (* my-window-offset 2) (- (display-pixel-width) my-window-offset))
  "The offset to the `alt' window based on display height.")
(defconst my-font-name "Berkeley Mono"
  "The name of the font to use.")
(defconst my-font-size (if is-4k 15 12)
  "The font size to use based on the display height.")
(defvar my-align-right-frame-alist
  `((width . ,my-cols) (height . ,my-rows) (top . 0) (left . ,my-window-right-offset))
  "The alist to use to place a frame aligned to the right size of the display.")

(setenv "WORKON_HOME" my-venv)

(defun my-setup-font ()
  "Install the desired font in the default face."
  (set-face-attribute 'default nil :font (font-spec :family my-font-name :size my-font-size)))

(add-hook 'after-init-hook #'my-setup-font)

(setq read-process-output-max (* 1024 1024)
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      load-path (append (list (expand-file-name "~/.emacs.d/lisp")) load-path)

      default-frame-alist `((width . ,my-cols) (height . ,my-rows) (top . 0) (left . ,my-window-offset))
      initial-frame-alist `((width . ,my-cols) (height . ,my-rows) (top . 0) (left . 0))

      frame-title-format (list  '(:eval (abbreviate-file-name default-directory)))

      scroll-conservatively 101
      scroll-margin 2)

(recentf-mode t)

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
     '(mac-right-option-modifier 'super))))

(let* ((common-paths (list (expand-file-name "~/bin")
                           (concat my-venv "/bin")))
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
(autoload 'my-lisp-mode-hook "my-lisp-mode")
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)
(add-hook 'scheme-mode-hook 'my-lisp-mode-hook)
(autoload 'my-lisp-data-mode-hook "my-lisp-mode")
(add-hook 'lisp-data-mode-hook 'my-lisp-data-mode-hook)
(autoload 'my-cmake-mode-hook "my-cmake-mode")
(add-hook 'cmake-mode-hook 'my-cmake-mode-hook)
(autoload 'my-c++-mode-hook "my-c++-mode")
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(autoload 'my-sh-mode-hook "my-sh-mode")
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(autoload 'my-shell-mode-hook "my-shell-mode")
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(autoload 'my-makefile-mode-hook "my-makefile-mode")
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

(unless (daemonp)
  (use-package session
    :hook (after-init . session-initialize)))

(when (display-graphic-p)
  (use-package doom-themes
    :defer t
    ;; :hook (after-init . session-initialize)
    :functions (doom-themes-visual-bell-config
                doom-themes-neotree-config)
    :custom
    ((doom-themes-enable-bold t)
     (doom-themes-enable-italic t))

    :init
    (load-theme 'doom-tomorrow-night t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config))

  (use-package all-the-icons)

  (use-package doom-modeline
    :hook (after-init . doom-modeline-mode)))

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

(use-package ediff
  :defer t
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

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
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package projectile
  :commands (projectile-project-root))

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
         ("M-s c" . consult-locate)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s k" . consult-keep-lines)
         ("M-s l" . consult-line)
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
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-register-format consult-register-window consult-xref)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :review-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :init
  (vertico-mode)
  (setq vertico-count 20
        vertico-resize t
        vertico-cycle t))

(use-package orderless
  :ensure t
  :commands (orderless-escapable-split-on-space)
  :init (icomplete-mode)
  :custom ((completion-styles '(orderless basic))
           (completion-category-defaults nil)
           (completion-category-overrides '((file (styles partial-completion))))))

(use-package emacs
  :ensure t
  :custom ((minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
           (enable-recursive-minibuffers t))
  :hook ((minibuffer-setup . cursor-intangible-mode)))

(use-package magit
  :ensure t
  :defer t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))

(use-package mode-line-bell
  :ensure t)

(use-package eglot
  :ensure t
  :defer t
  :commands (eglot-ensure)
  :hook ((python-mode . eglot-ensure)))

(use-package flymake
  :ensure t
  :defer t
  :config
  (setq elisp-flymake-byte-compile-load-path load-path
        flymake-mode-line-title "FM")
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package diff-hl
  :ensure t
  :defer t
  :commands (diff-hl-flydiff-mode)
  :init (diff-hl-flydiff-mode t)
  :hook (after-init . global-diff-hl-mode))

(use-package company
  :ensure t
  :commands (company-complete)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  :bind (("C-c C-c" . #'company-complete)))

(use-package company-quickhelp
  :ensure t
  :hook ((after-init . company-quickhelp-mode)))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package winner
  :hook (after-init . winner-mode))

(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-mode)
  ;; :config
  ;; (setq projectile-completion-system 'helm)
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :hook (prog-mode . projectile-mode))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package server
  :commands (server-running-p)
  :hook (emacs-startup . (lambda () (unless (server-running-p) (server-start)))))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :config
  (which-key-mode t))

(use-package hl-line
  :commands (global-hl-line-mode)
  :hook (emacs-startup . global-hl-line-mode))

(use-package marginalia
  :ensure t
  :hook (emacs-startup . marginalia-mode))

(global-prettify-symbols-mode t)

(autoload 'native-complete-setup-bash "native-complete")
(with-eval-after-load 'shell
  (message "Loading native-complete-setup-bash")
  (native-complete-setup-bash))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

(defun my-reset-frame-left ()
  "Reset frame size and position for left frame."
  (interactive)
  (modify-frame-parameters (window-frame (get-buffer-window)) initial-frame-alist))

(defun my-reset-frame-right ()
  "Reset frame size and position for right frame."
  (interactive)
  (modify-frame-parameters (window-frame (get-buffer-window)) default-frame-alist))

(defun my-reset-frame-right-display ()
  "Reset frame size and position for right side of display frame."
  (interactive)
  (modify-frame-parameters (window-frame (get-buffer-window)) my-align-right-frame-alist))

(defun my-reset-framewidth ()
  "Reset the current frame width to `my-cols'."
  (interactive)
  (set-frame-width (window-frame (get-buffer-window)) my-cols))

(defun ksh ()
  "Start a new shell."
  (interactive)
  (let ((tmp (get-buffer-create "*Shell*")))
    (switch-to-buffer tmp nil t)
    (shell tmp)))

(defun my-shell-other-window ()
  "Start a new shell in another window."
  (interactive)
  (let ((tmp (get-buffer-create "*Shell*")))
    (switch-to-buffer-other-window tmp)
    (ksh)))

(defun my-shell-other-frame ()
  "Start a new shell in another frame."
  (interactive)
  (select-frame (make-frame))
  (ksh))

(defun my-kill-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-info-other-frame ()
  "Show Info in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*info*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (info nil tmp)))

(defun my-customize-other-window ()
  "Show Customize in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (switch-to-buffer-other-window tmp)
    (customize)))

(defun my-customize-other-frame ()
  "Show Customize in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*Customize Group: Emacs*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (customize)))

(defun my-matching-paren (arg)
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

(defun my-indent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'copyright-update)

(global-set-key (kbd "C-x O") #'other-frame)
(global-set-key (kbd "C-x C-o") #'other-frame)

(global-set-key (kbd "M-<f1>") #'my-reset-frame-left)
(global-set-key (kbd "M-<f2>") #'my-reset-frame-right)
(global-set-key (kbd "M-<f3>") #'my-reset-frame-right-display)

(global-set-key (kbd "C-x 4 c") #'my-customize-other-window)
(global-set-key (kbd "C-x 4 k") #'my-shell-other-window)

;; (global-set-key [(?%)] #'my-matching-paren)
(global-set-key (kbd "C-x 5 c") #'my-customize-other-frame)
(global-set-key (kbd "C-x 5 i") #'my-info-other-frame)
(global-set-key (kbd "C-x 5 k") #'my-shell-other-frame)

(global-set-key (kbd "C-c C-k") #'my-kill-buffer)
(global-set-key (kbd "<f3>") #'eval-last-sexp)
(global-set-key (kbd "C-M-\\") #'my-indent-buffer)
(global-set-key (kbd "<home>") #'beginning-of-buffer)
(global-set-key (kbd "<end>") #'end-of-buffer)
(global-set-key (kbd "<delete>") #'delete-char)
(global-set-key (kbd "S-<f12>") #'package-list-packages)

(global-unset-key (kbd "C-z"))          ; suspend-frame
(global-unset-key (kbd "C-x C-z"))      ; suspend-frame
(global-unset-key (kbd "C-x h"))        ; mark-whole-bufferk

;; Disable font size changing based on mouse/trackpad changes
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<double-mouse-4>"))
(global-unset-key (kbd "C-<double-mouse-5>"))
(global-unset-key (kbd "C-<triple-mouse-4>"))
(global-unset-key (kbd "C-<triple-mouse-5>"))

(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)

;; (global-unset-key (kbd "M-<mouse-1>"))
;; (global-unset-key (kbd "M-<down-mouse-1>"))

;; (global-set-key [(control c)(control f)] #'ff-find-other-file)

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(setq gc-cons-threshold my-gc-cons-threshold)

(provide 'init)

;;; init.el ends here.

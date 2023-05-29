;;; package -- .emacs  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--

;; (require 'benchmark-init)
;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

(require 'cl-seq)
(require 'hl-line)
(require 'package)
(require 'server)
(require 'seq)
(require 'filenotify)
(require 'use-package)

(defconst my-venv (expand-file-name "~/venv"))
(defconst my-venv-python (concat my-venv "/bin/python"))
(defconst is-macosx (eq system-type 'darwin))
(defconst is-terminal (eq window-system nil))
(setenv "WORKON_HOME" my-venv)

(setq file-notify-debug nil)

(defconst my-rows
  (cond
   ((eq (display-pixel-height) 2160) 100)
   (t 60)))

(defconst my-cols
  (cond
   ((eq (display-pixel-height) 3840) 132)
   (t 124)))

;; We use source for helm and elpy due to packaging issues
(setq custom-file "~/.emacs.d/custom.el"
      frame-title-format (list  (system-name) ":" '(:eval (abbreviate-file-name default-directory)))
      load-path (cons (expand-file-name "~/src/pos-tip")
                      (cons (expand-file-name "~/src/helm")
                            (cons (expand-file-name "~/src/elpy")
                                  (cons (expand-file-name "~/.emacs.d/lisp")
                                        load-path)))))

(setq default-frame-alist `((width . ,my-cols) (height . ,my-rows) (top . 0) (left . 1024))
      initial-frame-alist `((width . ,my-cols) (height . ,my-rows) (top . 0) (left . 0)))

(load custom-file t)

;; (package-initialize)

(when is-macosx
  (eval-when-compile
    (require 'ls-lisp))
  (custom-set-variables
   '(ls-lisp-use-insert-directory-program nil))
  (when (not is-terminal)
    (custom-set-variables
     '(mac-command-modifier 'meta)
     '(mac-option-modifier 'alt)
     '(mac-right-option-modifier 'super))))

(let ((additional-paths (seq-filter #'file-exists-p
                                    (list (expand-file-name "~/venvs/notebooks/bin")
                                          (expand-file-name "~/bin")
                                          "/opt/homebrew/opt/sqlite/bin"
                                          "/opt/homebrew/opt/grep/libexec/gnubin"
                                          "/opt/homebrew/bin"))))
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
    :pin manual
    :hook (after-init . session-initialize)))

(when (display-graphic-p)
  (use-package doom-themes
    :defer t
    :pin manual
    :hook (after-init . session-initialize)

    :functions (doom-themes-visual-bell-config
                doom-themes-neotree-config)

    :custom
    ((doom-themes-enable-bold t)
     (doom-themes-enable-italic t))
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-ayu-dark t)
  ;; (load-;TODO: heme 'doom-ir-black t)
  ;; (load-theme 'doom-material-dark t)
  ;; (load-theme 'doom-tokyo-night t)
    :init
    (load-theme 'doom-tomorrow-night t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config))
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-xcode t)
  ;; (load-theme 'doom-zenburn t)

  (use-package all-the-icons
    :defer t
    :pin manual)

  (use-package doom-modeline
    :defer t
    :pin manual
    :hook (after-init . doom-modeline-mode)))

(use-package magit
  :defer t
  :pin manual
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))

;;; Use packages -- all custom settings are in `'custom.el`'

;; (use-package mode-line-bell)

(use-package helm
  :defer t
  :pin manual
  :config
  (require 'helm-config)
  (require 'helm-files)
  (require 'helm-projectile)
  :custom-face
  (helm-mark-prefix ((t (:foreground "Gold1"))))
  :bind (("C-x C-f" . helm-find-files)
         ("<f12>" . helm-find-files)
         ("C-x C-p" . helm-browse-project)
         ("M-<f12>" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("M-h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-h v" . helm-apropos)
         ("C-h f" . helm-apropos)
         :map helm-command-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("M-o" . helm-ff-run-switch-other-window)
         ("M-5" . helm-ff-run-switch-other-frame)))

(use-package helm-mode
  :defer t
  :pin manual
  :diminish
  :after helm
  :hook ((after-init . helm-mode)
         (after-init . helm-autoresize-mode)))

(use-package helm-projectile
  :defer t
  :pin manual
  :after helm-mode
  :bind ("M-h p" . helm-projectile)
  :hook ((after-init . helm-projectile-on)))

(use-package helm-ls-git
  :defer t
  :pin manual
  :after helm-projectile)

(use-package flymake
  :defer t
  :pin manual
  :functions flymake--mode-line-format
  :config
  (setq elisp-flymake-byte-compile-load-path load-path
        flymake-mode-line-title "FM")
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package diff-hl
  :defer
  :pin manual
  :commands diff-hl-flydiff-mode
  :init
  (diff-hl-flydiff-mode t)
  :hook (after-init . global-diff-hl-mode))

(require 'company)

(defun my-company-number ()
  "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    ;; (message "Command-key: %s" k)
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (if (string-equal "0" k) 10 (string-to-number k))))))

(use-package company-quickhelp
  :defer t
  :pin manual
  :hook ((after-init . company-quickhelp-mode)))

(use-package company
  :defer t
  :pin manual
  :defines company-ispell-dictionary
  :hook ((after-init . global-company-mode))
  :bind (("C-c ." . company-complete)
         ("C-c C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("C-d" . company-show-doc-buffer)
         ("C-c h" . company-quickhelp-manual-begin)
         ("M-." . company-show-location)
         ("1" . my-company-number)
         ("2" . my-company-number)
         ("3" . my-company-number)
         ("4" . my-company-number)
         ("5" . my-company-number)
         ("6" . my-company-number)
         ("7" . my-company-number)
         ("8" . my-company-number)
         ("9" . my-company-number)
         ("0" . my-company-number)
         ))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; (use-package elec-pair
;;   :init
;;   (electric-pair-mode 1))

(use-package markdown-mode
  :defer t
  :pin manual)

(use-package org-edna
  :defer t
  :pin manual
  :hook (after-init . org-edna-mode))

(use-package org-gtd
  :defer t
  :pin manual
  :after org-edna
  :config
  (require 'org-gtd-inbox-processing)
  :hook (after-init . org-gtd-mode)
  :bind-keymap ("C-c d" . org-gtd-process-map)
  :bind (:map org-gtd-process-map
              ("C-c c" . org-gtd-choose)
              ("d" . org-gtd-capture)
              ("e" . org-gtd-engage)
              ("i" . org-gtd-process-inbox)))

(use-package eglot
  :defer t
  :pin manual
  :commands eglot-ensure
  :hook (c-mode-common . (lambda () (eglot-ensure))))

(use-package elpy
  :defer t
  :pin manual
  :hook (after-init . elpy-enable))

(use-package projectile
  :defer t
  :pin manual
  :commands projectile-mode
  :config
  (setq projectile-completion-system 'helm)
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :hook (prog-mode . projectile-mode))

(use-package cmake-mode
  :defer t
  :pin manual)

(use-package server
  :defer t
  :pin manual)

(defun my-emacs-startup-hook ()
  "My custom startup hook."
  (unless (server-running-p)
    (server-start))
  (global-hl-line-mode))

(add-hook 'emacs-startup-hook #'my-emacs-startup-hook)

(require 'native-complete)

(with-eval-after-load 'shell
  (message "Loading native-complete-setup-bash")
  (native-complete-setup-bash))

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

(defun my-reset-framewidth ()
  "Reset the current frame width to 120."
  (interactive)
  (set-frame-width (window-frame (get-buffer-window)) 124))

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

(global-set-key (kbd "M-<f1>") #'my-reset-frame-left)
(global-set-key (kbd "M-<f2>") #'my-reset-frame-right)

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

(global-unset-key (kbd "C-z"))          ; suspend-frame
(global-unset-key (kbd "C-x C-z"))      ; suspend-frame
(global-unset-key (kbd "C-x h"))        ; mark-whole-bufferk

(global-set-key (kbd "<delete>") #'delete-char)

;; Disable font size changing based on mouse/trackpad changes
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<double-mouse-4>"))
(global-unset-key (kbd "C-<double-mouse-5>"))
(global-unset-key (kbd "C-<triple-mouse-4>"))
(global-unset-key (kbd "C-<triple-mouse-5>"))

;; (global-unset-key (kbd "M-<mouse-1>"))
;; (global-unset-key (kbd "M-<down-mouse-1>"))

;; (global-set-key [(control meta right)] #'c-forward-into-nomenclature)
;; (global-set-key [(control meta left)] #'c-backward-into-nomenclature)
;; (global-set-key [(control c)(control f)] #'ff-find-other-file)

;;; init.el ends here.

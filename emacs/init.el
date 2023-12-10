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
(defconst my-window-offset 960
  "The offset to the `alt' window based on display height.")
(defconst my-window-right-offset (- (display-pixel-width) my-window-offset)
  "The offset to the `alt' window based on display height.")
(defconst my-font-name "Berkeley Mono"
  "The name of the font to use.")
(defconst my-font-size 12
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

(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)

         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)

         ("M-y" . consult-yank-replace)
         ("C-x M-b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("C-s" . consult-line)
         ("C-c i" . consult-info))
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

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
  (setq consult-narry-wky "<")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 20
        vertico-resize t
        vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package emacs
  :init
  (require 'crm)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

;; (use-package helm
;;   :ensure t
;;   :config
;;   (require 'helm-autoloads)
;;   (require 'helm-bookmark)
;;   (require 'helm-buffers)
;;   (require 'helm-files)
;;   :defines helm-find-files-map helm-buffer-map helm-bookmark-map
;;   :custom-face
;;   (helm-mark-prefix ((t (:foreground "Gold1"))))
;;   :bind (("C-x C-f" . helm-find-files)
;;          ("<f12>" . helm-find-files)
;;          ("C-x C-p" . helm-projectile)
;;          ("M-<f12>" . helm-browse-project)
;;          ("C-x C-b" . helm-filtered-bookmarks)
;;          ("C-x r b" . helm-filtered-bookmarks)
;;          ("C-x b"   . helm-mini)
;;          ("M-h"     . helm-command-prefix)
;; 	 ("M-x"     . helm-M-x)
;;          ("M-y"     . helm-show-kill-ring)
;;          ("C-h f"   . helm-apropos)
;;          ("C-h v"   . helm-apropos)
;;          ("C-h i"   . helm-info)
;;          :map helm-find-files-map
;;          ("M-w" . helm-ff-run-switch-other-window)
;;          ("M-f" . helm-ff-run-switch-other-frame)
;;          :map helm-buffer-map
;;          ("M-w" . helm-buffer-switch-other-window)
;;          ("M-f" . helm-buffer-switch-other-frame)
;;          :map helm-bookmark-map
;;          ("M-w" . helm-bookmark-run-jump-other-window)
;;          ("M-f" . helm-bookmark-run-jump-other-frame)
;;          ))

;; (use-package helm-mode
;;   :diminish
;;   :defer t
;;   :after helm
;;   :hook ((after-init . helm-mode)
;;          (after-init . helm-autoresize-mode)))

;; (use-package helm-projectile
;;   :ensure t
;;   :defer t
;;   :after helm-mode
;;   :hook ((after-init . helm-projectile-on)))

(use-package magit
  :ensure t
  :defer t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         ;; (magit-mode . helm-mode)
         )
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))

(use-package mode-line-bell
  :ensure t)

(use-package eglot
  :ensure t
  :defer t
  :commands eglot-ensure
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
  :commands diff-hl-flydiff-mode
  :init
  (diff-hl-flydiff-mode t)
  :hook (after-init . global-diff-hl-mode))

(use-package company
  :ensure t
  :commands company-complete
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
  :commands projectile-mode
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
  :commands which-key-mode
  :config
  (which-key-mode t))

(use-package hl-line
  :commands global-hl-line-mode
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
  :commands diminish
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

(provide 'init)

;;; init.el ends here.

;;; package -- .emacs  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; (require 'benchmark-init)

;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

(require 'cl-seq)
;; (require 'company)
(require 'hl-line)
(require 'package)
(require 'server)

(setq load-path (cons (expand-file-name "~/src/helm")
                      (cons (expand-file-name "~/src/elpy")
                            (cons (expand-file-name "~/.emacs.d/lisp")
                            load-path)))
      custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/"))

;; (package-initialize)

(require 'use-package)

(defconst is-macosx (eq system-type 'darwin))
(defconst is-terminal (eq window-system nil))
(defconst my-user (user-login-name))
(defconst my-hostname
  (let ((v (or (getenv "HOSTNAME")
	       (when (shell-command "hostname" "*foo*")
		 (with-current-buffer "*foo*"
		   (goto-char (point-min))
		   (end-of-line 1)
		   (buffer-substring-no-properties (point-min) (point))))
	       "???")))
    (nth 0 (split-string v "[.]" t))))

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

(setenv "WORKON_HOME" (expand-file-name "~/venvs/notebooks"))

(setq frame-title-format
      (list  my-user "@" my-hostname ":" '(:eval (abbreviate-file-name default-directory))))

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

    :hook (after-init . session-initialize)))

(when (display-graphic-p)
  (use-package doom-themes
    :defer t
    :pin manual
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
  :commands helm-mode helm-autoresize-mode helm-projectile
  :defines helm-find-files-map
  :config
  (require 'helm-config)
  (require 'helm-files)
  :custom-face
  (helm-mark-prefix ((t (:foreground "Gold1"))))
  :bind (("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
         ("C-x C-p" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("M-h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-h v" . helm-apropos)
         ("C-h f" . helm-apropos)
         :map helm-find-files-map
         ("M-4" . helm-ff-run-switch-other-window)
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
  :bind ("M-h p" . helm-projectile))

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

(use-package yasnippet
  :defer t
  :pin manual
  :hook (after-init . yas-global-mode))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :hook (after-init . yasnippet-snippets-initialize))

(use-package company
  :defer t
  :pin manual
  :defines company-ispell-dictionary
  :hook ((after-init . global-company-mode))
  :bind (("C-c ." . company-complete)
         ("C-c C-." . company-complete)
         ("C-c y" . company-yasnippet)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("C-d" . company-show-doc-buffer)
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

;; (use-package emacssql-sqlite3)

;; (use-package org-roam
;;   :after org
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-database-connector 'sqlite3)
;;   (org-roam-directory (expand-file-name "~/org-roam"))
;;   (org-roam-completion-everywhere t)
;;   (org-roam-completion-system 'helm)
;;   :config
;;   (org-roam-db-autosync-mode 1)
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   :bind (("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          (:map org-mode-map
;;                (("C-c n i" . org-roam-node-insert)
;;                 ("C-c n o" . org-id-get-create)
;;                 ("C-c n t" . org-roam-tag-add)
;;                 ("C-c n a" . org-roam-alias-add)
;;                 ("C-c n l" . org-roam-buffer-toggle)))))

;; (require 'fancy-compilation)
;; (fancy-compilation-mode)

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

;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (require 'diminish)
;; (diminish 'abbrev-mode "A")
;; (diminish 'company-mode "C")
;; (diminish 'eldoc-mode "D")
;; (diminish 'auto-fill-function "F")
;; (diminish 'subword-mode "S")
;; (diminish 'yas-minor-mode "Y")

(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Custom functions

(defun my-reset-framewidth ()
  "Reset the current frame width to 132."
  (interactive)
  (set-frame-width (window-frame (get-buffer-window)) 132))

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

(global-set-key [(control x)(?4)(c)] #'my-customize-other-window)
(global-set-key [(control x)(?4)(k)] #'my-shell-other-window)
;; (global-set-key [(?%)] #'my-matching-paren)
(global-set-key [(control x)(?5)(c)] #'my-customize-other-frame)
(global-set-key [(control x)(?5)(i)] #'my-info-other-frame)
(global-set-key [(control x)(?5)(k)] #'my-shell-other-frame)
(global-set-key [(control c)(control k)] #'my-kill-buffer)
(global-set-key [(f3)] #'eval-last-sexp)
(global-set-key [(control meta ?\\)] #'my-indent-buffer)
(global-set-key [(home)] #'beginning-of-buffer)
(global-set-key [(end)] #'end-of-buffer)

(global-unset-key [(control z)])
(global-unset-key [(control x)(h)])

;; (global-set-key (kbd "C-x h") 'helm-command-prefix)

(global-set-key [delete] #'delete-char)

;; Disable font size changing based on mouse/trackpad changes
(global-unset-key [(control wheel-down)])
(global-unset-key [(control wheel-up)])

;; (global-unset-key (kbd "M-<mouse-1>"))
;; (global-unset-key (kbd "M-<down-mouse-1>"))

;; (global-set-key [(control meta right)] #'c-forward-into-nomenclature)
;; (global-set-key [(control meta left)] #'c-backward-into-nomenclature)
;; (global-set-key [(control c)(control f)] #'ff-find-other-file)

;;; init.el ends here.

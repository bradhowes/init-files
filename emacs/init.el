;;; package -- .emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'benchmark-init)

(setq load-path (cons (expand-file-name "~/src/helm") (cons (expand-file-name "~/.emacs.d/lisp") load-path)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
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

;; We do this when starting up as an app.
(setenv "PATH" (mapconcat 'identity
			  (list (expand-file-name "~/venvs/notebooks/bin")
				(expand-file-name "~/bin")
				"/opt/homebrew/bin"
				(getenv "PATH")) ":"))

;; We do the same for Emacs
(setq exec-path
      (append (list (expand-file-name "~/venvs/notebooks/bin")
		    (expand-file-name "~/bin")
		    "/opt/homebrew/bin")
	      exec-path))

(setq frame-title-format
      (list  my-user "@" my-hostname ":" '(:eval (abbreviate-file-name default-directory))))

(when (eq window-system nil)
  (set-face-background 'default "undefined"))

(defvar font-lock-brace-face
  (defface font-lock-brace-face
    '((((class color) (background light))
       (:foreground "yellow"))
      (((class color) (background dark))
       (:foreground "deep pink")))
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
    :ensure t
    :hook (after-init . session-initialize)))

(use-package magit
  :ensure t
  :defer
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))

(use-package mode-line-bell
  :ensure t
  :config
  (setq visible-bell nil
	ring-bell-function 'mode-line-bell-flash))

(use-package helm
  :commands helm-mode helm-autoresize-mode helm-projectile
  :config
  (require 'helm-config)
  (set-face-foreground 'helm-mark-prefix "Gold1")
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  :bind (("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
         ("C-x C-d" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("M-h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-mode
  :defer
  :diminish
  :after helm
  :hook ((after-init . helm-mode)
         (after-init . helm-autoresize-mode)))

(use-package helm-projectile
  :ensure t
  :after helm-mode
  :bind ("M-h p" . helm-projectile))

(use-package helm-ls-git
  :ensure t
  :after helm-projectile)

(use-package flymake
  :functions flymake--mode-line-format
  :config
  (setq elisp-flymake-byte-compile-load-path load-path
        flymake-no-changes-timeout 0.5
        flymake-start-on-flymake-mode t
        flymake-mode-line-title "FM"
        )
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map
         flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package diff-hl
  :commands diff-hl-flydiff-mode
  :init
  (diff-hl-flydiff-mode t)
  :hook (after-init . global-diff-hl-mode))

(eval-when-compile
  (require 'company))

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
  :ensure t
  :defer
  :custom
  (yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")
                          (expand-file-name "~/.emacs.d/elpa/yasnippet-snippets-1.0/snippets")))
  :hook (after-init . yas-global-mode))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :hook (after-init . yasnippet-snippets-initialize))

(use-package company
  :defines company-ispell-dictionary
  :config
  (setq company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-idle-delay 0.2
        company-show-numbers t
        company-ispell-dictionary "/usr/share/dict/web2"
        company-backends '(company-capf
                           company-keywords
                           company-semantic
                           company-files
                           company-etags
                           company-elisp
                           company-clang
                           company-dabbrev-code
                           company-cmake
                           company-ispell
                           company-yasnippet))
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

(use-package elec-pair
  :init
  (electric-pair-mode 1))

(use-package markdown-mode
  :defer
  :ensure t)

(use-package org-edna
  :config
  (setq org-edna-use-inheritance t)
  :hook (after-init . org-edna-mode))

(use-package org-gtd
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
  :defer
  :commands (eglot-ensure)
  :hook (c-mode-common . (lambda () (eglot-ensure)))
  :ensure t)

(use-package projectile
  :commands (projectile-mode)
  :defer
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :hook (prog-mode . projectile-mode))

(use-package cmake-mode)

(require 'flyspell-correct-helm)
(use-package flyspell-correct-helm
  :bind ("<f8>" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require 'diminish)
(diminish 'abbrev-mode "A")
(diminish 'company-mode "C")
(diminish 'eldoc-mode "D")
(diminish 'auto-fill-function "F")
(diminish 'subword-mode "S")
(diminish 'yas-minor-mode "Y")

(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
(put 'narrow-to-region 'disabled nil)

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

(defun my-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-info-other-frame ()
  "Show Info in a new frame."
  (interactive)
  (let ((tmp (get-buffer-create "*info*")))
    (set-buffer tmp)
    (select-frame (make-frame))
    (info nil tmp)))

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

(global-set-key [(control x)(?4)(k)] #'my-shell-other-window)
(global-set-key [(?%)] #'my-matching-paren)
(global-set-key [(control x)(?5)(i)] #'my-info-other-frame)
(global-set-key [(control x)(?5)(k)] #'my-shell-other-frame)
(global-set-key [(control c)(control k)] #'my-kill-buffer)
(global-set-key [(f3)] #'eval-last-sexp)
(global-set-key [(control meta ?\\)] #'my-indent-buffer)
(global-set-key [(home)] #'beginning-of-buffer)
(global-set-key [(end)] #'end-of-buffer)
(global-unset-key [(control z)])
(global-unset-key [(control x) h])

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

(use-package server
  :commands server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

;;; init.el ends here.

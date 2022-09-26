;;; package -- startup -*- Mode: Emacs-Lisp -*-
;;; Commentary:

(require 'use-package)

;;; Code:

(defconst is-macosx (string-equal "darwin" system-type))
(defconst is-windows (string-equal "windows-nt" system-type))
(defconst my-user (user-login-name))
(defconst my-python-command (expand-file-name "~/bin/python3"))
(defconst my-hostname
  (let ((v (or (getenv "HOSTNAME")
	       (when (shell-command "hostname" "*foo*")
		 (with-current-buffer "*foo*"
		   (goto-char (point-min))
		   (end-of-line 1)
		   (buffer-substring-no-properties (point-min) (point))))
	       "???")))
    (nth 0 (split-string v "[.]" t))))

(let ((config (concat "config-" my-hostname)))
   (load config t))

(setenv "WORKON_HOME" (expand-file-name "~/venvs"))

(use-package mode-line-bell
  :custom
  (mode-line-bell-flash-time 0.08)
  :config
  (setq visible-bell nil
        ring-bell-function 'mode-line-bell-flash))

(require 'company)
(defun my-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(use-package company
  :config
  (setq company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-idle-delay 0.2
        company-show-numbers t
        company-backends '(company-capf company-dabbrev-code company-keywords))
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)
              ("1" . my-company-number)
              ("2" . my-company-number)
              ("3" . my-company-number)
              ("4" . my-company-number)
              ("5" . my-company-number)
              ("6" . my-company-number)
              ("7" . my-company-number)
              ("8" . my-company-number)
              ("9" . my-company-number)
              ))

(use-package company-native-complete
  :config
  (add-to-list 'company-backends 'company-native-complete))

(use-package company-fuzzy
  :disabled
  :commands (global-company-fuzzy-mode)
  :config
  (progn (setq company-fuzzy-sorting-backend 'alphabetic
               company-fuzzy-prefix-on-top nil
               company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
         (global-company-fuzzy-mode 1))
  :hook (company-mode . company-fuzzy-mode))

(use-package magit
  :bind ("C-c g" . magit-file-dispatch))

(use-package wucuo
  :hook ((prog-mode . wucuo-start)
         (text-mode . wucuo-start)))

(unless (daemonp)
  (use-package session
    :hook (after-init . session-initialize))
  (save-place-mode 1)
  (savehist-mode 1))

(use-package helm
  :functions helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1)
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-apropos-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x c o" . helm-occur)
         ("C-x C-f" . helm-find-files)))

;; (use-package flycheck
;;   :functions global-flycheck-mode
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package flymake
  :config
  (setq elisp-flymake-byte-compile-load-path load-path
        flymake-no-changes-timeout 0.5
        flymake-start-on-flymake-mode t
        )
  :hook ((emacs-lisp-mode . flymake-mode)
         (python-mode . flymake-mode))
  :bind (:map
         flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; (add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(use-package eglot
  :ensure t)

(use-package elpy
  :ensure nil
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(when (not window-system)
  (require 'xterm-title)
  (if (fboundp 'xterm-title-mode)
      (xterm-title-mode 1)))

;; Use Emacs 'ls' routine, not native Mac OS X 'ls'
;;
;;(when is-macosx
;;  (require 'ls-lisp)
;;  (setq ls-lisp-use-insert-directory-program nil))

;; Overcome Tramp (SSH) issue where the value for ControlPath is too long due to TMPDIR being long on Mac OSX
;;
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
;; (setq explicit-zsh-args '("--login" "-i"))

;; Enable `narrow-to-region'. Invoke via C-X C-N N
;;
(put 'narrow-to-region 'disabled nil)

;; --- GLOBAL VARIABLE SETTINGS ---
;;
(setq-default fill-column 120
	      indent-tabs-mode nil
	      tab-width 8
	      save-place t
	      truncate-partial-width-windows nil)

(defconst my-project-name
  (or (getenv "PROJECT") "")
  "Name of the development environment this Emacs represents.")

(use-package diff-hl
  :commands (diff-hl-flydiff-mode global-diff-hl-mode)
  :init
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode t))

(use-package info)

(use-package smartparens-config
  :disabled
  :commands (show-smartparens-global-mode)
  :ensure smartparens
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  :config (progn (show-smartparens-global-mode t)))

(use-package projectile
  :commands (projectile-mode)
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-." . projectile-command-map)))

(setq frame-title-format
      (list (if (> (string-bytes my-project-name) 0)
		(concat my-project-name "-")
	      "") my-user "@" my-hostname ":"
              '(:eval (abbreviate-file-name default-directory)))

      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case")
      wucuo-flyspell-start-mode "normal"
      wucuo-spell-check-buffer-predicate nil ;; (lambda () (or (derived-mode-p prog-mode)
                                             ;;            (derived-mode-p text-mode)))

      split-width-threshold nil
      ;; copyright-query nil

      ;; Backup file control
      ;;
      make-backup-files t
      vc-make-backup-files t
      version-control t
      backup-directory-alist '(("." . ".~" ))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 10

      ;; compilation-scroll-output t

      process-coding-system-alist (cons '("bash" . raw-text-unix)
					process-coding-system-alist)

      require-final-newline 'ask
      next-line-add-newlines nil

      sentence-end-double-space nil
      sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"

      auto-save-list-file-prefix nil)

(auto-insert-mode 1)

;;  --- FACES ---
;;
(require 'font-lock)

(defvar font-lock-brace-face
  (defface font-lock-brace-face
    '((((class color) (background light))
       (:foreground "red"))
      (((class color) (background dark))
       (:foreground "deep pink")))
    "Font Lock mode face used to highlight parentheses, braces, and brackets."
    :group 'font-lock-faces))

;; --- FUNCTIONS ---
;;
(require 'my-insert-block-comment)

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

(defalias 'ib 'my-indent-buffer)

(defun bcf ()
  "Invoke `byte-compile-file' on the current buffer.
Current buffer should be visiting an Emacs Lisp file."
  (interactive)
  (byte-compile-file (buffer-file-name)))

(autoload 'js2-mode "js2-mode" "Major mode for editing Javascript files" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'csharp-mode "csharp-mode" nil t)
(autoload 'cmake-mode "cmake-mode" nil t)
(autoload 'web-mode "web-mode" nil t)

(setq auto-mode-alist (append '(("\\.mm$" . objc-mode)
				("\\.org\\'" . org-mode)
				("\\.plist\\'" . xml-mode)
                                ("\\.cs\\'" . csharp-mode)
                                ("\\.js\\'" . js2-mode)
                                ("\\.text\\'" . markdown-mode)
                                ("\\.markdown\\'" . markdown-mode)
                                ("\\.md\\'" . markdown-mode)
                                ("CMakeLists.txt" . cmake-mode)
				("\\.html\\'" . web-mode)
				)
			      auto-mode-alist))

(autoload 'auto-insert "autoinsert")

;; (add-hook 'find-file-hooks 'auto-insert)

(require 'autoinsert)
(setq auto-insert-alist
      '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
	 (let* ((namespace (read-string "Namespace: "))
		(bits (split-string namespace "::" t))
		(tag (upcase (concat (file-name-nondirectory
				      (file-name-sans-extension
				       (buffer-file-name)))
				     "_"
				     (file-name-nondirectory
				      (file-name-extension
				       (buffer-file-name)))))))
	   (unless (null bits)
	     (setq v1 (mapconcat
		       (function (lambda (x)
				   (format "namespace %s {\n" x))) bits "")
		   v2 (mapconcat
		       (function (lambda (x)
				   (format "} // end namespace %s\n" x)))
		       (reverse bits) "")
		   tag (concat
			(mapconcat (function (lambda (x) (upcase x))) bits "_")
			"_"
			tag)))
	   tag)
	 "#ifndef " str " // -*- C++ -*-\n"
	 "#define " str "\n\n"
	 "//\n"
	 "// (C) Copyright "
	 (format-time-string "%Y")
	 " Massachusetts Institute of Technology. "
	 "All rights\n"
	 "// reserved.\n"
	 "//\n\n"

	 v1 "\n"
	 _ "\n\n"

	 v2

	 "\n#endif\n"
	 '(c++-mode))

	(("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
	 nil
	 "//\n"
	 "// (C) Copyright "
	 (format-time-string "%Y")
	 " Massachusetts Institute of Technology. "
	 "All rights\n"
	 "// reserved.\n"
	 "//\n\n"

	 "#include \""
	 (file-name-nondirectory (file-name-sans-extension buffer-file-name))
	 ".h\"\n\n"
	 "using namespace SideCar::"
	 _
	 "\n\n")))

;; (add-hook 'write-file-functions (lambda () (copyright-update nil nil)))
(autoload 'my-python-mode-hook "my-python-mode")
(add-hook 'python-mode-hook 'my-python-mode-hook)
(autoload 'my-lisp-data-mode-hook "my-lisp-mode")
(add-hook 'lisp-data-mode-hook 'my-lisp-data-mode-hook)

(autoload 'my-lisp-mode-hook "my-lisp-mode")
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)
(add-hook 'scheme-mode-hook 'my-lisp-mode-hook)

(autoload 'my-sh-mode-hook "my-sh-mode")
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(autoload 'my-shell-mode-hook "my-shell-mode")
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(autoload 'my-c-mode-common "my-c-mode-common")
(add-hook 'c-mode-common-hook 'my-c-mode-common)
(autoload 'my-c-mode-hook "my-c-mode")
(add-hook 'c-mode-hook 'my-c-mode-hook)
(autoload 'my-c++-mode-hook "my-c++-mode")
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(autoload 'my-objc-mode-hook "my-objc-mode")
(add-hook 'objc-mode-hook 'my-objc-mode-hook)
(add-hook 'text-mode-hook 'auto-fill-mode)
(autoload 'my-makefile-mode-hook "my-makefile-mode")
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
(autoload 'my-nxml-mode-hook "my-nxml-mode")
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(autoload 'my-js2-mode-hook "my-js2-mode")
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(autoload 'my-cmake-mode-hook "my-cmake-mode")
(add-hook 'cmake-mode-hook 'my-cmake-mode-hook)
(autoload 'my-dired-mode-hook "my-dired-mode")
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(autoload 'my-markdown-mode-hook "my-markdown")
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
(autoload 'my-swift-mode-hook "my-swift-mode")
(add-hook 'swift-mode-hook 'my-swift-mode-hook)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun toggle-transparency ()
  "Toggle transparency of window and frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha
                         (if (eql (cond ((numberp alpha) alpha)
                                        ((numberp (car alpha)) (car alpha)))
                                  100)
                             '(90 . 90) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; --- KEYBOARD SETTINGS ---
;;
(global-set-key [(control x)(?4)(k)] 'my-shell-other-window)
(global-set-key [(?%)] 'my-matching-paren)
(global-set-key [(control x)(control j)] 'goto-line)
(global-set-key [(control x)(?5)(i)] 'my-info-other-frame)
(global-set-key [(control x)(?5)(k)] 'my-shell-other-frame)
(global-set-key [(control c)(control k)] 'my-kill-buffer)
(global-set-key [(f3)] 'eval-last-sexp)
(global-set-key [(control meta ?\\)] 'my-indent-buffer)
(global-set-key [(home)] 'beginning-of-buffer)
(global-set-key [(end)] 'end-of-buffer)
(global-unset-key [(control z)])
(global-set-key [delete] 'delete-char)
(global-set-key [(control meta right)] 'c-forward-into-nomenclature)
(global-set-key [(control meta left)] 'c-backward-into-nomenclature)
(global-set-key [(control c)(control f)] 'ff-find-other-file)
(global-set-key [(control x)(g)] 'magit-status)

(when is-macosx
  (define-key key-translation-map (kbd "<C-S-mouse-1>") (kbd "<mouse-2>")))

(display-time)

(use-package server
  :commands server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

;;; startup.el ends here.

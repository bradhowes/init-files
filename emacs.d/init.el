;;; init.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(setq debug-on-message "Unable to activate package 'lsp-mode'.")

;; Always work in the UTF-8 coding system.
(let ((coding-system 'utf-8))
  (set-charset-priority 'unicode)
  (prefer-coding-system coding-system)

  (dolist (v (list 'locale-coding-system
                   'coding-system-for-read
                   'coding-system-for-write))
    (set v coding-system))

  (dolist (p (list #'set-terminal-coding-system
                   #'set-keyboard-coding-system
                   #'set-selection-coding-system
                   #'prefer-coding-system))
    (funcall p coding-system)))

;; Replicating much of what is in user-lisp-autoloads.el in order to silence warnings from flymake. The file is loaded
;; upon startup but the elisp bytecompiler does not know about that when run in another Emacs process. I think flycheck
;; does the right thing here.
;;
(autoload 'my/is-graphical "my-env")
(autoload 'my/is-terminal "my-env")
(autoload 'my/user-lisp "my-env")
(autoload 'my/env-setup "my-env")
(autoload 'my/tmp-dir "my-env")
(autoload 'my/find-elpa-directory "my-finders")
(autoload 'my/find-shell-init-file "my-finders")
(autoload 'my/find-user-custom-file "my-finders")
(autoload 'my/find-user-init-file "my-finders")
(autoload 'my/find-user-lisp-file "my-finders")
(autoload 'my/copy-file-name-to-clipboard "my-functions")
(autoload 'my/describe-symbol-at-point "my-functions")
(autoload 'my/goto-mark "my-functions")
(autoload 'my/indent-buffer "my-functions")
(autoload 'my/matching-paren "my-functions")
(autoload 'my/reload-buffer "my-functions")
(autoload 'my/set-mark-deactivate "my-functions")
(autoload 'my/trusted-content-p "my-functions")
(autoload 'my/dired-jumps-bind "my-keymaps")
(autoload 'my/emacs-chord-bind "my-keymaps")
(autoload 'my/emacs-key-bind "my-keymaps")
(autoload 'my/emacs-make-key-bind "my-keymaps")
(autoload 'my/layout-frame-pos-left "my-layout")
(autoload 'my/layout-frame-pos-center "my-layout")
(autoload 'my/layout-frame-pos-right "my-layout")
(autoload 'my/layout-make-frame "my-layout")
(autoload 'my/layout-normal-screen-font-size "my-layout")
(autoload 'my/layout-screen-layout-changed "my-layout")
(autoload 'my/layout-share-screen-font-size "my-layout")
(autoload 'my/ace-window-always-dispatch "my-navigation")
(autoload 'my/ace-window-next "my-navigation")
(autoload 'my/ace-window-prefix "my-navigation")
(autoload 'my/ace-window-previous "my-navigation")
(autoload 'my/customize-other-window "my-navigation")
(autoload 'my/customize-search "my-navigation")
(autoload 'my/kill-current-buffer "my-navigation")
(autoload 'my/info-other-frame "my-navigation")
(autoload 'my/next-buffer-current-window "my-navigation")
(autoload 'my/prev-buffer-current-window "my-navigation")
(autoload 'my/show-messages-buffer "my-navigation")
(autoload 'my/show-messages-buffer-other-window "my-navigation")
(autoload 'my/notes-consult-notes-hook "my-notes")
(autoload 'my/notes-denote-hook "my-notes")
(autoload 'my/org-filter-buffer-substring "my-org")
(autoload 'my/show-project-menu "my-project")
(autoload 'my/start-emacs-server "my-server")
(autoload 'my/repl-other-window "my-shells")
(autoload 'my/shell "my-shells")
(autoload 'my/shell-other-window "my-shells")
(autoload 'my/shell-other-frame "my-shells")
(autoload 'ksh "my-shells")
(autoload 'my/htop "my-tops")

(my/env-setup)
(add-hook 'emacs-startup-hook #'my/layout-screen-layout-changed 98)
(add-hook 'emacs-startup-hook #'my/start-emacs-server 99)

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

;;;###autoload
(defvar my/dired-jumps-map
  (make-sparse-keymap)
  "Keymap for quick Dired jumps or file selection in directory.")

;;;###autoload
(defvar my/hyper-c-map
  (make-sparse-keymap)
  "Keymap for Hyper-c actions.")

;;;###autoload
(defvar my/hyper-n-map
  (make-sparse-keymap)
  "Keymap for Hyper-n actions.")

;; To keep this file small, we put all customizations in their own file.
(setq custom-file (file-name-concat (expand-file-name user-emacs-directory) "custom.el"))

(defalias 'repl 'my/repl
  "Legacy alias to start Elisp read/eval/print loop in current window.")

;; NOTE: for some reason, this may be breaking cape.
;; (advice-add 'trusted-content-p :filter-return #'my/trusted-content-p)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (use-package filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

(use-package package
  :defer nil ;; !!!
  :custom
  (package-archive-priorities '(("melpa" . 10)
                                ("melpa-stable" . 10)
                                ("gnu" . 15)
                                ("nongnu" . 20)))
  :config
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

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

(use-package accent
  :defer t
  :ensure t
  :bind (:map my/hyper-c-map ("a" . accent-menu)))

;;; ===== ace-window =====

(use-package ace-window
  :defer t
  :ensure t
  :config
  (setq aw-make-frame-char ?n)
  (advice-add 'aw-make-frame :override #'my/layout-make-frame)
  :commands (ace-window aw-flip-window))

(use-package bind-key
  :defer t
  :bind (:map help-map ("y" . describe-personal-keybindings)))

(defcustom my/use-c-ts-mode nil
  "Use `c-ts-mode' when t."
  :type '(boolean))

(autoload 'my/c++-mode-hook "my-c++-mode")
(let ((mode (if my/use-c-ts-mode 'c++-ts-mode 'c++-mode)))
  (add-to-list 'auto-mode-alist
               `(,(concat "\\.\\(cc\\|hh\\|ii\\|inl\\|mm\\|"
                          "\\([ch]\\(pp\\|xx\\|\\+\\+\\)\\)\\)\\'")
                 . ,mode)))

(if my/use-c-ts-mode
    (use-package c-ts-mode
      :defer t
      :hook ((c++-ts-mode . my/c++-mode-hook)
             (after-init . (lambda ()
                             (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))))))
  (use-package cc-mode
    :defer t
    :hook ((c++-mode . my/c++-mode-hook))))

(use-package char-menu
  :defer t
  :bind (("C-z" . char-menu))
  :custom
  (char-menu '("—" "‘’" "“”" "…" "«»" "–"
               ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
               ("Math" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
               ("Arrows" "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
               ("Greek" "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))))

(use-package compile
  :defer t
  :custom
  (compilation-error-regexp-alist (cons '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2) ; from pyright?
                                        compilation-error-regexp-alist)))

(use-package consult
  :defer t
  :ensure t
  :commands (consult--customize-put consult-flymake)
  :bind (:map ctl-x-map ;; C-x
              ("M-:" . consult-complex-command)
              ("b" . consult-buffer)
              ("f" . consult-recent-file)
              ("4 b" . consult-buffer-other-window)
              ("5 b" . consult-buffer-other-frame)
              ("r b" . consult-bookmark)
              ("r l" . consult-bookmark)

              ;; ("M-#" . consult-register-load)
              ;; ("M-'" . consult-register-store)
              ;; ("C-M-#" . consult-register)

              ("M-y" . consult-yank-replace)

              :map goto-map ;; M-g
              ("g" . consult-goto-line)
              ("M-g" . consult-goto-line)
              ("o" . consult-outline)
              ("m" . consult-mark)
              ("k" . consult-global-mark)
              ("i" . consult-imenu)
              ("I" . consult-imenu-multi)

              :map search-map ;; M-s
              ("d" . consult-find)
              ("g" . consult-grep)
              ("G" . consult-git-grep)
              ("i" . consult-imenu)      ; Duplicate 'M-g i'
              ("k" . consult-keep-lines)
              ("l" . consult-line)
              ("M-s" . my/consult-line-symbol-at-point)
              ("L" . consult-line-multi)
              ("r" . consult-ripgrep)
              ("u" . consult-focus-lines)
              ;; Isearch integration
              ("e" . consult-isearch-history)

              :map my/hyper-c-map ;; H-c
              ("M-x" . consult-mode-command)
              ("h" . consult-history)
              ;; ("C-h i" . consult-info)
              ("k" . consult-kmacro)
              ("m" . consult-man)
              ("I" . consult-info)

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

  :hook ((after-init . (lambda ()
                         ;; Tweak the register preview for `consult-register-load',
                         ;; `consult-register-store', and the built-in commands. This improves the
                         ;; register formatting, adds thin separate lines, register sorting and hides
                         ;; the window mode line.
                         (advice-add #'register-preview :override #'consult-register-window)
                         (setq register-preview-delay 0.5)

                         (defun my/consult-line-symbol-at-point ()
                           "Start `consult-line' with symbol at point."
                           (interactive)
                           (consult-line (thing-at-point 'symbol)))

                         (setq consult-narrow-key "<")
                         (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                                            consult-ripgrep consult-git-grep consult-grep consult-man
                                            consult-bookmark consult-recent-file consult-xref
                                            consult-source-bookmark consult-source-file-register
                                            consult-source-recent-file consult-source-project-recent-file
                                            :preview-key '(:debounce 0.4 any))))))

;; Use Consult to select xref locations with preview
(use-package xref
  :defer t
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package completion
  :defer t
  :hook ((after-init . dynamic-completion-mode)))

(use-package denote
  :defer t
  :ensure t
  :hook ((dired-mode . denote-dired-mode))
  :bind (:map my/hyper-n-map
              ("b" . denote-backlinks)
              ("c" . denote)
              ("d" . denote-dired)
              ("g" . denote-grep)
              ("l" . denote-link)
              ;; ("n" . consult-notes) -- done below
              ("r" . denote-rename-file)))

(use-package consult-notes
  :defer t
  :ensure t
  :custom
  (consult-notes-denote-display-id nil)
  :bind (:map my/hyper-n-map
         ("n" . consult-notes)
         :map my/hyper-c-map
         ("n" . consult-notes))
  :hook (after-init . my/notes-consult-notes-hook))

(use-package orderless
  :defer t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))
                                   (minibuffer (orderless initials basic))))
  (completion-category-defaults nil))

(use-package cape
  :defer t
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind (:map my/hyper-c-map
              ("p" . cape-prefix-map))
  :hook (after-init . (lambda ()
                        (add-hook 'completion-at-point-functions #'cape-dabbrev)
                        (add-hook 'completion-at-point-functions #'cape-file)
                        (add-hook 'completion-at-point-functions #'cape-elisp-block))))

(use-package corfu
  :defer t
  :ensure t
  :commands (global-corfu-mode corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Enable Corfu only for certain modes.
  :hook ((after-init . (lambda ()
                         (global-corfu-mode 1)
                         (corfu-popupinfo-mode 1)))))

(use-package crm)

(use-package crux
  :defer t
  :ensure t
  :commands (crux-find-current-directory-dir-locals-file)
  :bind (:map my/hyper-c-map
              ("d" . crux-duplicate-current-line-or-region)
              ("C-i" . crux-indent-defun)
              :map ctl-x-4-map
              ("t" . crux-transpose-windows)
              :map global-map
              ("C-a" . crux-move-beginning-of-line)
              ("C-k" . crux-smart-kill-line)
              ("C-^" . crux-top-join-line)))

(use-package dabbrev
  :defer t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package diff-hl
  :defer t
  :ensure t
  :commands (diff-hl-show-hunk diff-hl-flydiff-mode global-diff-hl-mode global-diff-hl-show-hunk-mouse-mode)
  :hook ((after-init . (lambda ()
                         (diff-hl-flydiff-mode t)
                         (global-diff-hl-mode t)
                         (global-diff-hl-show-hunk-mouse-mode t)))))

(autoload 'my/dired-mode-hook "my-dired-mode")
(use-package dired
  :defer t
  :hook (dired-mode . my/dired-mode-hook))

(autoload 'my/lisp-mode-hook "my-lisp-mode")
(autoload 'my/lisp-data-mode-hook "my-lisp-mode")
(use-package elisp-mode
  :defer t
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

;; (use-package eldoc-box
;; :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

(use-package emacs-pager
  :defer t
  :commands (emacs-pager emacs-pager-mode))

;; FYI: Embark's default action binding of "RET" fails if a mode binds to <return>.
(use-package embark
  :defer t
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :defer t
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package esup
;;   :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package exec-path-from-shell
  :defer t
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :defer t
  :ensure t
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

(use-package flymake
  :defer t
  :hook ((prog-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flyover
  :defer t
  :ensure t
  :hook ((flymake-mode . flyover-mode)
         (flyover-mode . my/flyover-mode-hook)))

(use-package helpful
  :defer t
  :ensure t
  :bind (:map help-map
              ("a" . helpful-symbol)
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("k" . helpful-key)
              ("." . helpful-at-point)
              ("M-f" . helpful-function)
              ("M-c" . helpful-command)))

(use-package hl-line
  :defer t
  :hook ((after-init . global-hl-line-mode)))

;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
(use-package ibuffer
  :defer t
  :config
  (keymap-unset ibuffer-mode-map "M-o" t))

(use-package indent-bars
  :defer t
  :ensure t
  :hook (prog-mode . indent-bars-mode))

(use-package iso-transl
  :defer t
  :bind-keymap ("H-8" . iso-transl-ctl-x-8-map)) ; Enter diacritics using "dead" keys after <H-8> or <C-X 8>

(use-package key-chord
  :defer t
  ;; :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define key-chord-mode)
  :config (key-chord-mode 1))

(use-package kkp
  :defer t
  :ensure t
  :if (my/is-terminal)
  :commands (global-kkp-mode)
  :hook (tty-setup . (lambda ()
                       ;; (setq kkp-alt-modifier 'alt) ; use to map the Alt keyboard modifier to Alt (and not to Meta)
                       ;; For C-g aborting blocking subprocesses, see "C-g and blocking subprocesses" in the README.
                       (setq kkp-restore-legacy-keys-around-subprocesses t)
                       (global-kkp-mode 1))))

(use-package ligature
  :defer t
  :ensure t
  :commands (ligature-set-ligatures global-ligature-mode)
  :hook ((after-init . (lambda ()
                         (ligature-set-ligatures
                          'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://" "www"))
                         (global-ligature-mode t)))))

(use-package magit
  :defer t
  :ensure t
  :commands (magit-status-setup-buffer magit-status magit-project-status)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (:map ctl-x-map
              ("g" . magit-status)
              ;; Take over vc-dir
              ("p v" . magit-project-status)
              ;; Take over vc-print-log
              ("v l" . magit-log-buffer-file)
              :map my/hyper-c-map
              ("f" . magit-file-dispatch))
  :custom
  (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package marginalia
  :defer t
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :bind (:map minibuffer-local-map
              ("<f1>" . marginalia-cycle))
  :hook (after-init . (lambda ()
                        (marginalia-mode t))))

(autoload 'my/markdown-ts-mode-hook "my-markdown-mode")
(autoload 'my/markdown-ts-setup-hook "my-markdown-mode")
(use-package markdown-ts-mode
  :defer t
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :hook ((markdown-ts-mode . my/markdown-ts-mode-hook))
         (after-init . my/markdown-ts-setup-hook))

(use-package mode-line-bell
  :defer t
  :ensure t)

(use-package keycast
  :defer t
  :ensure t
  :vc (:url "https://github.com/tarsius/keycast")
  :commands (keycast-invisible-mode)
  :custom
  (keycast-mode-line-format "%2s%k%c%r"))

(autoload 'my/mood-line-hook "my-mood-line")
;; (use-package mood-line
;;   :defer t
;;   :ensure t
;;   :vc (:url "https://gitlab.com/jessieh/mood-line")
;;   :hook (after-init . my/mood-line-hook))

(use-package multiple-cursors
  :defer t
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         :map my/hyper-c-map
         ("." . mc/mark-all-like-this)))

(use-package my-fontify-braces
  :defer t)

(use-package nerd-icons
  :defer t
  :ensure t)

(use-package nerd-icons-completion
  :defer t
  :ensure t
  :commands (nerd-icons-completion-mode)
  :hook ((after-init . (lambda () (nerd-icons-completion-mode 1)))))

(use-package nerd-icons-corfu
  :defer t
  :ensure t
  :commands (nerd-icons-corfu-formatter)
  :hook (after-init . (lambda ()
                        (require 'corfu)
                        (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

(use-package nerd-icons-dired
  :defer t
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-xref
  :defer t
  :ensure t
  :hook (after-init . nerd-icons-xref-mode))

(use-package orderless
  :defer t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package org
  :defer t
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
  :defer t
  :ensure t
  :if my/is-macosx
  :bind (:map my/hyper-c-map ("l" . osx-dictionary-search-pointer)))

(use-package popper
  :defer t
  :ensure t
  :commands (popper-kill-latest-popup popper-mode popper-echo-mode)
  :functions (popper--delete-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle))
  :hook ((after-init . (lambda ()
                         (popper-mode)
                         (popper-echo-mode)))))

(defvar my/project-search-map (make-sparse-keymap)
  "A prefix map like that found in projectile.
Bound to \\`C-x p s'.")

;; Need to do this here due to how `use-package(project)` works:
(keymap-set project-prefix-map "s" my/project-search-map)

(use-package project
  :defer t
  :commands (project--switch-project-command) ;; used in my/show-project-menu
  :bind (:map project-prefix-map
              ("$" . #'project-shell)
              ("m" . #'my/show-project-menu)
              ;; Cannot do this here for some reason -- see note above.
              ;; ("s" . my/project-search-map)))
              ))

(use-package recentf
  :defer t
  :hook ((after-init . recentf-mode)))

(use-package rg
  :defer t
  :ensure t
  :commands (rg-enable-default-bindings rg-project)
  :bind (:map my/project-search-map
              ("r" . rg-project))
  :hook (after-init . rg-enable-default-bindings))

(use-package savehist
  :defer t
  :hook (after-init . savehist-mode))

(use-package saveplace
  :defer t
  :hook ((after-init . save-place-mode)))

(use-package scratch
  :defer t
  :ensure t
  :bind (:map my/hyper-c-map ("s" . scratch)))

(autoload 'my/sh-mode-hook "my-sh-mode")
(use-package sh-script
  :defer t
  :hook (sh-mode . my/sh-mode-hook))

(defalias 'ksh #'shell
  "Alias to satisfy habit from AIX days.")

(autoload 'my/shell-mode-hook "my-shell-mode")
(use-package shell
  :defer t
  :custom
  (explicit-bash-args '("--noediting" "-i"))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package subword
  :defer t
  :hook ((after-init . global-subword-mode)))

(use-package ultra-scroll
  :defer t
  :ensure t
  :commands (ultra-scroll-mode)
  :hook ((after-init . (lambda ()
                         (setq scroll-conservatively 3
                               scroll-margin 0)
                         (ultra-scroll-mode 1)))))

(use-package vertico
  :defer t
  :ensure t
  :commands (vertico-mode)
  :hook ((after-init . vertico-mode)))

(use-package which-key
  :defer t
  :ensure t
  :hook (after-init . which-key-mode))

(use-package whitespace
  :defer t
  :hook ((after-init . (lambda () (global-whitespace-mode t)))
         (prog-mode . (lambda () (add-hook 'before-save-hook #'whitespace-cleanup)))))

(use-package winner
  :defer t
  :hook (after-init . winner-mode)
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         :map my/hyper-c-map
         ("u" . winner-undo)
         ("C-u" . winner-undo)
         ("C-r" . winner-redo)))

(use-package xclip
  :defer t
  :ensure t
  :commands (xclip-mode)
  :hook (tty-setup . (lambda () (xclip-mode 1))))

(use-package yasnippet
  :defer t
  :ensure t)

(use-package yasnippet-snippets
  :defer t
  :ensure t)

(use-package zoxide
  :defer t
  :ensure t)

(use-package consult-zoxide
  :defer t
  :ensure t)

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
  (setq read-process-output-max (* 64 1024 1024)
	process-adaptive-read-buffering nil
        ;; debug-on-error t
	frame-title-format (let ((buffer-directory '(:eval (abbreviate-file-name default-directory))))
                             (if (not (display-graphic-p))
                                 (list (concat (system-name) " ") buffer-directory)
                               buffer-directory))
        load-path-filter-function #'load-path-filter-cache-directory-files)
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
         (after-init . (lambda ()
                         (abbrev-mode)
                         (when (file-exists-p custom-file)
                           (load custom-file 'noerror))))))

(use-package window
  :init
  (setq switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t
        window-resize-pixelwise t
        window-sides-slots '(0 0 3 1)
        display-buffer-base-action '((display-buffer-reuse-window ace-display-buffer))
        display-buffer-alist `(("\\*help\\[R" (display-buffer-reuse-mode-window ace-display-buffer) (reusable-frames . nil))
                               ("\\*R" nil (reusable-frames . nil))
                               ,(cons "\\*helm" display-buffer-fallback-action))))
;; Show log buffer in something other than the current window
;; ("magit-log" nil (inhibit-same-window . t))
;; ("magit-diff:" nil (inhibit-same-window . t))))))

;;; --- Key Bindings
(my/emacs-key-bind my/hyper-c-map
                   "i" #'my/find-user-init-file
                   "j" my/dired-jumps-map
                   "r" #'ielm
                   "D" #'crux-find-current-directory-dir-locals-file
                   "S" #'my/find-shell-init-file
                   "H-l" #'my/find-user-init-file
                   "," #'my/find-user-custom-file
                   "H-c" #'my/copy-file-name-to-clipboard
                   "H-k" #'my/kill-current-buffer)

(my/emacs-key-bind help-map
                   "C-h" #'my/describe-symbol-at-point
                   "C-j" #'popper-toggle
                   "a" #'describe-symbol
                   "c" #'describe-char
                   "h" #'ignore         ; show 'Hello' in various fonts
                   "u" #'apropos-user-option
                   "F" #'apropos-function
                   "K" #'describe-keymap
                   "L" #'apropos-library
                   "M" #'consult-man
                   "V" #'apropos-variable)

(my/emacs-key-bind ctl-x-map
                   "C-b" #'ibuffer
                   "C-o" #'other-frame
                   "C-z" #'ignore       ; suspend-frame
                   "C-+" #'ignore       ; text-scale-adjust
                   "C-=" #'ignore       ; text-scale-adjust
                   "C--" #'ignore       ; text-scale-adjust
                   "0" #'delete-other-windows
                   "O" #'other-frame
                   "M-b" #'consult-project-buffer
                   "M-v" #'my/reload-buffer
                   "h" #'ignore)        ; mark-whole-buffer

(my/emacs-key-bind ctl-x-4-map
                   "c" #'my/customize-other-window
                   "k" #'my/shell-other-window
                   "o" #'my/ace-window-prefix
                   "r" #'my/repl-other-window)

(my/emacs-key-bind ctl-x-5-map
                   "i" #'my/info-other-frame
                   "k" #'my/shell-other-frame)

(my/emacs-key-bind global-map
                   "S-<left>" #'my/ace-window-previous
                   "S-<right>" #'my/ace-window-next
                   "S-<up>" #'my/ace-window-previous
                   "S-<down>" #'my/ace-window-next

                   "M-g d" #'dired-jump
                   "M-o" #'other-window
                   ;; "M-O" #'my/ace-window-always-dispatch

                   "C-o" #'aw-flip-window

                   ;; "C-s" #'isearch-forward-regexp
                   ;; "C-M-s" #'isearch-forward-symbol

                   "M-{" #'my/prev-buffer-current-window
                   "M-}" #'my/next-buffer-current-window

                   "M-<f1>" #'my/layout-frame-pos-left
                   "M-<f2>" #'my/layout-frame-pos-center
                   "M-<f3>" #'my/layout-frame-pos-right

                   "C-M-\\" #'my/indent-buffer

                   "<home>" #'beginning-of-buffer
                   "<end>" #'end-of-buffer
                   "<delete>" #'delete-char
                   "S-<f12>" #'package-list-packages
                   "S-<f11>" #'my/layout-screen-layout-changed

                   "M-z" #'zap-up-to-char
                   "M-_" #'join-line

                   "M-P" #'my/ace-window-previous
                   "M-N" #'my/ace-window-next

                   "C-S-p" #'my/ace-window-previous
                   "C-S-n" #'my/ace-window-next

                   "<f1>" #'my/layout-normal-screen-font-size
                   "<f2>" #'my/layout-share-screen-font-size

                   "<insert>" #'ignore  ; disable key for toggling overwrite-mode
                   "<insertchar>" #'ignore  ; disable key for toggling overwrite-mode

                   "<pinch>" #'ignore

                   ;; Disable font size changes via trackpad/scroll-wheel
                   "C-<mouse-4>" #'ignore
                   "C-<mouse-5>" #'ignore
                   "C-<wheel-up>" #'ignore
                   "C-<wheel-down>" #'ignore

                   "C-M-<mouse-4>" #'ignore
                   "C-M-<mouse-5>" #'ignore
                   "C-M-<wheel-up>" #'ignore
                   "C-M-<wheel-down>" #'ignore)

(my/dired-jumps-bind my/dired-jumps-map
                     ;; Collection of 3-tuples that define a directory to jump to:
                     ;; 1 - key to use
                     ;; 2 - the directory to jump to (if not absolute then prepend with value from `my/repos')
                     ;; 3 - the name to assign to the utility function (if nil make from directory)
                     `(("a" "auv3-support" nil)
                       ("c" "AUv3Controls" nil)
                       ("i" "init-files" nil)
                       ("e" "init-files/emacs.d" "emacs.d")
                       ("E" ,(expand-file-name user-emacs-directory) "~.emacs.d")
                       ("l" my/find-elpa-directory nil)
                       ("L" ,(file-name-concat user-emacs-directory "elpa") "elpa")
                       ("p" "SoundFontsPlus" nil)
                       ("s" "AUv3Support" nil)
                       ("u" my/find-user-lisp-file nil)
                       ("U" ,(my/user-lisp) "user-lisp")
                       ("z" "init-files/shells" "shells")
                       ("2" "SF2Lib" nil)))

(defvar my/point-jumps-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'consult-register-store)
    (define-key map "j" #'consult-register-load)
    map)
  "Keymap for quick Dired jumps.")

(when (my/is-graphical)
  (my/emacs-key-bind global-map
                     ;; NOTE: these conflict with terminal escape sequences so only use on graphical displays
                     "M-O" #'my/ace-window-always-dispatch
                     "M-[" #'previous-buffer
                     "M-]" #'next-buffer))

(defvar my/hyper-keys-map
  (make-sparse-keymap)
  "Keymap for terminal hyper actions.")

;; Populate two key maps with hyper-key definitions. The first -- global -- holds the mapping that uses the real `Hyper'
;; modifier. The second keymap -- `my/hyper-keys-map` -- holds the mapping that uses a keychord to activate which is
;; useful on terminals that do not offer a `Hyper' modifier.
(let ((hyper-mappings (list "H-SPC" #'my/set-mark-deactivate
                            "H-." #'my/goto-mark
                            "H-1" #'delete-other-windows
                            "H-2" #'split-window-below
                            "H-4" #'other-window-prefix ; was ctl-x-4-prefix
                            "H-5" #'other-frame-prefix  ; was ctl-x-5-prefix
                            "H-a" #'my/ace-window-always-dispatch
                            "H-b" #'consult-buffer
                            "H-B" #'consult-project-buffer
                            "H-c" my/hyper-c-map
                            "H-f" #'consult-flymake
                            "H-g" #'magit-status
                            "H-h" #'my/describe-symbol-at-point
                            "H-j" my/point-jumps-map
                            "H-k" #'bury-buffer
                            "H-K" #'my/shell
                            "H-m" #'consult-bookmark
                            "H-M-m" #'my/show-messages-buffer
                            "H-n" my/hyper-n-map
                            "H-p" project-prefix-map
                            "H-r" #'speedbar
                            "H-s" #'my/shell
                            "H-t" #'my/htop
                            "H-u" #'undo
                            "H-v" #'my/reload-buffer
                            "H-w" #'my/ace-window-prefix
                            "H-z" #'my/shell
                            "H-," #'my/customize-search
                            "H-<f1>" #'keycast-invisible-mode
                            "H-;" #'my/matching-paren)))
  (apply #'my/emacs-key-bind global-map hyper-mappings)
  (apply #'my/emacs-make-key-bind my/hyper-keys-map (lambda (key) (substring key 2)) hyper-mappings))

;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy to
;; type with one or two hands. Not so sure about how useful this is -- I keep encountering issues.
(my/emacs-chord-bind global-map
                     "aa" #'my/ace-window-always-dispatch
                     "hh" my/hyper-keys-map
                     "HH" #'helpful-at-point
                     "hb" #'popper-kill-latest-popup
                     "jn" #'my/ace-window-next
                     "jp" #'my/ace-window-previous
                     "qq" #'undo
                     "sb" #'speedbar
                     "vv" #'diff-hl-show-hunk
                     )

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

(when (eq system-type 'gnu/linux)
  (set-face-background 'default "undefined"))

(when (eq system-type 'darwin)
  (custom-set-variables
   '(insert-directory-program "gls"))
  (when (display-graphic-p)
    (custom-set-variables
     '(frame-resize-pixelwise t))))

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097 Basically, put backup and autosave files in their
;; own directories inside our own `~/tmp' directory.
(let ((backup-dir (file-name-concat (my/tmp-dir) "emacs_backups"))
      (auto-saves-dir (file-name-concat (my/tmp-dir) "emacs_autosaves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (custom-set-variables
   `(backup-directory-alist '(("." . ,backup-dir)))
   `(auto-save-file-name-transforms '((".*" ,auto-saves-dir t)))
   ;; Tramp as well but note slight change in pattern
   `(tramp-backup-directory-alist '((".*" . ,backup-dir)))
   `(tramp-auto-save-directory ,auto-saves-dir))
  (setq auto-save-list-file-prefix (file-name-concat auto-saves-dir ".saves-")))

(define-skeleton add-message-field
  "Blah."
  "Field name: "
  "<field name='" str "' required='N' />\n")

(define-skeleton add-field-definition
  "Blah."
  "Field name: "
  > str " = \"" _ "\"\n")

(defun my/display-startup-time ()
  "Show the elapsed startup time."
  (message "Elapsed load time: %s" (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

(provide 'init)

;;; init.el ends here.

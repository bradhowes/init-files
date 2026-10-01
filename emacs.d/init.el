;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

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

;; Replicating much of what is in user-lisp-autoloads.el in order to silence warnings from flymake.
;; The .user-lisp-autoloads.el file is loaded upon startup.
;;
;; (autoload 'my/is-terminal "my-env")
(autoload 'my/is-graphical "my-env")
(autoload 'my/repos "my-env")
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
(autoload 'my/layout-frame-pos-left "my-layout")
(autoload 'my/layout-frame-pos-center "my-layout")
(autoload 'my/layout-frame-pos-right "my-layout")
(autoload 'my/layout-make-frame "my-layout")
(autoload 'my/layout-normal-screen-font-size "my-layout")
(autoload 'my/layout-screen-layout-changed "my-layout")
(autoload 'my/layout-share-screen-font-size "my-layout")
(autoload 'my/org-filter-buffer-substring "my-org")
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

(require 'my-keymaps)
;; (require 'my-modes)

;; To keep this file small, we put all customizations in their own file.
;; But then we need to load it ourselves.
(setq custom-file (file-name-concat (expand-file-name user-emacs-directory) "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(defalias 'ksh 'my/shell
  "Legacy alias to start shell in current window.")

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
  :defer nil
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
  :bind (:map my/hyper-c-map ("a" . accent-menu)))

;;; ===== ace-window =====

(use-package ace-window
  :defer t
  :config
  (setq aw-make-frame-char ?n)
  (advice-add 'aw-make-frame :override #'my/layout-make-frame)
  :commands (ace-window aw-flip-window))

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
  :config
  (push '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2) ; I think this is from pyright
        compilation-error-regexp-alist))

(use-package consult
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

  ;; The :init configuration is always executed (not lazy).
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store', and the built-in commands. This improves the
  ;; register formatting, adds thin separate lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

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
                     consult-source-bookmark consult-source-file-register
                     consult-source-recent-file consult-source-project-recent-file
                     :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  :custom (consult-narrow-key "<"))

;; Use Consult to select xref locations with preview
(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(defun my/denote-format-keywords-for-md-front-matter (keywords)
  "Custom KEYWORDS formatter for keystrokecountdown.com markdown files.
The default Markdown keyword formatter puts each keyword in double-quotes,
separates them with a \", \" and surrounds the result with square brackets.
Here, we just separate them by a comma."
  (format "%s" (mapconcat (lambda (k) k) keywords ", ")))

(use-package denote
  :defer t
  :commands (denote-dired-mode-in-directories)
  :hook ((dired-mode . denote-dired-mode)
         (after-init . (lambda ()
                         (push '(markdown-brh
                                 :extension ".md"
                                 :date-function (lambda (date) (format-time-string "%F %T"))
                                 :front-matter denote-yaml-front-matter
                                 :title-key-regexp "^title\\s-*:"
                                 :title-value-function denote-trim-whitespace
                                 :title-value-reverse-function denote-trim-whitespace
                                 :keywords-key-regexp "^tags\\s-*:"
                                 :keywords-value-function my/denote-format-keywords-for-md-front-matter
                                 :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                                 :link denote-md-link-format
                                 :link-in-context-regexp denote-md-link-in-context-regexp)
                               denote-file-types))))
  :bind (:map my/hyper-n-map
              ("b" . denote-backlinks)
              ("c" . denote)
              ("d" . denote-dired)
              ("g" . denote-grep)
              ("l" . denote-link)
              ;; ("n" . consult-notes)
              ("r" . denote-rename-file))
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-file-type 'markdown-brh)
  (denote-rename-buffer-mode 1)
  (denote-sort-keywords t))

(use-package completion
  :defer t
  :hook ((after-init . dynamic-completion-mode)))

(use-package consult-notes
  :defer t
  :after (consult denote)
  :commands (consult-notes-denote-mode denote-directory-files)
  :bind (:map my/hyper-n-map ("n" . consult-notes))
  :hook ((after-init . consult-notes-denote-mode)))

(use-package corfu
  :after orderless
  :commands (global-corfu-mode corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (after-init . (lambda ()
                         (global-corfu-mode)
                         (corfu-popupinfo-mode)))))

(use-package crm)

(use-package crux
  :commands (crux-find-current-directory-dir-locals-file)
  :defer nil                            ; load now due to dependencies below
  :bind (:map my/hyper-c-map
              ("d" . crux-duplicate-current-line-or-region)
              ("C-i" . crux-indent-defun)
              :map ctl-x-4-map
              ("t" . crux-transpose-windows)
              :map global-map
              ("C-a" . crux-move-beginning-of-line)
              ("C-k" . crux-smart-kill-line)
              ("C-^" . crux-top-join-line)))

(use-package dired
  :hook (dired-mode . my/dired-mode-hook))

(autoload 'my/lisp-mode-hook "my-lisp-mode")
(autoload 'my/lisp-data-mode-hook "my-lisp-mode")

(use-package elisp-mode
  :hook ((lisp-mode . my/lisp-mode-hook)
         (lisp-interaction-mode . my/lisp-mode-hook)
         (lisp-data-mode . my/lisp-data-mode-hook)
         (scheme-mode . my/lisp-mode-hook)
         (emacs-lisp-mode . my/lisp-mode-hook)))

;; (use-package eldoc-box
;; :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

(use-package emacs-pager
  :commands (emacs-pager emacs-pager-mode))

;; FYI: Embark's default action binding of "RET" fails if a mode binds to <return>.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package esup
;;   :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flyover
  :hook ((flymake-mode . flyover-mode))
  :custom
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)

  ;; Icons
  ;; (flyover-info-icon " ")
  ;; (flyover-warning-icon " ")
  ;; (flyover-error-icon " ")

  ;; Display settings
  (flyover-display-mode 'hide-on-same-line)
  (flyover-max-line-length 120))

(use-package helpful
  :bind (:map help-map
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("k" . helpful-key)
              ("." . helpful-at-point)
              ("M-f" . helpful-function)
              ("M-c" . helpful-command)))

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
(use-package ibuffer
  :config (keymap-unset ibuffer-mode-map "M-o" t))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))

(use-package iso-transl
  :bind-keymap ("H-8" . iso-transl-ctl-x-8-map)) ; Enter diacritics using "dead" keys after <H-8> or <C-X 8>

(use-package key-chord
  ;; :vc (:url "https://github.com/emacsorphanage/key-chord" :rev :newest)
  :commands (key-chord-define key-chord-mode)
  :config (key-chord-mode 1))

(use-package ligature
  :commands (ligature-set-ligatures global-ligature-mode)
  :config
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
  (global-ligature-mode t))

(use-package magit
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
  :custom (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package marginalia-mode
  :defer t
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package markdown-mode
  :hook (markdown-mode . my/markdown-mode-hook))

(use-package mode-line-bell)

(use-package mood-line
  :commands (mood-line-mode)
  :custom
  (mood-line-format mood-line-format-default)
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  :hook (after-init . (lambda () (mood-line-mode t))))

(use-package keycast)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         :map my/hyper-c-map
         ("." . mc/mark-all-like-this)))

(use-package my-fontify-braces)

(use-package nerd-icons-completion
  :after (marginalia)
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :hook ((after-init . nerd-icons-completion-mode)
         (marginalia-mode nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package orderless
  :custom
  (completion-styles '(partial-completion orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (minibuffer (initials orderless)))))

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
  :bind (:map my/hyper-c-map ("l" . osx-dictionary-search-pointer)))

(use-package popper
  :defer t
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
  :commands (project--switch-project-command) ;; used in my/show-project-menu
  :bind (:map project-prefix-map
              ("$" . #'project-shell)
              ("m" . #'my/show-project-menu)
              ;; Cannot do this here for some reason -- see note above.
              ;; ("s" . my/project-search-map)))
              ))

(use-package rg
  :after (project)
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
  :bind (:map my/hyper-c-map ("s" . scratch)))

(autoload 'my/sh-mode-hook "my-sh-mode")
(use-package sh-script
  :hook (sh-mode . my/sh-mode-hook))

(autoload 'my/shell-mode-hook "my-shell-mode")
(use-package shell
  :custom
  (explicit-bash-args '("--noediting" "-i"))
  :hook ((shell-mode . my/shell-mode-hook)))

(use-package subword
  :defer t
  :hook ((after-init . global-subword-mode)))

(use-package vertico
  :defer t
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
         (after-init . vertico-mode)))

(use-package which-key
  :defer t
  :hook (after-init . which-key-mode))

(use-package whitespace
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

(use-package yasnippet)
(use-package yasnippet-snippets)

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
         (after-init . abbrev-mode)))

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
                            "H-b" #'consult-project-buffer
                            "H-B" #'consult-buffer
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
                            "H-;" #'my/matching-paren)))
  (apply #'my/emacs-key-bind global-map hyper-mappings)
  (apply #'my/emacs-make-key-bind my/hyper-keys-map (lambda (key) (substring key 2)) hyper-mappings))

;;; --- Key Chords

(use-package diff-hl
  :defer t
  :commands (diff-hl-show-hunk diff-hl-flydiff-mode global-diff-hl-mode global-diff-hl-show-hunk-mouse-mode)
  :hook ((after-init . (lambda ()
                         (diff-hl-flydiff-mode t)
                         (global-diff-hl-mode t)
                         (global-diff-hl-show-hunk-mouse-mode t)))))

(use-package recentf
  :hook ((after-init . recentf-mode)))

;; Rationale: pick character combinations that do not match sequences in English or programming, and that are easy to
;; type with one or two hands.
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
                     ;; "fm" #'flymake-show-buffer-diagnostics
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

(when (and (tty-type)
           my/is-linux)
  (set-face-background 'default "undefined"))

(when my/is-macosx
  (custom-set-variables
   '(insert-directory-program "gls"))
  (when (display-graphic-p)
    (custom-set-variables
     '(frame-resize-pixelwise t))))

;; Backup strategy - from https://emacs.stackexchange.com/a/36/17097
;; Basically, put backup and autosave files in their own directories
;; inside our own `~/tmp' directory.
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

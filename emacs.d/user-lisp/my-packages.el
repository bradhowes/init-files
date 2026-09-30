;;; my-packages.el --- packages -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-keymaps)
(require 'my-layout)
(require 'my-project)

;; (autoload 'my/layout-make-frame "my-layout")

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

(use-package package
  :custom
  (package-archive-priorities '(("melpa" . 10)
                                ("melpa-stable" . 10)
                                ("gnu" . 15)
                                ("nongnu" . 20)))
  :config
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(use-package accent
  :defer t
  :bind (:map my/hyper-c-map ("a" . accent-menu)))

;;; ===== ace-window =====

(use-package ace-window
  :commands (ace-window aw-flip-window)
  :defines (aw-dispatch-always)
  :config
  (setq aw-make-frame-char ?n)
  (advice-add 'aw-make-frame :override #'my/layout-make-frame))

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
  ;; :after (project xref)
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
                     consult-source-bookmark consult-source-file-register
                     consult-source-recent-file consult-source-project-recent-file
                     :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  :custom (consult-narrow-key "<"))

(use-package denote
  :commands (denote-dired-mode-in-directories)
  :hook (dired-mode . denote-dired-mode)
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
  (denote-sort-keywords t)

  :config
  (defun my/denote-format-keywords-for-md-front-matter (keywords)
    "Custom KEYWORDS formatter for keystrokecountdown.com markdown files.
The default Markdown keyword formatter puts each keyword in double-quotes,
separates them with a \", \" and surrounds the result with square brackets.
Here, we just separate them by a comma."
    (format "%s" (mapconcat (lambda (k) k) keywords ", ")))

  (setq denote-file-types (cons
                           '(markdown-brh
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
                           denote-file-types)
        denote-file-type 'markdown-brh))

(use-package consult-notes
  :after (consult denote)
  ;; :config
  ;; (require 'consult-notes-denote)
  :bind (:map my/hyper-n-map ("n" . consult-notes)))

(use-package corfu
  :after orderless
  :commands (global-corfu-mode)
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
         (eshell-mode . corfu-mode))
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode))

(use-package crm)

(use-package crux
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

(use-package diff-hl
  :commands (diff-hl-show-hunk))

(use-package dired
  :hook (dired-mode . my/dired-mode-hook))

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
  :config
  (setq esup-depth 0)
  (push (file-truename "~/.emacs.d/user-lisp") load-path)
  :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

;; (use-package eldoc-box
;;   :ensure t)
;; :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

;; (use-package flyover
;;   :hook ((flymake-mode . flyover-mode))
;;   :custom
;;   ;; Appearance
;;   (flyover-background-lightness 45)
;;   (flyover-percent-darker 40)

;;   ;; Icons
;;   ;; (flyover-info-icon " ")
;;   ;; (flyover-warning-icon " ")
;;   ;; (flyover-error-icon " ")

;;   ;; Display settings
;;   (flyover-display-mode 'hide-on-same-line)
;;   (flyover-max-line-length 120))

(use-package helpful
  :bind (:map help-map
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("k" . helpful-key)
              ("." . helpful-at-point)
              ("M-f" . helpful-function)
              ("M-c" . helpful-command)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-line)

;; Unbind the ibuffer use of "M-o" so as not to conflict with my global definition using `ace-window'
(use-package ibuffer
  :config (keymap-unset ibuffer-mode-map "M-o" t))

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

(use-package marginalia
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package mode-line-bell)

(use-package keycast)

(use-package mood-line
  :hook (after-init . mood-line-mode))

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
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)))

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
  :defer nil
  :commands (rg-enable-default-bindings rg-project)
  :bind (:map my/project-search-map
              ("r" . rg-project))
  :hook (after-init . rg-enable-default-bindings))

(use-package scratch
  :bind (:map my/hyper-c-map ("s" . scratch)))

(use-package vertico
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package which-key)

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
(use-package winner
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         :map my/hyper-c-map
         ("u" . winner-undo)
         ("C-u" . winner-undo)
         ("C-r" . winner-redo)))

(use-package yasnippet)
(use-package yasnippet-snippets)

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

(provide 'my-packages)

;;; my-packages.el ends here.

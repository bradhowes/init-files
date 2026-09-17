;;; init.el --- load the full configuration -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--
;;; Commentary:
;;; Code:

(require 'flymake)
(require 'seq)
(require 'my-constants)
(require 'my-customizations)
(require 'my-env)
(require 'my-functions)
(require 'my-layout)
(require 'my-modes)
(require 'wid-edit)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(autoload 'my/trusted-content-p "my-functions")
(autoload 'my/ace-window-always-dispatch "my-functions")
(autoload 'my/ace-window-next "my-functions")
(autoload 'my/ace-window-previous "my-functions")
(autoload 'my/next-buffer-current-window "my-functions")
(autoload 'my/prev-buffer-current-window "my-functions")
(autoload 'my/ace-window-one-command "my-functions")
(autoload 'my/ace-window-prefix "my-functions")
(autoload 'crux-find-current-directory-dir-locals-file "my-functions")
(autoload 'my/show-project-menu "my-functions")
(autoload 'my/find-user-init-file "my-functions")
(autoload 'my/find-shell-init-file "my-functions")
(autoload 'my/kill-current-buffer "my-functions")
(autoload 'my/dump-hashtable "my-functions")
(autoload 'my/reload-buffer "my-functions")
(autoload 'my/run-something-in-buffer "my-functions")
(autoload 'my/run-shell "my-functions")
(autoload 'my/shell "my-functions")
(autoload 'my/shell-other-window "my-functions")
(autoload 'my/shell-other-frame "my-functions")
(autoload 'my/bury-or-kill-current-buffer "my-functions")
(autoload 'my/bury-current-buffer "my-functions")
(autoload 'my/kill-current-buffer "my-functions")
(autoload 'my/info-other-frame "my-functions")
(autoload 'my/customize-other-window "my-functions")
(autoload 'my/consult-notes-other-frame "my-functions")
(autoload 'my/remove-all-text-properties "my-functions")
(autoload 'my/matching-paren "my-functions")
(autoload 'my/indent-buffer "my-functions")
(autoload 'my/copy-file-name-to-clipboard "my-functions")
(autoload 'my/repl "my-functions")
(autoload 'my/repl-other-window "my-functions")
(autoload 'my/describe-symbol-at-point "my-functions")
(autoload 'my/htop "my-functions")
(autoload 'my/top "my-functions")
(autoload 'my/set-mark-deactivate "my-functions")
(autoload 'my/goto-mark "my-functions")
(autoload 'my/customize-search "my-functions")

(defalias 'ksh 'my/shell
  "Legacy alias to start shell in current window.")

(defalias 'repl 'my/repl
  "Legacy alias to start Elisp read/eval/print loop in current window.")

;; NOTE: for some reason, this is breaking cape.
;; (advice-add 'trusted-content-p :filter-return #'my/trusted-content-p)

;; Set this to `t` to debug issue involving the filenotify package
(when nil
  (require 'filenotify)
  (setq file-notify-debug nil))
;; (debug-on-entry 'file-notify-add-watch)

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

(defvar my/hyper-c-map
  (make-sparse-keymap)
  "Keymap for Hyper-c actions.")

(use-package accent
  :ensure t
  :bind (:map my/hyper-c-map ("a" . accent-menu)))

;;; ===== ace-window =====

(use-package ace-window
  :ensure t
  :commands (aw-window-list aw-switch-to-window aw-select aw-flip-window ace-display-buffer ace-window)
  :defines (aw-dispatch-always)
  :config (setq aw-make-frame-char ?n))

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

(use-package char-menu
  :ensure t
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
  :ensure t
  :config
  (add-to-list 'compilation-error-regexp-alist
               '("^  \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(.*\\)$" 1 2 3 2))) ; I think this is from pyright

(use-package xref
  :ensure t
  :defines (xref-show-xrefs-function xref-show-definitions-function))

(use-package consult
  :ensure t
  :after (project xref)
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

(defvar my/hyper-n-map
  (make-sparse-keymap)
  "Keymap for Hyper-n actions.")

(use-package denote
  :ensure t
  :commands (denote-dired-mode-in-directories)
  :hook (dired-mode . denote-dired-mode)
  :bind (:map my/hyper-n-map
              ("b" . denote-backlinks)
              ("d" . denote-dired)
              ("g" . denote-grep)
              ("l" . denote-link)
              ("n" . denote)
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
  :ensure t
  :after (consult denote)
  :defines (consult-notes-denote-files-function)
  :commands (consult-notes-denote-mode denote-directory-files)
  :config
  (require 'consult-notes-denote)
  :bind (:map my/hyper-n-map ("c" . consult-notes)))

(use-package corfu
  :after orderless
  :ensure t
  :commands (global-corfu-mode)
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
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
  :init
  (global-corfu-mode)                   ; This does not play well in eshell if you run a repl
  (setq corfu-auto t))

;; ;; (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
;; ;; (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous

(use-package crm)

(use-package crux
  :ensure t
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

;; (use-package eldoc-box
;;   :ensure t)
;; :if my/is-terminal)
;; :hook (prog-mode . eldoc-box-hover-mode)))

(use-package emacs-pager
  :commands (emacs-pager emacs-pager-mode))

;; FYI: Embark's default action binding of "RET" fails if a mode binds to <return>.
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package esup
;;   :ensure t
;;   :custom (esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package expand-region
  :ensure t
  :bind ("C-\\" . er/expand-region))

(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode)
  :hook ((compilation-mode . fancy-compilation-mode)))

(use-package flyover
  :ensure t
  :hook ((flymake-mode . flyover-mode))
  :custom
  ;; Appearance
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
  :ensure t
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
  :ensure t)

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
  :ensure t
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
  :custom (magit-process-find-password-functions '(my/read-gitlab-password)))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-M-<tab>" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

(use-package mode-line-bell
  :ensure t)

(use-package mood-line
  :ensure t
  ;; :if (display-graphic-p)
  :commands (mood-line-mode)
  :hook (after-init . mood-line-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         :map my/hyper-c-map
         ("." . mc/mark-all-like-this)))

(use-package my-fontify-braces)

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia)
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :hook ((after-init . nerd-icons-completion-mode)
         (marginalia-mode nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package orderless
  :ensure t
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
  :ensure t
  :if my/is-macosx
  :bind (:map my/hyper-c-map ("l" . osx-dictionary-search-pointer)))

(use-package popper
  :ensure t
  :defer nil                            ; load now due to dependencies below
  :commands (popper-kill-latest-popup)
  :functions (popper--delete-popup)
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)))

(use-package project
  :commands (project--switch-project-command) ;; used in my/show-project-menu
  :bind (:map project-prefix-map
              ("$" . project-shell)))

(keymap-set project-prefix-map "m" #'my/show-project-menu)

(defvar my/project-search-map (make-sparse-keymap)
  "A prefix map like that found in projectile.
Bound to \\`C-x p s'.")

(use-package rg
  :ensure t
  :after (project)
  :commands (rg-enable-default-bindings rg-project)
  :bind (:map project-prefix-map
              ("s" . my/project-search-map)
              :map my/project-search-map
              ("r" . #'rg-project))
  :hook (after-init . rg-enable-default-bindings))

(use-package scratch
  :ensure t
  :bind (:map my/hyper-c-map ("s" . scratch)))

(use-package tempo
  :ensure t
  :commands (tempo-define-template))

(defun tempo-template-my/org-emacs-lisp-source (&optional _)
  "Define empty function to satisfy flymake/byte-compile (ARG is ignored).")

(tempo-define-template "my/org-emacs-lisp-source" '("#+begin_src emacs-lisp" & r % "#+end_src")
                       "<m"
                       "Insert an Emacs Lisp source block in an org document.")

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package which-key
  :ensure t)

(use-package winner
  :ensure t
  :bind (("C-<left>" . winner-undo)
         ("C-<right>" . winner-redo)
         :map my/hyper-c-map
         ("u" . winner-undo)
         ("C-u" . winner-undo)
         ("C-r" . winner-redo)))

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

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
  (setq read-process-output-max (* 64 1024 1024)
	process-adaptive-read-buffering nil
        ;; debug-on-error t
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
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
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

(when my/is-graphical
  (my/emacs-key-bind global-map
                     ;; NOTE: these conflict with terminal escape sequences so only use on graphical displays
                     "M-O" #'my/ace-window-always-dispatch
                     "M-[" #'previous-buffer
                     "M-]" #'next-buffer))

(defvar my/hyper-keys-map
  (make-sparse-keymap)
  "Keymap for terminal hyper actions.")

;; Populate two key maps with hyper-key definitions. The first -- global -- holds the mapping that uses the real `Hyper'
;; modifier. The second keymap -- my/hyper-keys-map -- holds the mapping that uses a keychord to activate which is
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
                           "H-c" my/hyper-c-map
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
                           "H-z" #'my/shell
                           "H-," #'my/customize-search
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

(if my/is-terminal
    (when my/is-linux
      (set-face-background 'default "undefined"))
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

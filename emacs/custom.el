;;; package -- custom.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist '(("." . ".~")))
 '(case-fold-search t)
 '(company-backends
   '(company-capf company-keywords company-semantic company-files company-etags company-elisp company-clang company-dabbrev-code company-cmake company-ispell company-yasnippet))
 '(company-ispell-dictionary "/usr/share/dict/web2")
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.2)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output t)
 '(completion-styles '(basic partial-completion emacs22 flex))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "9160510e527c6037f3413a3e90ff6452a0011d494a3216c6a4f69001df161bee" default))
 '(default-frame-alist '((width . 132) (height . 77) (top . 10) (left . 956)))
 '(delete-old-versions t)
 '(dired-isearch-filenames t)
 '(display-time-mode t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0.2)
 '(elpy-autodoc-delay 0.2)
 '(fill-column 120)
 '(global-subword-mode t)
 '(helm-apropos-fuzzy-match nil)
 '(helm-autoresize-min-height 20)
 '(helm-buffers-fuzzy-matching nil)
 '(helm-candidate-number-limit 500)
 '(helm-ff-DEL-up-one-level-maybe nil)
 '(helm-ff-auto-update-initial-value t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-follow-mode-persistent t)
 '(helm-imenu-fuzzy-match nil)
 '(helm-sources-using-default-as-input
   '(helm-source-info-bash helm-source-imenu helm-source-imenu-all helm-source-info-elisp helm-source-etags-select helm-source-man-pages helm-source-occur helm-source-moccur helm-source-grep-ag helm-source-grep-git helm-source-grep))
 '(helm-visible-mark-prefix "✓")
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "howes")
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((width . 132) (height . 77) (top . 10) (left . 0)))
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(kept-old-versions 10)
 '(ls-lisp-use-insert-directory-program nil)
 '(mac-command-modifier 'meta)
 '(mac-emulate-three-button-mouse t)
 '(mac-option-modifier 'alt)
 '(mac-pass-command-to-system nil)
 '(mac-right-option-modifier 'super)
 '(makefile-electric-keys t)
 '(mode-line-bell-flash-time 0.08)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'super)
 '(org-edna-use-inheritance t)
 '(org-roam-db-autosync-mode t)
 '(org-roam-directory "/Users/howes/org-roam")
 '(package-quickstart t)
 '(package-selected-packages
   '(company-quickhelp company-jedi jedi jedi-core projectile-ripgrep ripgrep all-the-icons-dired all-the-icons-ibuffer mode-icons doom-modeline doom-themes flyspell-correct-helm org-gtd org helm-ls-git helm-projectile flyspell-correct-popup benchmark-init yasnippet-snippets yasnippet markdown-mode eglot cmake-mode diff-hl company projectile async diminish modus-vivendi-theme session highlight-indentation mode-line-bell magit use-package))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line-prefix " P")
 '(pyvenv-workon ".")
 '(ring-bell-function 'mode-line-bell-flash)
 '(save-place-mode t)
 '(savehist-mode t)
 '(sentence-end "[.?!][]\"')]*\\($\\|	\\| \\)[
]*")
 '(sentence-end-double-space nil)
 '(session-use-package t nil (session))
 '(split-width-threshold nil)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(use-package-always-defer t)
 '(vc-follow-symlinks t)
 '(version-control t)
 '(visible-bell nil)
 '(warning-suppress-log-types '((comp) ((flymake flymake))))
 '(warning-suppress-types '((comp) ((flymake flymake)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type tty)) nil) (t (:height 141 :family "Inconsolata" :foundry "nil" :slant normal :weight normal :width normal))))
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(font-lock-brace-face ((t (:foreground "tomato"))))
 '(helm-mark-prefix ((t (:foreground "Gold1"))))
 '(highlight-indentation-current-column-face ((t nil)))
 '(hl-line ((t (:background "gray14" :extend t :inherit modus-themes-hl-line))))
 '(makefile-space ((t (:background "gray46"))))
 '(show-paren-match ((t (:background "#0d0d0d" :foreground "spring green" :underline nil :weight ultra-bold)))))

;;; custom.el ends here.

;;; package -- custom.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3--

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
 '(auto-revert-use-notify nil)
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist '(("." . ".~")))
 '(byte-compile-verbose nil)
 '(case-fold-search t)
 '(comint-buffer-maximum-size 8192)
 '(company-backends
   '(company-capf company-keywords company-semantic company-files company-etags company-elisp company-clang company-dabbrev-code company-cmake company-ispell company-native-complete))
 '(company-idle-delay 0.15)
 '(company-ispell-dictionary "/usr/share/dict/web2")
 '(company-minimum-prefix-length 3)
 '(company-require-match nil)
 '(company-selection-wrap-around t)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.2)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" default))
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-isearch-filenames t)
 '(display-time-default-load-average nil)
 '(doom-modeline-buffer-file-name-style 'relative-from-project)
 '(doom-modeline-buffer-file-true-name t)
 '(doom-modeline-irc nil)
 '(doom-modeline-lsp nil)
 '(doom-modeline-major-mode-color-icon nil)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-project-detection 'projectile)
 '(doom-modeline-vcs-max-length 40)
 '(doom-modeline-window-width-limit 120)
 '(dynamic-completion-mode t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0.2)
 '(fill-column 120)
 '(find-file-visit-truename t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "howes")
 '(inhibit-startup-screen t)
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(kept-old-versions 10)
 '(ls-lisp-use-insert-directory-program nil)
 '(makefile-electric-keys t)
 '(mode-line-bell-flash-time 0.08)
 '(mode-line-compact 'long)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-percent-position nil)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'super)
 '(org-edna-use-inheritance t)
 '(org-roam-db-autosync-mode t)
 '(org-roam-directory "/Users/howes/org-roam")
 '(package-archives
   '(("melpa" . "http://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-quickstart t)
 '(package-selected-packages
   '(consult-dir orderless vertico consult embark-consult embark helm-projectile markdown-mode ace-window flymake yaml yaml-mode which-key wgrep scratch ripgrep reformatter projectile-ripgrep projectile mode-line-bell mode-icons marginalia magit jedi-core highlight-indentation helm flycheck-indentation flycheck doom-themes doom-modeline diminish diff-hl company-quickhelp company-jedi company cmake-mode async all-the-icons-ibuffer all-the-icons-dired all-the-icons-completion all-the-icons))
 '(projectile-generic-command "rg --files --hidden -0")
 '(projectile-globally-ignored-directoriesx
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".tox" ".svn" ".cache" ".clangd" "./build" "./cmake"))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line-prefix " P")
 '(pyvenv-workon ".")
 '(ring-bell-function 'mode-line-bell-flash)
 '(save-place-mode t)
 '(savehist-mode t)
 '(sentence-end "[.?!][]\"')]*\\($\\|\11\\| \\)[\12]*")
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
 '(warning-suppress-types '((emacs) (comp) ((flymake flymake)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Berkeley Mono" :foundry "nil" :slant normal :weight regular :height 130 :width normal))))
 '(aw-leading-char-face ((t (:inherit (bold modus-themes-reset-soft) :foreground "light green" :height 1.5))))
 '(doom-modeline-bar-inactive ((t nil)))
 '(eglot-highlight-symbol-face ((t (:background "orchid4" :inherit bold))))
 '(fixed-pitch ((t (:family "Berkeley Mono"))))
 '(font-lock-brace-face ((t (:foreground "deep pink"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "OliveDrab3" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "olive drab" :inherit modus-themes-slant))))
 '(helm-buffer-directory ((t (:extend t :foreground "DarkRed"))))
 '(helm-ff-directory ((t (:extend t :foreground "deep sky blue" :slant normal))))
 '(helm-mark-prefix ((t (:foreground "Gold1"))))
 '(helm-visible-mark ((t (:background "dark slate blue" :extend t :inherit bold))))
 '(highlight-indentation-current-column-face ((t nil)))
 '(hl-line ((t (:background "gray14" :extend t :inherit modus-themes-hl-line))))
 '(makefile-space ((t (:background "gray46"))))
 '(mode-line ((t (:inherit modus-themes-ui-variable-pitch :background "dark slate gray" :foreground "#ffffff" :box nil))))
 '(mode-line-inactive ((t (:inherit modus-themes-ui-variable-pitch :background "tomato4" :foreground "gray" :box nil))))
 '(show-paren-match ((t (:background "#0d0d0d" :foreground "spring green" :underline nil :weight ultra-bold)))))


;;; custom.el ends here.

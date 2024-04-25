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
 '(auto-save-visited-interval 60)
 '(backup-directory-alist '(("." . ".~")))
 '(bookmark-save-flag 1)
 '(byte-compile-verbose nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10000000)
 '(comint-input-ignoredups t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-scroll-output t)
 '(completion-cycle-threshold 3)
 '(confirm-kill-emacs 'y-or-n-p)
 '(copyright-query nil)
 '(copyright-year-ranges t)
 '(corfu-auto-prefix 1)
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(denote-rename-buffer-mode t)
 '(denote-auto-revert-buffer t)
 '(denote-dwim-target t)
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-AbBhF --group-diredtories-first") ;; "-hADFG"
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-time-default-load-average nil)
 '(doom-modeline-buffer-file-name-style 'relative-from-project)
 '(doom-modeline-buffer-file-true-name t)
 '(doom-modeline-irc nil)
 '(doom-modeline-lsp t)
 '(doom-modeline-major-mode-color-icon nil)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes t)
 '(doom-modeline-mode t)
 '(doom-modeline-project-detection 'projectile)
 '(doom-modeline-vcs-max-length 40)
 '(doom-modeline-window-width-limit 120)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(dynamic-completion-mode t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0.2)
 '(enable-recursive-minibuffers t)
 '(fill-column 120)
 '(find-file-visit-truename t)
 '(frame-resize-pixelwise t)
 '(global-prettify-symbols-mode t)
 '(global-subword-mode t)
 '(grep-save-buffers t)
 '(history-delete-duplicates t)
 '(indent-bars-prefer-character t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "howes")
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(jit-lock-stealth-load nil)
 '(jit-lock-stealth-nice nil)
 '(kept-old-versions 10)
 '(line-move-visual nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(magit-diff-refine-hunk t)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(makefile-electric-keys t)
 '(marginalia-mode t)
 '(message-send-mail-function 'sendmail-send-it)
 '(mode-line-bell-flash-time 0.08)
 '(mode-line-compact 'long)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-percent-position nil)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'hyper)
 '(ns-right-command-modifier 'super)
 '(org-edna-use-inheritance t)
 '(org-roam-db-autosync-mode t)
 '(org-roam-directory "/Users/howes/org-roam")
 '(package-archives
   '(("melpa" . "http://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-quickstart t)
 '(package-selected-packages
   '(doom-modeline window-commander ef-themes dired-single persp-mode edwina rg eglot indent-guide ws-butler cape corfu-info lsp-sourcekit swift-mode corfu use-package treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile denote consult-dir orderless vertico consult embark-consult embark markdown-mode ace-window flymake yaml yaml-mode which-key wgrep scratch ripgrep reformatter projectile-ripgrep projectile mode-line-bell mode-icons marginalia magit jedi-core highlight-indentation flycheck-indentation flycheck diminish diff-hl company-quickhelp company-jedi company cmake-mode async all-the-icons-ibuffer all-the-icons-dired all-the-icons-completion all-the-icons))
 '(projectile-generic-command "rg --files --hidden -0")
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".tox" ".svn" ".cache" ".clangd" "./build" "./cmake"))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line-prefix " P")
 '(projectile-sort-order 'recently-active)
 '(projectile-switch-project-action 'projectile-dired)
 '(pyvenv-workon ".")
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 50)
 '(resize-mini-windows t)
 '(ring-bell-function 'mode-line-bell-flash)
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
     (projectile-project-compilation-cmd . "./build.sh --mode Debug")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))))
 '(save-place-mode t)
 '(savehist-additional-variables
   '(register-alist kill-ring projectile-project-command-history corfu-history))
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-margin 2)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/opt/homebrew/bin/msmtp")
 '(sentence-end "[.?!][]\"')]*\\($\\|\11\\| \\)[\12]*")
 '(sentence-end-double-space nil)
 '(smtpmail-smtp-server "smtp.mail.me.com")
 '(smtpmail-smtp-service 587)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(switch-to-prev-buffer-skip t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode nil)
 '(use-package-always-defer t)
 '(user-full-name "Brad Howes")
 '(user-mail-address "bradhowes@mac.com")
 '(vc-follow-symlinks t)
 '(version-control t)
 '(vertico-count 30)
 '(vertico-cycle t)
 '(vertico-resize t)
 '(visible-bell nil)
 '(warning-suppress-log-types '((comp) ((flymake flymake))))
 '(warning-suppress-types '((emacs) (comp) ((flymake flymake))))
 '(which-key-idle-delay 1.0))
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
 '(highlight-indentation-current-column-face ((t nil)))
 '(hl-line ((t (:background "gray14" :extend t :inherit modus-themes-hl-line))))
 '(makefile-space ((t (:background "gray46"))))
 '(mode-line ((t (:inherit modus-themes-ui-variable-pitch :background "dark slate gray" :foreground "#ffffff" :box nil))))
 '(mode-line-inactive ((t (:inherit modus-themes-ui-variable-pitch :background "tomato4" :foreground "gray" :box nil))))
 '(show-paren-match ((t (:background "#0d0d0d" :foreground "spring green" :underline nil :weight ultra-bold)))))


;;; custom.el ends here.

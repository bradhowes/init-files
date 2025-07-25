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
 '(all-the-icons-completion-mode t)
 '(auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
 '(auto-revert-use-notify nil)
 '(auto-save-list-file-prefix nil)
 '(auto-save-visited-interval 60)
 '(backup-directory-alist '(("." . ".~")))
 '(blink-cursor-blinks 0)
 '(bookmark-save-flag 1)
 '(byte-compile-verbose nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10000000)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-scroll-output t)
 '(completion-category-overrides '((file (styles basic partial-completion))) nil nil "Customized with use-package orderless")
 '(completion-cycle-threshold 3)
 '(completion-styles '(orderless partial-completion basic) nil nil "Customized with use-package orderless")
 '(confirm-kill-emacs 'y-or-n-p)
 '(consult-narrow-key "<")
 '(consult-notes-denote-files-function #'denote-directory-files)
 '(consult-notes-denote-mode t)
 '(consult-notes-use-rg t)
 '(copyright-query nil)
 '(copyright-year-ranges t)
 '(corfu-history-mode t nil nil "Customized with use-package corfu")
 '(corfu-popupinfo-delay '(0.5 . 0.5))
 '(corfu-popupinfo-mode t)
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8"
     "183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(denote-file-type 'markdown-brh)
 '(denote-infer-keywords t)
 '(denote-rename-buffer-mode t)
 '(denote-sort-keywords t)
 '(diff-hl-flydiff-mode t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-time-default-load-average nil)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-autoshutdown t)
 '(eglot-connect-timeout nil)
 '(eglot-ignored-server-capabilities '(:documentHighlightProvider))
 '(eglot-send-changes-idle-time nil)
 '(enable-recursive-minibuffers t)
 '(fill-column 120)
 '(find-file-visit-truename t)
 '(flyspell-mode-line-string "s ")
 '(frame-resize-pixelwise t)
 '(global-corfu-mode t nil nil "Customized with use-package corfu")
 '(global-diff-hl-mode t)
 '(global-diff-hl-show-hunk-mouse-mode t)
 '(global-eldoc-mode t)
 '(global-hl-line-mode t)
 '(global-prettify-symbols-mode t)
 '(global-subword-mode t)
 '(grep-save-buffers t)
 '(history-delete-duplicates t)
 '(httpd-port 4465)
 '(indent-bars-color '(highlight :face-bg t :blend 0.4))
 '(indent-bars-prefer-character t)
 '(indent-bars-unspecified-bg-color "black")
 '(indent-bars-unspecified-fg-color "white")
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "howes")
 '(inhibit-startup-screen t)
 '(insert-directory-program "gls")
 '(isearch-lazy-count t)
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(ispell-local-dictionary "english")
 '(jit-lock-stealth-load nil)
 '(jit-lock-stealth-nice nil)
 '(kept-old-versions 10)
 '(key-chord-mode t)
 '(line-move-visual nil)
 '(load-prefer-newer t)
 '(ls-lisp-use-insert-directory-program nil)
 '(magit-diff-refine-hunk t)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(major-mode-remap-alist '((python-mode . python-ts-mode)))
 '(makefile-electric-keys t)
 '(marginalia-mode t)
 '(message-send-mail-function 'sendmail-send-it)
 '(minibuffer-electric-default-mode t nil nil "Customized with use-package emacs")
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt) nil nil "Customized with use-package emacs")
 '(mode-line-bell-flash-time 0.08)
 '(mode-line-compact 'long)
 '(mode-line-in-non-selected-windows t)
 '(mode-line-percent-position nil)
 '(my/screen-4k-pick 0)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'hyper)
 '(ns-right-command-modifier 'super)
 '(ns-right-control-modifier 'hyper)
 '(package-quickstart t)
 '(package-selected-packages
   '(accent ace-window async cape char-menu cmake-mode compile-multi consult consult-compile-multi consult-denote
            consult-dir consult-eglot consult-notes corfu corfu-terminal crux dape diff-hl ef-themes eglot-luau
            eldoc-box embark embark-consult esup exec-path-from-shell expand-region fancy-compilation flymake-aspell
            flymake-json flymake-lua flymake-shellcheck geiser geiser-guile impatient-mode indent-bars jedi-core
            jiralib2 js-comint js2-mode json-mode key-chord logview lsp-jedi lsp-pyright lua-mode magit marginalia
            mode-line-bell mood-line multiple-cursors nerd-icons-completion nerd-icons-corfu nerd-icons-dired
            nerd-icons-ibuffer orderless org-jira osx-dictionary pdb-capf popper pyvenv realgud rg scratch
            tree-sitter-langs treemacs-nerd-icons verb vertico which-key ws-butler yaml yaml-mode yasnippet
            yasnippet-capf yasnippet-snippets))
 '(package-vc-selected-packages
   '((magit :url "https://github.com/magit/magit.git" :branch "v4.1.3" :lisp-dir "lisp" :make '("lisp" "info") :doc
            "docs/magit.texi")))
 '(pixel-scroll-mode t)
 '(pixel-scroll-precision-mode t)
 '(pixel-scroll-precision-use-momentum t)
 '(popper-display-control 'user)
 '(popper-echo-mode t)
 '(popper-mode t)
 '(popper-reference-buffers
   '("\\*Messages\\*" "Output\\*$" "\\*Async Shell Command\\*" "\\*Compile-Log\\*" "\\*Man .*\\*" "\\*eldoc\\*" help-mode
     compilation-mode))
 '(pyvenv-workon ".")
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(register-preview-delay 0.5)
 '(resize-mini-windows t)
 '(ring-bell-function 'mode-line-bell-flash)
 '(safe-local-variable-directories '("/Users/howes/src/Mine/init-files/"))
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (projectile-project-compilation-cmd . "make ") (projectile-project-compilation-cmd . "make -C emacs/lisp -k ")
     (checkdoc-package-keywords-flag)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(register-alist kill-ring project-regexp-history-variable corfu-history))
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-margin 2)
 '(search-default-mode t)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/opt/homebrew/bin/msmtp")
 '(sentence-end "[.?!][]\"')]*\\($\\|\11\\| \\)[\12]*")
 '(sentence-end-double-space nil)
 '(sh-mode-hook '(flymake-shellcheck-load indent-bars-mode my/sh-mode-hook))
 '(size-indication-mode nil)
 '(smtpmail-smtp-server "smtp.mail.me.com")
 '(smtpmail-smtp-service 587)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(switch-to-prev-buffer-skip t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode nil)
 '(trusted-content :all)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-package-always-defer t)
 '(user-full-name "Brad Howes")
 '(user-mail-address "bradhowes@mac.com")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(vertico-count 30)
 '(vertico-cycle t)
 '(vertico-mode t)
 '(vertico-resize t)
 '(visible-bell nil)
 '(which-key-idle-delay 1.0)
 '(which-key-mode t)
 '(windmove-wrap-around t)
 '(winner-mode t)
 '(world-clock-time-format "%d %b %R %Z")
 '(xref-show-definitions-function 'consult-xref)
 '(xref-show-xrefs-function 'consult-xref)
 '(zoneinfo-style-world-list
   '(("America/Montreal" "Montreal") ("America/New_York" "New York") ("Europe/London" "London") ("Europe/Paris" "Paris")
     ("Asia/Tokyo" "Tokyo") ("Asia/Hong_Kong" "Hong Kong") ("Asia/Bangkok" "Bangkok") ("Asia/Singapore" "Singapore"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Berkeley Mono" :foundry "nil" :slant normal :weight regular :height 130 :width normal))))
 '(aw-leading-char-face ((t (:inherit (bold modus-themes-reset-soft) :foreground "light green" :height 1.5))))
 '(cursor ((t (:background "orange"))))
 '(doom-modeline-bar-inactive ((t nil)))
 '(eglot-highlight-symbol-face ((t (:background "orchid4" :inherit bold))))
 '(fixed-pitch ((t (:family "Berkeley Mono"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "OliveDrab3" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "olive drab" :inherit modus-themes-slant))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(highlight-indentation-current-column-face ((t nil)))
 '(hl-line ((t (:background "gray10" :extend t :inherit modus-themes-hl-line))))
 '(makefile-space ((t (:background "gray46"))))
 '(mode-line ((t (:inherit modus-themes-ui-variable-pitch :background "dark slate gray" :foreground "#ffffff" :box nil))))
 '(mode-line-inactive ((t (:inherit modus-themes-ui-variable-pitch :background "tomato4" :foreground "gray" :box nil))))
 '(region ((t (:extend t :background "dark violet" :foreground "#ffffff"))))
 '(show-paren-match ((t (:background "#0d0d0d" :foreground "spring green" :underline nil :weight ultra-bold)))))


;;; custom.el ends here.

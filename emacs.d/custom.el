;;; package -- custom.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("/opt/homebrew/share/info"))
 '(auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
 '(auto-revert-use-notify nil)
 '(auto-save-file-name-transforms '((".*" "~/tmp/emacs_autosaves" t)))
 '(auto-save-list-file-prefix nil)
 '(auto-save-visited-interval 60)
 '(backup-directory-alist '(("." . "~/tmp/emacs_backups")))
 '(blink-cursor-blinks 0)
 '(bookmark-save-flag 1)
 '(byte-compile-verbose nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10000)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-scroll-output t)
 '(compile-multi-config
   '((python-ts-mode ("pytest" . "pytest") ("compile" . "pip3 install -U -e ."))))
 '(completion-cycle-threshold 3)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".bzr/"
     "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl"
     ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl"
     ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
     ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))
 '(confirm-kill-emacs 'y-or-n-p)
 '(consult-dir-default-command 'consult-dir-dired)
 '(consult-dir-sort-candidates t)
 '(consult-dir-sources
   '(consult-dir--source-default consult-dir--source-project consult-dir--source-recentf consult-dir--source-bookmark
                                 consult-dir--source-tramp-local))
 '(consult-notes-denote-files-function #'denote-directory-files)
 '(consult-notes-use-rg t)
 '(context-menu-mode t)
 '(copyright-query nil)
 '(copyright-year-ranges t)
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("967c23e9ba179b80560774419f081df22e7674aac23c5c550b817e4a1ce7d058"
     "6dcf1ca4c7432773084b9d52649ee5eb2c663131c4c06859f648dea98d9acb3e"
     "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8"
     "183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-al --group-directories-first")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(doom-modeline-buffer-encoding 'nondefault)
 '(doom-modeline-buffer-file-name-style 'file-name-with-project)
 '(doom-modeline-buffer-modification-icon nil)
 '(doom-modeline-display-misc-in-all-mode-lines nil)
 '(doom-modeline-selection-info nil)
 '(eat-enable-auto-line-mode t)
 '(eat-line-input-history-isearch 'dwim)
 '(eglot-code-action-indications nil)
 '(eglot-send-changes-idle-time 0.1)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-use-multiline-p 5)
 '(eldoc-help-at-pt t)
 '(enable-recursive-minibuffers t)
 '(exchange-point-and-mark-highlight-region nil)
 '(exec-path-from-shell-arguments '("-l"))
 '(fill-column 120)
 '(find-file-visit-truename t)
 '(flymake-inline-diagnostics nil)
 '(flyspell-mode-line-string "s ")
 '(frame-resize-pixelwise t)
 '(global-eldoc-mode t)
 '(global-prettify-symbols-mode t)
 '(grep-save-buffers t)
 '(history-delete-duplicates t)
 '(httpd-port 4465)
 '(indent-bars-color '(highlight :face-bg t :blend 0.4))
 '(indent-bars-prefer-character t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "howes")
 '(inhibit-startup-screen t)
 '(insert-directory-program "gls")
 '(isearch-lazy-count t)
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(ispell-local-dictionary "english")
 '(kept-old-versions 10)
 '(line-move-visual nil)
 '(magit-diff-refine-hunk t)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(major-mode-remap-alist '((python-mode . python-ts-mode)))
 '(makefile-electric-keys t)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t))
 '(mode-line-bell-flash-time 0.08)
 '(mode-line-compact 'long)
 '(mode-line-percent-position nil)
 '(mouse-yank-at-point t)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-function-modifier 'hyper)
 '(ns-right-alternate-modifier 'hyper)
 '(ns-right-command-modifier 'super)
 '(ns-right-control-modifier 'hyper)
 '(package-selected-packages
   '(accent ace-window cape char-menu cmake-mode compile-multi compile-multi-embark compile-multi-nerd-icons
            consult-compile-multi consult-dir consult-eglot consult-multi-compile consult-notes consult-zoxide corfu
            crux denote diff-hl doom-themes eldoc-box embark-consult esup exec-path-from-shell expand-region
            fancy-compilation flymake-gradle flymake-shellcheck flyover format-all git-timemachine go-mode gradle-mode
            helpful impatient-mode indent-bars inheritenv js2-mode key-chord keycast kkp kotlin-ts-mode language-id
            ligature magit marginalia mode-line-bell mood-line multiple-cursors nano-modeline nerd-icons
            nerd-icons-completion nerd-icons-corfu nerd-icons-dired nerd-icons-grep nerd-icons-xref orderless
            osx-dictionary popper pyenv pyvenv pyvenv-auto rg scratch simple-modeline sleek-modeline symbol-overlay
            tempel tempel-collection timu-line treemacs-nerd-icons ultra-scroll vertico ws-butler xclip yaml yaml-mode
            yasnippet yasnippet-snippets zoxide))
 '(popper-display-control 'user)
 '(popper-reference-buffers
   '("\\*MESSAGES\\*" "Output\\*$" "\\*Async Shell Command\\*" "\\*Compile-Log\\*" "\\*Man .*\\*" "\\*eldoc\\*" help-mode
     compilation-mode))
 '(project-mode-line 'non-remote)
 '(project-switch-commands
   '((project-find-file "Find file" nil) (project-find-regexp "Find regexp" nil) (project-find-dir "Find directory" nil)
     (magit-project-status "Magit" 109) (project-eshell "Eshell" nil) (project-any-command "Other" nil)))
 '(python-indent-guess-indent-offset-verbose nil)
 '(pyvenv-workon ".")
 '(quit-window-kill-buffer t)
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 50)
 '(register-preview-delay 0.5)
 '(resize-mini-windows t)
 '(ring-bell-function 'mode-line-bell-flash)
 '(safe-local-variable-directories '("/Users/howes/src/Mine/init-files"))
 '(safe-local-variable-values
   '((compile-multi-default-directory lambda nil "~/Developer/Mine/sidecar")
     (compile-multi-config
      (t ("cmake" . "rm -rf build && mkdir build && cd build && cmake ..") ("compile" . "cd build && make -j 8")))
     (compile-multi-default-directory lambda nil "~/src/python-project")
     (compile-multi-default-directory lambda nil my/project-root)
     (compile-multi-config
      (python-ts-mode (#("pytest" 0 1 (compile-multi--task "pytest")) . "pytest")
                      (#("compile" 0 1 (compile-multi--task "pip3 install -U -e .")) . "pip3 install -U -e .")))
     (compile-multi-config quote ((python-ts-mode ("pytest" . "pytest") ("compile" . "pip3 install -U -e ."))))
     (compile-multi-default-directory . my/project-root) (my/project-root . "~/src/python-project")
     (compile-multi-config
      '((python-mode ("make:build" . "pip install -U -e .") ("make:test" . "pytest")) (t ("make:test" . "pytest"))))
     (compile-multi-config '((python-ts-mode ("make:build" . "pip install -U -e .") ("make:test" . "pytest"))))
     (compile-multi-config '((t ("make:build" . "pip install -U -e .") ("make:test" . "pytest"))))
     (dired-omit-files . "*.elc") (checkdoc-minor-mode . t)))
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables
   '(register-alist kill-ring project-regexp-history-variable corfu-history))
 '(scroll-preserve-screen-position 1)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/opt/homebrew/bin/msmtp")
 '(sentence-end "[.?!][]\"')]*\\($\\|\11\\| \\)[\12]*")
 '(sentence-end-double-space nil)
 '(smtpmail-smtp-server "smtp.mail.me.com")
 '(smtpmail-smtp-service 587)
 '(switch-to-prev-buffer-skip t)
 '(tab-always-indent 'complete)
 '(text-scale-mode-step 1.0)
 '(tramp-auto-save-directory "/Users/bradhowes/tmp/emacs_autosaves")
 '(tramp-backup-directory-alist '((".*" . "/Users/bradhowes/tmp/emacs_backups")))
 '(truncate-string-ellipsis "…" t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-package-always-defer t)
 '(user-mail-address "bradhowes@mac.com")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(vertico-count 30)
 '(vertico-cycle t)
 '(vertico-resize t)
 '(view-read-only t)
 '(whitespace-line-column my/layout-cols-graphical)
 '(whitespace-style '(face trailing lines-tail empty))
 '(windmove-wrap-around t))
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
 '(mode-line-active ((t (:inherit modus-themes-ui-variable-pitch :background "#505050" :foreground "#ffffff" :box (:line-width (1 . 1) :color "#959595")))))
 '(mode-line-inactive ((t (:inherit modus-themes-ui-variable-pitch :background "gray10" :foreground "gray" :box (:line-width (1 . 1) :color "grey75" :style released-button)))))
 '(region ((t (:extend t :background "dark violet" :foreground "#ffffff"))))
 '(show-paren-match ((t (:background "#0d0d0d" :foreground "spring green" :underline nil :weight ultra-bold)))))


;;; custom.el ends here.

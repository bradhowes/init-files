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
 '(completion-styles '(basic partial-completion emacs22 flex))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "9160510e527c6037f3413a3e90ff6452a0011d494a3216c6a4f69001df161bee" default))
 '(default-frame-alist '((width . 132) (height . 77) (top . 10) (left . 10)))
 '(delete-old-versions t)
 '(display-time-mode t)
 '(fill-column 120)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((width . 132) (height . 77) (top . 10) (left . 10)))
 '(ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case"))
 '(kept-old-versions 10)
 '(ls-lisp-use-insert-directory-program nil)
 '(makefile-electric-keys t)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'super)
 '(package-selected-packages
   '(flyspell-correct-helm helm-ls-git flyspell-correct-popup popup benchmark-init yasnippet-snippets yasnippet markdown-mode eglot cmake-mode diff-hl company projectile async diminish zerodark-theme modus-vivendi-theme session highlight-indentation mode-line-bell magit use-package))
 '(ring-bell-function 'ignore)
 '(save-place-mode t)
 '(savehist-mode t)
 '(sentence-end "[.?!][]\"')]*\\($\\|	\\| \\)[
]*")
 '(sentence-end-double-space nil)
 '(session-use-package t nil (session))
 '(split-width-threshold nil)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(version-control t)
 '(visible-bell t)
 '(warning-suppress-types '(((flymake flymake)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type tty))) (t (:height 140 :family "Inconsolata" :foundry "nil" :slant normal :weight normal :width normal))))
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(font-lock-brace-face ((t (:foreground "tomato"))))
 '(makefile-space ((t (:background "gray46")))))

;;; custom.el ends here.

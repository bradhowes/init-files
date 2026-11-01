---
title:      Nerd Font / Icons
date:       2026-10-31T11:12:14+01:00
tags:       berkeley, emacs, fonts, nerdfonts
identifier: 
signature:  
---

## Text Setup

Download [Berkeley Mono][bm] with following options:

* zero with a dot
* seven with a bar
* solid verticals bar
* curved 'r'

Install on macOS via FontBook.

## Symbols

Download [Symbols Nerd Font][snf]. Install on macOS via FontBook.

## Emacs Configuration

Add or replace the `default` value to `custom.el` in the `custom-set-faces` section at the bottom of the file:

```
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Berkeley Mono" :foundry "nil" :slant normal :weight regular :height 130 :width normal))))
```

The height is not that important as it will be changed during startup to a size that is appropriate for the display
geometry available to Emacs. See [my-layout][ml].

Install the following packages:

* nerd-icons
* nerd-icons-completion
* nerd-icons-corfu
* nerd-icons-dired
* nerd-icons-grep
* nerd-icons-xref
* treemacs-nerd-icons
* compile-multi-nerd-icons

Finally, use them:

```elisp
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :commands (nerd-icons-completion-mode))

(use-package compile-multi-nerd-icons)

(use-package nerd-icons-corfu
  :commands (nerd-icons-corfu-formatter)
  :hook ((after-init . my/nerd-icons-corfu-initialize)))

(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package nerd-icons-xref
  :hook ((after-init . nerd-icons-xref-mode)))
```

[bm]: https://usgraphics.com/products/berkeley-mono
[snf]: https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.1/NerdFontsSymbolsOnly.zip
[ml]: ../emacs.d/user-lisp/my-layout.el

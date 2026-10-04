---
title:      Using vterm
date:       2024-08-06 20:28:28
tags:       emacs
identifier: "20240806T202828"
---
# Using vterm

** NOTE: emitiing escape sequences from vterm will cause Emacs to crash **

Using Emacs 29.4 from Brew tap `d12frosted/emacs-plus` but should work with stock.
(GNU Emacs 29.4 (build 2, aarch64-apple-darwin24.0.0, NS appkit-2559.10 Version 15.0 (Build 24A5298h)) of 2024-07-27

Source repo - https://github.com/akermu/emacs-libvterm

For some reason, it does not appear in the list of packages on MELPA -- perhaps because of dependency issues -- so
install via `use-package` per below.

- install `libvterm` -- `brew install libvterm`
- add the following to `init.el` and evaluate it or restart Emacs

```
(use-package vterm
  :vc (:fetcher github :repo "akermu/emacs-libvterm"))
```

- Run `M-x vterm-module-compile` to compile a shim that allows Emacs to load `libvterm.so`
- Try it out with `M-x vterm`

Too bad this does not works...

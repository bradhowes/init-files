---
title:      Emacs in iTerm2
date:       2026-09-26T15:57:02+02:00
tags:       emacs, keymap
identifier: "20260926T155702"
---

Quick note on how to configure [iTerm] and [Emacs][emacs] to work better together (here, Emacs is started from a
shell prompt via `emacs -nw`).

## Modifiers

By default, iTerm leaves the modifiers alone, meaning that typing a sequence like ⌘-W (Cmd-W) will function just like in
any other macOS application and close the active window. This is OK, but when one is running Emacs, one usually would
like the `⌘` key to act as a "meta" modifier, such that ⌘-W is seen by Emacs as `M-w` which is mapped to the `yank-pop`
operation.

1. In "Keys" settings, swap "Left option" and "Left command" -- this leaves the Option key available to work as an
   iTerm2 `⌘` key, so ⌥-W will close the active window and ⌥-Q will quit the application.
2. In _Profiles_ settings, _Keys_ tab, _General_ tab: set _Left option key_ to be `Esc+`.
3. Make sure _Report keys using CSI u_ is unchecked.
4. Set _Right control key_ to be `Hyper`.

That should be it. To recap, in the _Keys_ settings _Remap modifiers globally_ is not set, and in
_Profiles_ >> _Keys_ >> _General_ settings,
the first and last _Key reporting_ options are enabled, but nothing else.

Emacs should now see the _left control_ and _left meta_ modifiers. When Karabiner is remapping "caps lock" to _right
control_, we can also have Emacs differentiate between the left and right modifiers by using the ["Kitty Keyboard
Protocol"][KKP] to communicate them. The [Emacs kkp package][emacs-kkp] performs the necessary terminal setup with
iTerm2 to enable KKP:

```elisp
(use-package kkp
  :defer t
  :ensure t
  :commands (global-kkp-mode)
  :hook (tty-setup . (lambda ()
                       ;; (setq kkp-alt-modifier 'alt) ; use to map the Alt keyboard modifier to Alt (and not to Meta)
                       ;; For C-g aborting blocking subprocesses, see "C-g and blocking subprocesses" in the README.
                       (setq kkp-restore-legacy-keys-around-subprocesses t)
                       (global-kkp-mode 1))))
```

Once KKP mode is enabled, running `emacs -nw` in the iTerm2 window should now see a _hyper_ modifier via the "caps lock"
key.

## Clipboard Access

With the above modifier settings, pasting into the iTerm2 window no longer happens with ⌘-V (Cmd-V); one must remember
to type ⌥-V (Option-V) instead. This works, but when working in Emacs, the habit is to use Cmd-Y "yank". By default,
Emacs in a term window does not know about clipboards on the host OS. Once again, there is an yet-another Emacs package
that rectifies the situation -- [xclip]. Once installed and enabled via `(xclip-mode 1)`, the contents of the clipboard
will become available to Emacs when "yanking".

```elisp
(use-package xclip
  :defer t
  :ensure t
  :commands (xclip-mode)
  :hook (tty-setup . (lambda () (xclip-mode 1))))
```

Now, "killing" text (Cmd-W) text or copying it (Meta-W) will put the text into the OS clipboard making available for
pasting in another app. Same works in the other direction so that after cutting or copying text in another app, one can
bring the copy into Emacs via Cmd-Y.

[iTerm]: https://iterm2.com
[emacs]: https://www.gnu.org/software/emacs/
[emacs-kkp]: https://github.com/benotn/kkp
[kkp]: https://sw.kovidgoyal.net/kitty/keyboard-protocol/
[xclip]: https://elpa.gnu.org/packages/xclip.html

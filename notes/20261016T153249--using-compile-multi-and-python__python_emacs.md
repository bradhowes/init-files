---
title:      Using compile-multi and Python
date:       2026-10-16T15:32:49+02:00
tags:       python, emacs
identifier: 
signature:  
---

# Installation

Install [compile-multi](https://github.com/mohkale/compile-multi) via package manager. I've also installed

* [compile-multi-embark](https://github.com/mohkale/compile-multi)
* [compile-multi-nerd-icons](https://github.com/mohkale/compile-multi)
* [consult-compile-multi](https://github.com/mohkale/compile-multi) (!)

# Configuration

Nothing complicated.

```Emacs-Lisp
(use-package compile-multi
  :defer t
  :ensure t)

(use-package consult-compile-multi
  :defer t
  :ensure t
  :hook ((after-init . consult-compile-multi-mode)))
```

# Python Project Integration

The `compile-multi` depends on the value found in `compile-multi-config` to present to the user a list of "tasks" to
execute. The best way to do this for a Python project is to put them in a `.dir-locals.el` file at the root of the
Python project. For example, assume that there is a Python project at `~/src/python-project`. We wish to have two
commands available whenever editing a file in the project -- in the root directory or any child directory of it. The
following contents placed in `~/src/python-project/.dir-locals.el` will define two tasks, "pytest" and "compile":

```Emacs-Lisp
;; -*- no-byte-compile: t -*-
((nil . ((compile-multi-config . ((python-ts-mode ("pytest" . "pytest") ("compile" . "pip3 install -U -e .")))))))
```

This will work fine when the Emacs buffer's `default-directory` is `~/src/python-project`, but when it is deeper in the
directory hierarchy, it will fail since the commands expect to run at the root-level. To fix, this we leverage a feature
of the `compile-multi` package and place a custom function in `compile-multi-default-directory` which will return the
project root:

```Emacs-Lisp
;; -*- no-byte-compile: t -*-
((nil . ((compile-multi-config . ((python-ts-mode ("pytest" . "pytest") ("compile" . "pip3 install -U -e ."))))
         (compile-multi-default-directory . (lambda () "~/src/python-project")))))
```

Now, editing a file such as `~/src/python-project/src/example_package/foo.py`, typing "M-x compile-multi" to get to the
list of tasks, and then selecting say "pytest" will correctly run the `pytest` command in the project root and not in the
`~/src/python-project/Src/example_package` directory.

# Emacs Configuration

Current configuration files:

- [early-init.el](early-init.el) -- early steps to reduce startup times
- [init.el](init.el) -- contains normal session configuration
- [custom.el](custom.el) -- contains customized settings
- [lisp](lisp) -- directory containing various mode hooks and functions

# Installation

Run the `install.sh` script in this directory. This creates a new `~/.emacs.d` directory and establishes soft-links to the
files in this repository directory. If `brew` is installed, the script will install packages that the Emacs configuration
depends on:

- coreutils -- in order to get GNU `ls` (as `gls`) for use in `dired` buffers on macOS
- ispell -- spelling service used by Emacs
- shellcheck -- a linter for shell scripts (Bash, Zsh, etc.)

Finally, the script attempts to install GNU Emacs from the `d12frosted/emacs-plus` tap. This installs a pre-built version which
I'm OK with -- the paranoid may want to view all of the source files and then build with the usual configure steps.

# Historical

Old-school Emacs configuration files kept for posterity. Lots of it come from when I first began using Emacs back in v18
time, and a brief detour to XEmacs on Solaris when it appeared Emacs was stuck in the mud. Now, XEmacs is pretty much
dead and buried.

- [dot\_emacs.old](dot_emacs.old) -- was originally named `.emacs` and found in my home directory
- [lisp/old](lisp/old) -- old versions of [lisp](lisp) directory contents, but with a [startup.el](elisp/old/startup.el)
  file that served as a byte-compiled `init.el` file. Some functionality has been kept in some form in the [lisp](lisp)
  directory.

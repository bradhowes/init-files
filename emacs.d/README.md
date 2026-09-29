# Emacs Configuration

Current configuration files:

- [early-init.el](early-init.el) -- early steps to reduce startup times
- [init.el](init.el) -- contains normal session configuration
- [custom.el](custom.el) -- contains customized settings
- [user-lisp](user-lisp) -- directory containing various mode hooks and functions

# Installation

Run the `install.sh` script in this directory. This creates a new `~/.emacs.d` directory and establishes soft-links to
the files in this repository directory. If `brew` is installed, the script will install packages that the Emacs
configuration depends on:

- coreutils -- in order to get GNU `ls` (as `gls`) for use in `dired` buffers on macOS
- ispell -- spelling service used by Emacs
- shellcheck -- a linter for shell scripts (Bash, Zsh, etc.)

Finally, the script attempts to install GNU Emacs from the `d12frosted/emacs-plus` tap. This installs a pre-built
version which I'm OK with -- the paranoid may want to view all of the source files and then build with the usual
configure steps.

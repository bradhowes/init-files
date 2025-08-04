# shellcheck shell=bash # -*- Mode: Sh; -*-

# shellcheck disable=SC2154
. "${my_cfg}/functions.sh"
. "${my_cfg}/aliases.sh"

[[ "${my_arch}" = "Darwin" ]] && ulimit -n 8096

export GEM_HOME="${HOME}/.gem"

export PATH
PathAdd PATH \
        /opt/homebrew/opt/grep/libexec/gnubin \
        /Applications/Emacs.app/Contents/MacOS/bin \
        /opt/homebrew/bin \
        /opt/homebrew/sbin \
        /opt/homebrew/Cellar/ruby/3.2.2_1/bin \
        "${GEM_HOME}/bin" \
        "${HOME}/bin"

HISTSIZE=10000
# shellcheck disable=SC2034
SAVEHIST=${HISTSIZE}
HISTFILE="${HOME}/.history"

setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

# shellcheck disable=SC2034
HISTORY_IGNORE="(cd|ls|ps|pwd|history|exit)"

export ZSH_THEME="robbyrussell"

osc7_cwd() {
    # NOTE: need to fix ansi-osc.el with `(string= (url-host url) (downcase (system-name)))))`
    # to make Emacs see the changes in directory.
    # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68632
    print -P "\e]7;file://${HOST}${PWD}\e\\"
}

esc0_cwd() {
    # NOTE: need to fix ansi-osc.el with `(string= (url-host url) (downcase (system-name)))))`
    # to make Emacs see the changes in directory.
    # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68632
    print -P "\e]0;${PWD}\007"
}

autoload -U add-zsh-hook

export LESS="-FRX"
export PAGER="less ${LESS}"
export CLICOLOR=yes

export TERM=xterm-256color

export EDITOR="emacs"


chpwd1 () { osc7_cwd; }
chpwd2 () { esc0_cwd; }

if [[ -n "${INSIDE_EMACS}" ]]; then
    if [[ -x "$(whence emacsclient)" ]]; then
        EDITOR="$(whence emacsclient)"

        man() {
            ${EDITOR} --eval "(manual-entry \"${*}\")"
        }
    fi
    export PAGER="${HOME}/bin/emacs-pager"

    add-zsh-hook -Uz chpwd1
elif [[ -n "${TERM}" ]]; then
    add-zsh-hook -Uz chpwd2
fi

case "${TERM}" in
    xterm*|rxvt|vt100)
	stty erase '^?'
	;;

    emacs|dumb)
        export TERM=xterm-256color
        export PAGER="${HOME}/bin/emacs-pager"
	;;
esac

export MANPAGER="${PAGER}"
export GIT_PAGER="${PAGER}"

# [[ -x /usr/bin/dircolors ]] && eval TERM=$(xterm-color dircolors)

# Simple prompt - green color, show user name, Git branch

# Setup GIT branch info in prompt
autoload -Uz vcs_info
precmd() { vcs_info; }
zstyle ':vcs_info:git:*' formats '%b|'
setopt PROMPT_SUBST

export PROMPT="${vcs_info_msg_0_}%B%F{green}%n%#%f%b "

# export PS1="\e]0;\u@\h:\w\007[\e[1;32m\]$(parse_git_branch)\u%\[\033[0m\] "
# export PS1="%B%F{green}%n%#%f%b "
# export PROMPT="${vcs_info_msg_0_}%B%F{green}%n%#%f%b "

cd "${PWD}" || :

# shellcheck disable=SC1091
[[ -f "${HOME}/.iterm2_shell_integration.zsh" ]] && . "${HOME}/.iterm2_shell_integration.zsh"

# shellcheck shell=bash # -*- Mode: Sh; -*-

# shellcheck disable=SC2154
. "${my_cfg}/functions.sh"
. "${my_cfg}/aliases.sh"

[[ "${my_arch}" = "Darwin" ]] && ulimit -n 8096

export ASDF_DATA_DIR="${HOME}/.asdf"

echo "PATH=${PATH}"

export PATH
PathAdd PATH \
        /opt/homebrew/opt/python@3.14/libexec/bin/ \
        /opt/homebrew/opt/grep/libexec/gnubin \
        /Applications/Emacs.app/Contents/MacOS/bin \
        "${HOME}/bin" \
        "${ASDF_DATA_DIR}/shims" \
        "${HOME}/.jenv/bin"

# Prepend homebrew paths even if already in PATH. This is due to the fact that Homebrew's own policy is to not shadow
# any Apple bits (wise), but this PATH is only being used in command-line entries, and I *want* to shadow Apple tools
# such as `jq`.
PathAdd -f PATH \
        /opt/homebrew/bin \
        /opt/homebrew/sbin

# Use current Java environment
eval "$(jenv init -)"

HISTSIZE=100000
# shellcheck disable=SC2034
SAVEHIST=${HISTSIZE}
HISTFILE="${HOME}/.history"

setopt EXTENDED_HISTORY
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

  add-zsh-hook -Uz chpwd chpwd1
elif [[ -n "${TERM}" ]]; then
  add-zsh-hook -Uz chpwd chpwd2
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

zstyle ':vcs_info:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{2}[%b]%f'
zstyle ':vcs_info:*' disable bzr cdv cvs darcs fossil hg mtn p4 svk svn tla

# zstyle ':vcs_info:git:*' formats '%b|'
setopt PROMPT_SUBST

# zstyle ':vcs_info:git:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
# zstyle ':vcs_info:git:*' formats '%b %a|'

PS1='${vcs_info_msg_0_}%B%F{green}%#%f%b '

# export PS1="\e]0;\u@\h:\w\007[\e[1;32m\]$(parse_git_branch)\u%\[\033[0m\] "
# export PS1="%B%F{green}%n%#%f%b "
# export PROMPT="${vcs_info_msg_0_}%B%F{green}%n%#%f%b "

cd "${PWD}" || :

# shellcheck disable=SC1091
[[ -f "${HOME}/.iterm2_shell_integration.zsh" ]] && . "${HOME}/.iterm2_shell_integration.zsh"

# The next line updates PATH for the Google Cloud SDK.
# shellcheck disable=SC1091
[[ -f '/Users/howes/google-cloud-sdk/path.zsh.inc' ]] && . '/Users/howes/google-cloud-sdk/path.zsh.inc'

# The next line enables shell command completion for gcloud.
# shellcheck disable=SC1091
[[ -f '/Users/howes/google-cloud-sdk/completion.zsh.inc' ]] && . '/Users/howes/google-cloud-sdk/completion.zsh.inc'

zstyle ':completion:*' menu select

# shellcheck disable=SC2206,SC3030
fpath=(${ASDF_DATA_DIR}/completions $fpath)

autoload -Uz compinit && compinit

# Perform Emacs `eat` integration if enabled
# shellcheck disable=SC1090
[[ -n "${EAT_SHELL_INTEGRATION_DIR}" ]] && . "${EAT_SHELL_INTEGRATION_DIR}"

[[ -f "${HOME}/.ssh/id_ed25519" ]] && ssh-add -q --apple-use-keychain "${HOME}/.ssh/id_ed25519"


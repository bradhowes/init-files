# shellcheck shell=bash # -*- Mode: Sh; -*-

# shellcheck disable=SC2154
. "${my_cfg}/functions.sh"
. "${my_cfg}/aliases.sh"

[[ "${my_arch}" = "Darwin" ]] && ulimit -n 8096

# Use current Java environment
eval "$(jenv init -)"

export HISTSIZE=100000
# shellcheck disable=SC2034
export SAVEHIST=${HISTSIZE}
HISTFILE="${HOME}/.history"

setopt EXTENDED_HISTORY
setopt SHARE_HISTORY

if ((my_is_bash)); then
  setopt INC_APPEND_HISTORY
else
  setopt HIST_EXPIRE_DUPS_FIRST
  setopt HIST_IGNORE_DUPS
  setopt HIST_IGNORE_ALL_DUPS
  setopt HIST_FIND_NO_DUPS
  setopt HIST_IGNORE_SPACE
  setopt HIST_SAVE_NO_DUPS
  setopt HIST_REDUCE_BLANKS
  setopt HIST_VERIFY
fi

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

# Setup GIT branch info in prompt
autoload -Uz vcs_info
precmd() { vcs_info; }

zstyle ':vcs_info:*' actionformats '%F{2}[%b%F{3}|%F{1}%a%F{2}]%f'
zstyle ':vcs_info:*' formats '%F{2}[%b]%f'
zstyle ':vcs_info:*' disable bzr cdv cvs darcs fossil hg mtn p4 svk svn tla
zstyle ':completion:*' menu select

setopt PROMPT_SUBST

# Show git branch in [] if in a repo directory followed by bland '%' or '#' depending on user.
export PROMPT="\${vcs_info_msg_0_}%B%F{green}%#%f%b "

# Force the emission of a path escape sequence for Emacs/term programs. May not be necessary anymore.
cd "${PWD}" || :

# Load in completion facility -- must be done before injecting
autoload -Uz compinit && compinit

# Use current Java environment
[[ -d "${HOME}/.jenv/bin" ]] &&  eval "$(jenv init -)"

# Use brew completions
[[ -x "/opt/homebrew/bin/brew" ]] && eval "$(brew shellenv)"

# shellcheck disable=SC1091
# curl -L https://iterm2.com/shell_integration/install_shell_integration.sh | bash
[[ -f "${HOME}/.iterm2_shell_integration.zsh" ]] && . "${HOME}/.iterm2_shell_integration.zsh"

# The next line updates PATH for the Google Cloud SDK.
# shellcheck disable=SC1091
[[ -f '/Users/howes/google-cloud-sdk/path.zsh.inc' ]] && . '/Users/howes/google-cloud-sdk/path.zsh.inc'

# The next line enables shell command completion for gcloud.
# shellcheck disable=SC1091
[[ -f '/Users/howes/google-cloud-sdk/completion.zsh.inc' ]] && . '/Users/howes/google-cloud-sdk/completion.zsh.inc'

# shellcheck disable=SC2206,SC3030
fpath=(${ASDF_DATA_DIR}/completions $fpath)

# Perform Emacs `eat` integration if enabled
# shellcheck disable=SC1090
[[ -n "${EAT_SHELL_INTEGRATION_DIR}" ]] && . "${EAT_SHELL_INTEGRATION_DIR}"

# Work-related -- add GitHub SSL key
[[ -f "${HOME}/.ssh/id_ed25519" ]] && ssh-add -q --apple-use-keychain "${HOME}/.ssh/id_ed25519"

mise="/Users/bradhowes/.local/bin/mise"
[[ -f "${mise}" ]] && eval "$(${mise} activate zsh)"

# dx shell completion
dx="/Users/bradhowes/.local/bin/dx"
[[ -f "${dx}" ]] && eval "$(${dx} completion zsh)"

# >>> dx ai-kit (managed — do not edit) >>>
[[ -f /Users/bradhowes/.traderepublic/ai-kit/ai-kit-env.sh ]] && . /Users/bradhowes/.traderepublic/ai-kit/ai-kit-env.sh
# <<< dx ai-kit <<<

echo "-- zshrc END"

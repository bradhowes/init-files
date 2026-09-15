# shellcheck shell=bash # -*- Mode: Sh; -*-

# tracer BEGIN aliases.sh

# Reload alias and function definitions
#
# shellcheck disable=SC2154
alias realias=". \${my_cfg}/aliases.sh"
# shellcheck disable=SC2154
alias refunc=". \${my_cfg}/functions.sh"

# Shortcut ls aliases
#
alias ls="ls -CF"
alias ll="ls -CFl"
alias llh="ls -CFlh"

# Show only processes associated with my user ID.
#
alias psme='ps -U ${USER} -o "pid,command" -ww'

alias r="fc -s"

alias whois="whois -h whois.arin.net"

alias hgrep="history | grep"

alias envgrep='env | grep'

alias sf="cd \${HOME}/src/Mine/SoundFonts"

alias ec="emacsclient -c -n"

alias scrolling="tput rmcup"

alias cfg="cd \${my_cfg}"

# Obtain the top-level directory for a git repository.
# NOTE: does not work if inside the '.git' directory.
alias git-top='git rev-parse --show-toplevel'

# Move to the top-level directory.
alias cd-top='cd -P -- "$(git-top)" && pwd'

# tracer END aliases.sh

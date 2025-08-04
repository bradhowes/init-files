# shellcheck shell=bash # -*- Mode: Sh; -*-

tracer BEGIN aliases.sh

# Reload alias and function definitions
#
# shellcheck disable=SC2154
alias realias=". \${my_cfg}/aliases"
# shellcheck disable=SC2154
alias refunc=". \${my_cfg}/functions"

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

tracer END aliases.sh

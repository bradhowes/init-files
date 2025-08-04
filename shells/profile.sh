# shellcheck shell=bash # -*- Mode: Sh; -*-

my_bashrc_loaded=""
# shellcheck disable=SC2034
my_profile_loaded="Y"

export my_arch
my_arch="$(uname -s)"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TZ="Europe/Paris"

export USER
[ -z "${USER}" ] && USER="${USERNAME}"
[ -z "${USER}" ] && USER="${LOGNAME}"

# shellcheck disable=SC1091,SC2154
[[ -f "${my_cfg}/functions.sh" ]] && . "${my_cfg}/functions.sh"

export INFOPATH
export MANPATH

INFOPATH=""
MANPATH=""

[[ -n "${PS1}" && -z "${my_bashrc_loaded}" ]] && . "${my_cfg}/bashrc.sh"

export PATH
PathAdd PATH \
        /Applications/Emacs.app/Contents/MacOS/bin \
        /opt/homebrew/bin \
        /opt/homebrew/sbin \
        /opt/homebrew/bin/opt/ruby/bin \
        /opt/homebrew/bin/lib/ruby/gems/3.1.0/bin \
        "${HOME}/bin"

export DOCKER_USER
DOCKER_USER="b.howes"

export WORKON_HOME
WORKON_HOME=${HOME}/venvs

# RubyGems env
export GEM_HOME="${HOME}/.gem"
PathAdd PATH "${GEM_HOME}/bin"

[[ "${my_arch}" = "Darwin" ]] && ulimit -n 8096
[[ -f "${HOME}/.pythonrc" ]] && export PYTHONSTARTUP="${HOME}/.pythonrc"

# shellcheck shell=bash

export my_cfg="${HOME}/src/Mine/init-files/shells"
# shellcheck disable=SC2155
export my_arch="$(uname -s)"

export USER
[[ -z "${USER}" ]] && USER="${USERNAME}"
[[ -z "${USER}" ]] && USER="${LOGNAME}"

# !!! Important to get rid of encoding errors in Python
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export WORKON_HOME="${HOME}/venvs/notebooks"

# shellcheck disable=SC1091
[[ -f "${HOME}/.local_envs" ]] && . "${HOME}/.local_envs"

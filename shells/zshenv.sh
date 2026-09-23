# shellcheck shell=bash

export my_cfg="${HOME}/src/Mine/init-files/shells"
# shellcheck disable=SC2155
export my_arch="$(/usr/bin/uname -s)"

export USER
[[ -z "${USER}" ]] && USER="${USERNAME}"
[[ -z "${USER}" ]] && USER="${LOGNAME}"

# !!! Important to get rid of encoding errors in Python
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export WORKON_HOME="${HOME}/venvs/notebooks"

export ASDF_DATA_DIR="${HOME}/.asdf"

. "${my_cfg}/functions.sh"

export PATH
PathAdd PATH \
        /opt/homebrew/opt/python@3.14/libexec/bin/ \
        /opt/homebrew/opt/grep/libexec/gnubin \
        /opt/podman/bin \
        /Applications/Emacs.app/Contents/MacOS/bin \
        "${HOME}/bin" \
        "${ASDF_DATA_DIR}/shims" \
        "${HOME}/.jenv/bin" \
        /usr/local/bin

# Prepend homebrew paths even if already in PATH. This is due to the fact that Homebrew's own policy is to not shadow
# any Apple bits (wise), but this PATH is only being used in command-line entries, and I *want* to shadow Apple tools
# such as `jq`.
PathAdd -f PATH \
        /opt/homebrew/bin \
        /opt/homebrew/sbin

# shellcheck disable=SC1091
[[ -f "${HOME}/.local_envs" ]] && . "${HOME}/.local_envs"

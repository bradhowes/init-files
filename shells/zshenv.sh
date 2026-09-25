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

# Add a value to a ':' separated variable value (eg PATH) as long as it does not
# already exist in the given variable's value.
#
# \param append optional flag (-a) to append to variable instead of prepending
# \param append optional flag (-f) to force addition of value even if it exists in VAR
# \param VAR the name of the variable to change, such as PATH or LD_LIBRARY_PATH
# \param VALUE value to add
#
PathAdd() {
  local append="" force=""
  while [[ "${1#-}" != "${1}" ]]; do # Process anything that begins with a '-' character
    case "${1}" in
      -a) append="-a" ;;
      -f) force="-f" ;;
      *) echo "*** invalid option - '${1}' ***" ;;
    esac
    shift 1
  done

  local var="${1}"
  # shellcheck disable=SC2154
  [[ "${my_arch}" = "Darwin" && "${var}" = "LD_LIBRARY_PATH" ]] && var="DY${var}"
  shift 1

  eval local current="\$${var}"
  for each in "${@}"; do
    if [[ "${var}" = "PATH" && ! -d "${each}" ]]; then
      echo "*** directory '${each}' does not exist -- not adding to PATH"
      continue
    fi

    local check=":${current}:"
    # shellcheck disable=SC2295
    if [[ "${check%%:${each}:*}" = "${check}" || -n "${force}" ]]; then
      if [[ -n "${append}" ]]; then
	current="${current}${current:+:}${each}"
      else
        current="${each}${current:+:}${current}"
      fi
      if [[ "${var}" = "PATH" ]]; then
        # Update MANPATH and INFOPATH with some possible values
        local root="${each%/bin}"
        PathAdd ${append} ${force} MANPATH "${root}/man" "${root}/share/man"
        PathAdd ${append} ${force} INFOPATH "${root}/info" "${root}/share/info"
      fi
    fi
  done

  eval "${var}=\"${current}\""
}

# export_function PathAdd

# Setting up paths
export PATH
PathAdd PATH \
        /opt/homebrew/opt/python@3.14/libexec/bin/ \
        /opt/homebrew/opt/grep/libexec/gnubin \
        /Applications/Emacs.app/Contents/MacOS/bin \
        "${HOME}/bin" \
        "${HOME}/.jenv/bin" \
        /usr/local/bin

# Prepend homebrew paths even if already in PATH. This is due to the fact that Homebrew's own policy is to not shadow
# any Apple bits (wise), but this PATH is only being used in command-line entries, and I *want* to shadow Apple tools
# such as `jq`.
PathAdd -f PATH \
        /opt/homebrew/bin \
        /opt/homebrew/sbin

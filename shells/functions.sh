# shellcheck shell=bash # -*- Mode: Sh; -*-

tracer BEGIN functions.sh

let my_is_zsh=0
[[ "${SHELL%zsh}" != "${SHELL}" ]] && let my_is_zsh=1

let my_is_bash=0
[[ "${SHELL%bash}" != "${SHELL}" ]] && let my_is_bash=1

if ((my_is_bash)); then
    alias whence=type
    export_function() { export -f "${1?}"; }
else
    export_function() { :; }
fi

dbg() { : echo "--" "${@}" 1>&2; }

is_function() { > /dev/null declare -f -F "${1}"; }

# Add a value to a ':' separated variable value (eg PATH) as long as it does not
# already exist in the given variable's value.
#
# \param VAR the name of the variable to change, such as PATH or LD_LIBRARY_PATH
# \param VALUE value to add
#
PathAdd() { 
    local append=""
    while [[ "${1#-}" != "${1}" ]]; do # Process anything that begins with a '-' character
	case "${1}" in
	    -a) append="1" ;;
	    *) echo "*** invalid option - '${1}' ***" ;;
	esac
	shift 1
    done

    local var="${1}"
    # shellcheck disable=SC2154
    [[ "${my_arch}" = "Darwin" && "${var}" = "LD_LIBRARY_PATH" ]] && var="DY${var}"
    shift 1

    eval local current="\$${var}"n

    for each in "${@}"; do
        [[ -d "${each}" ]] || continue
        local check=":${current}:"
        # shellcheck disable=SC2295
        if [[ "${check%%:${each}:*}" == "${check}" ]]; then
            if [[ -n "${append}" ]]; then
	        current="${current}${current:+:}${each}"
            else
                current="${each}${current:+:}${current}"
            fi
            if [[ "${var}" == "PATH" ]]; then
                # Update MANPATH and INFOPATH with some possible values
                local root="${each%/bin}"
                PathAdd MANPATH "${root}/man" "${root}/share/man"
                PathAdd INFOPATH "${root}/info" "${root}/share/info"
            fi
        fi
    done

    # dbg "NEW: ${var} = '${current}'"
    eval "${var}=\"${current}\""
}

export_function PathAdd

doScreen() { ssh -o ServerAliveInterval=60 -tt "${1}" execd screen -dRR -U /usr/local/bin/emacs -nw; }

ztart() {
    case "${1}" in
	*.bz2) F="j";;
	*.tbz) F="j";;
	*.tar) F="";;
	*) F="z";;
    esac
    tar ${F}tvf "${@}"
}

ztarx() {
    case "${1}" in
	*.bz2) F="j";;
	*.tbz) F="j";;
	*.tar) F="";;
	*) F="z";;
    esac
    tar ${F}xvf "${@}"
}

if ((my_is_zsh)); then
    settitle () { print "\e]0;${*}\007"; }
else
    settitle () { printf "\033]0;%s\007" "${@}"; }
fi

declare -A __my_saved_venv_state

save_env() {
    local name=""
    for name in "${@}"; do
        # shellcheck disable=SC2140
        eval __my_saved_venv_state["${name}"]="\$${name}"
    done
}

export_function save_env

restore_env() {
    local name=""
    for name in "${!__my_saved_venv_state[@]}"; do
        eval "${name}"="${__my_saved_venv_state[${name}]}"
    done
}

export_function restore_env

activate() {
    local dir="${1%/}"          # remove any trailing /

    if [[ -z "${dir}" ]]; then
        echo "*** missing or empty DIR argument"
    elif [[ -f "${DIR}/bin/activate" ]]; then
        local tmp_ps1="${PS1}"
        # shellcheck disable=SC1091
        . "${dir}/bin/activate"
        PS1="(${dir##*/})${tmp_ps1}"
    else
        echo "*** invalid env '${dir}'"
    fi
}

export_function activate

venv() {
    local name="${1}"
    if [[ -z "${name}" ]]; then
        echo "*** missing or empty NAME argument"
    elif [[ -e "${name}" ]]; then
        echo "-- activating ${name} environment"
        activate "${name}"
    else
        echo "-- creating ${name} environment"
        python3 -m venv "${name}"
        activate "${name}"
        pip install -U pip
        [[ -f "requirements.txt" ]] && pip install -r requirements.txt
    fi
}

exit() {
    if [[ -n "${CONDA_SHLVL}" && "${CONDA_SHLVL}" != 0 ]]; then
        conda deactivate
    elif is_function deactivate; then
        deactivate
    else
        builtin exit "$@"
    fi
}

find-root() {
    local old="${PWD}"
    while [[ ! -d "./.git" ]]; do
        if [[ "${PWD}" = "/" ]]; then
            builtin cd "${old}" || return 1
            return 1
        fi
        builtin cd .. || return 1
    done

    pwd
    builtin cd "${old}" || return 1
    return 0
}

my_bash_history_sync() {
    builtin history -a          # append new lines to history file
    HISTFILESIZE=${HISTSIZE}    # purge oldest entries beyond HISTSIZE
    builtin history -c          # clear history
    builtin history -r          # reload history
}

history() {
    my_bash_history_sync
    builtin history "${@}"
}

my_prompt_command() {
    my_bash_history_sync
    if [[ -n "${INSIDE_EMACS}" ]]; then
        printf "\e]7;file://%s%Rs\e\\" "${HOSTNAME}" "${PWD}"
    else
        printf "\e]0;%s;\007" "${PWD}"
    fi
}

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

tracer END functions.sh

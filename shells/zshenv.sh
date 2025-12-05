export my_cfg="${HOME}/src/Mine/init-files/shells"
export my_arch="$(uname -s)"

export USER
[[ -z "${USER}" ]] && USER="${USERNAME}"
[[ -z "${USER}" ]] && USER="${LOGNAME}"

# !!! Important to get rid of encoding errors in Python
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export WORKON_HOME="${HOME}/venvs/notebooks"

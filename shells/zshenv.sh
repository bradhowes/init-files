export BRH_INIT_FILES="${HOME}/src/Mine/init-files/shells"
export BRH_ARCH="$(uname -s)"

export USER
[[ -z "${USER}" ]] && USER="${USERNAME}"
[[ -z "${USER}" ]] && USER="${LOGNAME}"

. "${BRH_INIT_FILES}/functions"

# !!! Important to get rid of encoding errors in Python
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export DOCKER_USER="b.howes"
export WORKON_HOME="${HOME}/venvs/notebooks"

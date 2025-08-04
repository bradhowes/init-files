# shellcheck shell=bash # -*- Mode: Sh -*-

my_repos="${HOME}/src/Mine"
my_local_cfg="${my_repos}/init-files/shells"
my_home_cfg="${HOME}/init-files/shells"

if [[ -d "${my_local_cfg}" ]]; then
    my_cfg="${my_local_cfg}"
elif [[ -d "${my_home_cfg}" ]]; then
    my_cfg="${my_home_cfg}"
else
    echo "*** no init-files/shells directory located"
fi

. "${my_cfg}/profile.sh"

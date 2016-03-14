#!/usr/bin/env zsh

# Source this script to get a convenient emaci function to send commands to emacs.
# It also tries to transfer all environment variables.
# Usage:
# emaci echo Hello World
# emaci 'echo $PWD'
# emaci './configure && make && make install'

emacienv () { echo -e $(declare -px | awk '{if (NR == 1) printf $0;else if ($0 !~ /^typeset -.*/ && last !~ /^typeset -ax.*/) printf "\\n"$0;else printf " && "$0;}{last=$0}')' && ' }
emaci () { emacsclient --eval "$(echo "(emaci/submit-job-comint \"$PWD\" \"$(emacienv)cd $PWD && $@\")")" }

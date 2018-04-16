#!/usr/bin/env bash

# Stop script when an error occurs
set -o errexit
set -o pipefail
set -o nounset
# For debugging purposes
set -o xtrace

readonly DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly outDIR="${DIR}/output"

_main() {
    # Number of experiments to launch
    local readonly N=30
    local readonly moadir="./Software/moa-release-2017.06b"
    local readonly basedir="."
    local readonly outputResultsDir="."

    if [ ! -d "${outDIR}" ]; then
        mkdir "${outDIR}"
    fi

    cd ${moadir}

    for i in $(seq $N)
    do
        java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask "${@:2}" \
             "-s (generators.RandomTreeGenerator -i $RANDOM)" \
             > "${outDIR}/${@:1:1}$i.csv"
    done
}

_usage () {
    local readonly script_name="$0"
    echo "Usage: $0 <outputfile> <args to moa>"
}


if [[ $# -eq 2 ]]
then
    _main "$@"
else
    _usage
fi

#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=c2sm
GIT_REMOTE=git@github.com:C2SM-RCM/cosmo-ghg.git
MODEL=cosmo-ghg

pushd ext

# Clone the repo if not already existing
if [[ ! -d "${MODEL}" ]]; then
    git clone --depth 1 -b ${BRANCH} ${GIT_REMOTE} ${MODEL}
fi

pushd ${MODEL}

. ../spack-c2sm/setup-env.sh
spack devbuildcosmo cosmo @develop %nvhpc cosmo_target=gpu ^mpich%nvhpc

popd

popd

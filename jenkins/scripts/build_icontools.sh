#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=master
GIT_REMOTE=git@github.com:C2SM/icontools.git
MODEL=icontools
COMPILER=gcc

pushd ext

# Clone the repo if not already existing
if [[ ! -d "${MODEL}" ]]; then
    git clone --depth 1 -b ${BRANCH} ${GIT_REMOTE} ${MODEL}
fi

pushd ${MODEL}

. ../spack-c2sm/setup-env.sh
spack dev-build ${MODEL}@${BRANCH}%${COMPILER}

popd

popd

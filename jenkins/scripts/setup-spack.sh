#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=v0.18.1.5
GIT_REMOTE=https://github.com/C2SM/spack-c2sm.git

rm -fr src/spack-c2sm

pushd src
git clone --depth 1 --recurse-submodules --shallow-submodules -b ${BRANCH} ${GIT_REMOTE}
. spack-c2sm/setup-env.sh
popd

spack install icontools


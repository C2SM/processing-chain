#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=c2sm-features
GIT_REMOTE=git@github.com:C2SM-RCM/int2lm.git

pushd ext
# Activate spack
. spack-c2sm/setup-env.sh

# Remove int2lm folder (if existing)
rm -fr int2lm

# Clone and build int2lm
git clone --depth 1 -b ${BRANCH} ${GIT_REMOTE}
    pushd int2lm
    spack dev-build int2lm @dev-build %nvhpc
    popd
popd

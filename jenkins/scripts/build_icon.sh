#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

ICON_BRANCH=main
GIT_REMOTE=git@github.com:C2SM/icon.git

# Remove icon folder (if existing)
rm -fr src/icon

# Activate spack
. spack-c2sm/setup-env.sh

pushd src
# Clone icon
git clone -b ${ICON_BRANCH} ${GIT_REMOTE}
    pushd icon
    spack env activate -p -d config/cscs/spack/v0.18.1.4/daint_cpu_nvhpc
    spack install -u build
    popd
popd

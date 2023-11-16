#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=main
GIT_REMOTE=git@github.com:C2SM/icon.git

pushd externals
# Activate spack
. spack-c2sm/setup-env.sh

# Remove icon folder (if existing)
rm -fr icon

# Clone icon
git clone --depth 1 --recurse-submodules --shallow-submodules -b ${BRANCH} ${GIT_REMOTE} icon
SPACK_TAG=`cat icon/config/cscs/SPACK_TAG`
    pushd icon
    spack env activate -p -d config/cscs/spack/${SPACK_TAG}/daint_cpu_nvhpc
    spack install -u build
    popd
popd

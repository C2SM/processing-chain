#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

BRANCH=art
GIT_REMOTE=git@github.com:C2SM/icon.git

pushd src
# Activate spack
. spack-c2sm/setup-env.sh

# Remove icon-art folder (if existing)
rm -fr icon-art

# Clone icon-art
git clone --depth 1 --recurse-submodules --shallow-submodules -b ${BRANCH} ${GIT_REMOTE} icon-art
SPACK_TAG=`cat icon-art/config/cscs/SPACK_TAG`
    pushd icon-art
    spack env activate -p -d config/cscs/spack/${SPACK_TAG}/daint_cpu_nvhpc_art
    spack install -u build
    popd
popd

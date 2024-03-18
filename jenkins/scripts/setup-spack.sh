#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

if [[ $(hostname) == eu-* ]]; then
    source /cluster/apps/local/env2lmod.sh
    module load git/2.31.1
    SPACK_TAG=main
elif [[ $(hostname) == daint* ]]; then 
    git clone --depth 1 git@github.com:C2SM/icon.git icon-tag
    SPACK_TAG=`cat icon-tag/config/cscs/SPACK_TAG_DAINT`
    rm -fr icon-tag
elif [[ $(hostname) == balfrin* ]]; then 
    git clone --depth 1 git@github.com:C2SM/icon.git icon-tag
    SPACK_TAG=`cat icon-tag/config/cscs/SPACK_TAG_BALFRIN`
    rm -fr icon-tag
else
    error "Unknown hostname: $(hostname)"
fi

GIT_REMOTE=https://github.com/C2SM/spack-c2sm.git

rm -fr ext/spack-c2sm

pushd ext

git clone --depth 1 --recurse-submodules --shallow-submodules -b ${SPACK_TAG} ${GIT_REMOTE}

popd

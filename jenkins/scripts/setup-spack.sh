#!/bin/bash

set -e -x

source jenkins/scripts/common.sh

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

SPACK_TAG_COSMO=v0.18.1.12

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

# Clone Spack for ICON
git clone --depth 1 --recurse-submodules --shallow-submodules -b ${SPACK_TAG} ${GIT_REMOTE} spack-c2sm

# Clone Spack for COSMO-GHG
git clone --depth 1 --recurse-submodules --shallow-submodules -b ${SPACK_TAG_COSMO} ${GIT_REMOTE} spack-c2sm-cosmo

popd

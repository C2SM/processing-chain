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

pushd src
# Activate spack
. spack-c2sm/setup-env.sh

# Remove cosmo-ghg folder (if existing)
rm -fr cosmo-ghg

# Clone cosmo-ghg
git clone -b ${BRANCH} ${GIT_REMOTE}
    pushd cosmo-ghg
    spack devbuildcosmo cosmo @develop %nvhpc cosmo_target=gpu ^mpich%nvhpc
    popd
popd

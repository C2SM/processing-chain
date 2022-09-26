#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

# source spack temp instance
. spack-c2sm/spack/share/spack/setup-env.sh

SPACK_SPEC=$(cat cases/cosmo-ghg-11km-test/cosmo_spec)
spack installcosmo -v ${SPACK_SPEC}

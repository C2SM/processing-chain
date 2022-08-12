#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

SPACK_BRANCH='master'
GIT_REMOTE='https://github.com/C2SM/spack-c2sm.git'

SPACK_SPEC=$(cat cases/cosmo-ghg-11km-test/cosmo_spec)

rm -fr spack-c2sm
git clone -b ${SPACK_BRANCH} ${GIT_REMOTE}

pushd spack-c2sm
# install spack temp instance with branch config files and mch spack packages
./config.py -m daint -i . -r ./spack/etc/spack -p $PWD/spack -s $PWD/spack -u ON -c ./spack-cache
# source spack temp instance
. spack/share/spack/setup-env.sh
popd

spack installcosmo -v ${SPACK_SPEC}

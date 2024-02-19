#!/bin/bash

set -e -x

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

source jenkins/scripts/common.sh

BRANCH=art
GIT_REMOTE=git@github.com:C2SM/icon.git
MODEL=icon-art

pushd ext

# Clone the repo if not already existing
if [[ ! -d "${MODEL}" ]]; then
    git clone --depth 1 --recurse-submodules -b ${BRANCH} ${GIT_REMOTE} ${MODEL}
fi

pushd ${MODEL}

if [[ $(hostname) == eu-* ]]; then
    ./jenkins/scripts/jenkins_euler.sh -b -fc gcc --configure euler.cpu.gcc.O2
else
    SPACK_TAG=`cat config/cscs/SPACK_TAG`
    . ../spack-c2sm/setup-env.sh
    spack env activate -d config/cscs/spack/${SPACK_TAG}/daint_cpu_nvhpc
    spack install -u build
fi

popd

popd

#!/bin/bash

set -e -x

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

source jenkins/scripts/common.sh

BRANCH=release-2025.10-public
GIT_REMOTE=https://gitlab.dkrz.de/icon/icon-model.git
MODEL=icon

pushd ext

# Clone the repo if not already existing
if [[ ! -d "${MODEL}" ]]; then
    git clone --depth 1 --recurse-submodules -b ${BRANCH} ${GIT_REMOTE} ${MODEL}
fi

pushd ${MODEL}

if [[ $(hostname) == eu-* ]]; then
    # Load necessary modules
    module load stack/2025-06 git eth_proxy
    # Setup spack
    SPACK_TAG=$(cat "config/ethz/SPACK_TAG_EULER")
    git clone --depth 1 --recurse-submodules --shallow-submodules -b ${SPACK_TAG} https://github.com/C2SM/spack-c2sm.git
    . spack-c2sm/setup-env.sh
    # Build ICON
    spack env activate -d config/ethz/spack/${SPACK_TAG}/euler_cpu_gcc
    srun -N 1 -n 12 --mem-per-cpu=1G spack install -j 12
else
    error "Unknown hostname: $(hostname)"
fi

popd

popd

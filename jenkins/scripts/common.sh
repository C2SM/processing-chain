#!/bin/bash

function error {
  echo "*** Error: $@" >&2
  exit 1
}

function clone_and_build_package {
  BRANCH=$1
  GIT_REMOTE=$2
  MODEL=$3
  PACKAGE=$4
  VERSION=$5
  COMPILER=$6
  BUILD=$7
  FLAGS=${8:-}

  pushd ext

  # Clone the repo if not already existing
  if [[ ! -d "${MODEL}" ]]; then
      git clone --depth 1 --recurse-submodules -b ${BRANCH} ${GIT_REMOTE} ${MODEL} || error "Failed to clone repository"
  fi

  pushd ${MODEL}

  if [[ "${MODEL}" == cosmo-ghg ]]; then
    . ../spack-c2sm-cosmo/setup-env.sh
  else
    . ../spack-c2sm/setup-env.sh
  fi
  spack ${BUILD} -u build ${PACKAGE}@${VERSION}%${COMPILER} ${FLAGS}  || error "Failed to build ${PACKAGE}"

  popd

  popd
}


#!/bin/bash

function error {
  echo "*** Error: $@" >&2
  exit 1
}

function clone_and_build_package {
  BRANCH=$1
  GIT_REMOTE=$2
  PACKAGE=$3
  VERSION=$4
  COMPILER=$5
  BUILD=$6
  FLAGS=${7:-}

  pushd ext

  # Clone the repo if not already existing
  if [[ ! -d "${PACKAGE}" ]]; then
      git clone --depth 1 --recurse-submodules -b ${BRANCH} ${GIT_REMOTE} ${PACKAGE} || error "Failed to clone repository"
  fi

  pushd ${PACKAGE}

  . ../spack-c2sm/setup-env.sh
  spack ${BUILD} ${PACKAGE}@${VERSION}%${COMPILER} ${FLAGS}  || error "Failed to build ${PACKAGE}"

  popd

  popd
}


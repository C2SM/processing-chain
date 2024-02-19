#!/bin/bash

set -e -x

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

source jenkins/scripts/common.sh

BRANCH=c2sm
GIT_REMOTE=git@github.com:C2SM-RCM/cosmo-ghg.git
PACKAGE=cosmo-ghg
VERSION=develop
COMPILER=nvhpc
BUILD=devbuildcosmo
FLAGS="cosmo_target=gpu ^mpich%nvhpc"

clone_and_build_package "${BRANCH}" "${GIT_REMOTE}" "${PACKAGE}" "${VERSION}" "${COMPILER}" "${BUILD}" "${FLAGS}"

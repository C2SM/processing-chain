#!/bin/bash

set -e -x

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

source jenkins/scripts/common.sh

BRANCH=c2sm-features
GIT_REMOTE=git@github.com:C2SM-RCM/int2lm.git
PACKAGE=int2lm
VERSION=c2sm-features
COMPILER=nvhpc
BUILD=dev-build

clone_and_build_package "${BRANCH}" "${GIT_REMOTE}" "${PACKAGE}" "${VERSION}" "${COMPILER}" "${BUILD}" 

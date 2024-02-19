#!/bin/bash

set -e -x

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

source jenkins/scripts/common.sh

BRANCH=master
GIT_REMOTE=git@github.com:C2SM/icontools.git
PACKAGE=icontools
VERSION=c2sm-master
COMPILER=gcc
BUILD=dev-build

clone_and_build_package "${BRANCH}" "${GIT_REMOTE}" "${PACKAGE}" "${VERSION}" "${COMPILER}" "${BUILD}" 

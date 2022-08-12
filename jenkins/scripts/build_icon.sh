#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

rm -fr icon
git clone git@github.com:C2SM/icon.git

pushd icon
  git submodule update --init
  ./config/cscs/c2sm/daint.cpu.gcc.O2
  make -j8
popd


#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

mkdir -p input
pushd input
    wget ftp://iacftp.ethz.ch/pub_read/mjaehn/input_processing-chain/input_processing-chain.tgz 
    tar -xvzf input_processing-chain.tgz
    rm -f input_processing-chain.tgz
popd

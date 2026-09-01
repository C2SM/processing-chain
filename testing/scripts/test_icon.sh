#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

if [[ $(hostname) == eu-* ]]; then
    python run_chain.py icon-test-euler -f
else
    python run_chain.py icon-test -f
fi


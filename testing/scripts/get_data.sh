#!/bin/bash

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

URL=https://data.iac.ethz.ch/c2sm-ci-input/processing-chain/input_processing-chain.tgz
ARCHIVE=input_processing-chain.tgz

# Sanity check after extraction, so that a truncated download fails here
# instead of halfway through a chain run. icon-test-euler is not covered:
# it reads its input from a shared directory, staged by
# testing/scripts/stage_icon-test-euler_input.sh.
REQUIRED=(
    icon/grid/VERIFY_DOM_DOM01.nc
    icon/rad/rrtmg_lw.nc
)

mkdir -p input
pushd input
    # -c resumes a partial download instead of starting a second .tgz.1
    wget -c -O "${ARCHIVE}" "${URL}" || error "failed to download ${URL}"
    tar -xzf "${ARCHIVE}" || error "failed to extract ${ARCHIVE} (truncated download?)"

    missing=()
    for f in "${REQUIRED[@]}"; do
        [[ -e "$f" ]] || missing+=("$f")
    done
    if [[ ${#missing[@]} -gt 0 ]]; then
        error "input archive is missing: ${missing[*]}"
    fi

    rm -f "${ARCHIVE}"
popd

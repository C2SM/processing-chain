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

# Verification check performed after extraction, so that a truncated
# download is detected here rather than during a later chain run.
# icon-test-euler is not included, as it reads its input from a shared
# directory staged by testing/scripts/stage_icon-test-euler_input.sh.
REQUIRED=(
    icon/grid/VERIFY_DOM_DOM01.nc
    icon/rad/rrtmg_lw.nc
)

mkdir -p input
pushd input
    # The -c option resumes an incomplete download instead of creating a
    # second archive file (.tgz.1)
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

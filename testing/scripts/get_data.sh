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

# Files the test cases need; checked after extraction so that a truncated
# download fails here instead of halfway through a chain run.
REQUIRED=(
    icon/grid/icon_grid_0002_R02B06_G.nc
    icon/grid/icon_extpar_0002_R02B06_G.nc
    icon/rad/rrtmg_lw.nc
    icon/rad/rrtm_cldopt.nc
    icon/mapping/map_file.ana
    era5/era5_ml_2018-01-01.grib
    era5/era5_surf_2018-01-01.grib
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

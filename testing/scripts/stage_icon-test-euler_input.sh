#!/bin/bash
#
# Stage the input data for the icon-test-euler case on Euler.
#
# The case reads its grid, extpar and ERA5 files straight from a shared
# directory rather than from the input/ archive, so this only has to be run
# once per system (and again if the case period changes).
#
# Usage:
#   ./testing/scripts/stage_icon-test-euler_input.sh
#
# Override the destination with PROC_CHAIN_INPUT_DIR; it must match the
# paths in cases/icon-test-euler/config.yaml.

set -e -x

function error {
    echo "*** Error: $@" >&2
    exit 1
}

[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

DEST=${PROC_CHAIN_INPUT_DIR:-/cluster/work/climate/icon_input/processing-chain/icon-test-euler}

# Global R02B06 grid and its matching extpar (same uuidOfHGrid)
GRID_SRC=/cluster/work/climate/icon_input/DWD_Tutorial2017/test_cases/case2/input
GRID_FILES=(
    icon_grid_0024_R02B06_G.nc
    icon_extpar_0024_R02B06_G_20150805_tiles.nc
)

# ERA5 initial conditions for the case period (see startdate in config.yaml)
ERA5_SRC=/cluster/work/climate/lroither/icon_c2sm/icon_era5_global/era5_raw
ERA5_DATE=${ERA5_DATE:-2013-05-25}
ERA5_FILES=(
    "era5_ml_${ERA5_DATE}.grib"
    "era5_surf_${ERA5_DATE}.grib"
)

mkdir -p "${DEST}/grid" "${DEST}/era5"

for f in "${GRID_FILES[@]}"; do
    [[ -f "${GRID_SRC}/$f" ]] || error "missing source file ${GRID_SRC}/$f"
    [[ -f "${DEST}/grid/$f" ]] && continue
    cp -v "${GRID_SRC}/$f" "${DEST}/grid/"
done

for f in "${ERA5_FILES[@]}"; do
    [[ -f "${ERA5_SRC}/$f" ]] || error "missing source file ${ERA5_SRC}/$f"
    [[ -f "${DEST}/era5/$f" ]] && continue
    cp -v "${ERA5_SRC}/$f" "${DEST}/era5/"
done

echo "Staged icon-test-euler input under ${DEST}"

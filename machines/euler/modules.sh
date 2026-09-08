#!/usr/bin/env bash
# machines/euler/modules.sh
#
# Load the system modules required by the Processing Chain on Euler.
# This file can be sourced standalone in interactive sessions or
# included at the top of Slurm job scripts.
#
# The compiler, MPI, cdo, nco and netcdf-c versions are kept consistent with
# the Euler site settings of cesm2icon, so that the ICON executable used by
# the Processing Chain (see icon.binary_file in the case configuration) runs
# in the environment for which it was built and tested:
#   https://github.com/C2SM/cesm2icon/blob/main/run/sites/euler.sh
#
# Usage:
#   source machines/euler/modules.sh

module load stack/2025-06 gcc/12.2.0 openmpi/4.1.7
module load cdo/2.4.4 nco/5.2.4 netcdf-c/4.9.2
module load python/3.13.0

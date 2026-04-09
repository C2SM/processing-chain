#!/usr/bin/env bash
# machines/euler/modules.sh
#
# Load the system modules required by the Processing Chain on Euler.
# This file can be sourced standalone in interactive sessions or
# included at the top of Slurm job scripts.
#
# Usage:
#   source machines/euler/modules.sh

module load stack/2024-06 gcc/12.2.0 openmpi/4.1.6 python/3.12.8
module load cdo/2.2.2 nco/5.1.6 netcdf-c/4.9.2

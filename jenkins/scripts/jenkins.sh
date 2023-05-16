#!/bin/bash

set -e -x

# Activate conda environment
conda activate proc-chain

# Setup spack
./jenkins/scripts/setup-spack.sh

# Preparation
./get_data.sh

# Build COSMO-GHG
./jenkins/scripts/build_cosmo-ghg.sh

# Build ICON
./jenkins/scripts/build_icon.sh

# Test COSMO-GHG
python run_chain.py cosmo-ghg-11km-test 2015-01-01 0 24 -f

# Test ICON
python run_chain.py icon-test 2018-01-01 0 24 -j prepare_data icon -f

# Print success message
echo "Success!"

#!/bin/bash

set -e -x

# Activate conda environment
eval "$(conda shell.bash hook)"
conda activate proc-chain

# Setup spack
if [[ -d src/spack-c2sm ]]; then
  echo spack folder already exists - activating spack...
  . src/spack-c2sm/setup-env.sh
else
  echo building spack...
  ./jenkins/scripts/setup-spack.sh
fi

# Preparation
size=$(du -sb input | awk '{print $1}')
if [[ $size -gt 40000000000 ]]; then
  echo input data already present - skipping download...
else
  echo downloading input data...
  ./jenkins/scripts/get_data.sh
fi

# Build int2lm
if [[ -f src/int2lm/test/testsuite/int2lm ]]; then
  echo int2lm executable already exists - skipping build...
else
  echo building int2lm...
  ./jenkins/scripts/build_int2lm.sh
fi

# Build COSMO-GHG
if [[ -f src/cosmo-ghg/cosmo/ACC/cosmo_gpu ]]; then
  echo cosmo executable already exists - skipping build.
else
  echo building cosmo...
  ./jenkins/scripts/build_cosmo-ghg.sh
fi

# Build ICON
if [[ -f src/icon/bin/icon ]]; then
  echo icon executable already exists - skipping build.
else
  echo building icon...
  ./jenkins/scripts/build_icon.sh
fi

# Test COSMO-GHG
if [[ -f work/cosmo-ghg-11km-test/2015010112_-6_12/checkpoints/finished/post_cosmo ]]; then
  echo cosmo-ghg test case already finished - skipping test.
else
  echo running cosmo-ghg test case...
  python run_chain.py cosmo-ghg-11km-test 2015-01-01 0 24 -f
fi

# Test ICON
echo running icon test case...
python run_chain.py icon-test 2018-01-01 0 24 -j prepare_data icon -f

# Print success message
echo "Success!"

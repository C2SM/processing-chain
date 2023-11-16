#!/bin/bash

# Argument parsing
force_execution=false

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -f|--force)
            force_execution=true
            shift
            ;;
        *)
            echo "Unknown parameter: $1"
            exit 1
            ;;
    esac
done

set -e -x

# Activate conda environment
eval "$(conda shell.bash hook)"
conda activate proc-chain

# Setup spack
if [[ -d externals/spack-c2sm ]]; then
  echo spack folder already exists - activating spack...
  . externals/spack-c2sm/setup-env.sh
else
  echo building spack...
  ./jenkins/scripts/setup-spack.sh
fi

# Preparation
size=$(du -sb input | awk '{print $1}')
if [[ $size -gt 12000000000 ]]; then
  echo input data already present - skipping download...
else
  echo downloading input data...
  ./jenkins/scripts/get_data.sh
fi

# Build int2lm
if [[ -f externals/int2lm/test/testsuite/int2lm ]]; then
  echo int2lm executable already exists - skipping build...
else
  echo building int2lm...
  ./jenkins/scripts/build_int2lm.sh
fi

# Build COSMO-GHG
if [[ -f externals/cosmo-ghg/cosmo/ACC/cosmo_gpu ]]; then
  echo cosmo executable already exists - skipping build.
else
  echo building cosmo...
  ./jenkins/scripts/build_cosmo-ghg.sh
fi

# Build ICON
if [[ -f externals/icon/bin/icon ]]; then
  echo icon executable already exists - skipping build.
else
  echo building icon...
  ./jenkins/scripts/build_icon.sh
fi

# Build ICON-ART
if [[ -f externals/icon-art/bin/icon ]]; then
  echo icon-art executable already exists - skipping build.
else
  echo building icon-art...
  ./jenkins/scripts/build_icon-art.sh
fi

# Test COSMO-GHG
if [[ -f work/cosmo-ghg-test/2015010100_6_12/checkpoints/finished/post_cosmo && "$force_execution" == false ]]; then
  echo cosmo-ghg test case already finished - skipping test.
else
  echo running cosmo-ghg test case...
  ./jenkins/scripts/test_cosmo-ghg.sh
fi

# Test COSMO-GHG (spinup)
if [[ -f work/cosmo-ghg-spinup-test/2015010106_-3_6/checkpoints/finished/post_cosmo && "$force_execution" == false ]]; then
  echo cosmo-ghg test case already finished - skipping test.
else
  echo running cosmo-ghg-spinup test case...
  ./jenkins/scripts/test_cosmo-ghg-spinup.sh
fi

# Test ICON
if [[ -f work/icon-test/2018010100_6_12/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon test case already finished - skipping test.
else
  echo running icon test case...
  ./jenkins/scripts/test_icon.sh
fi

# Test ICON-ART
if [[ -f work/icon-art-oem-test/2018010100_0_24/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon-art test case already finished - skipping test.
else
  echo running icon-art-oem test case...
  ./jenkins/scripts/test_icon-art-oem.sh
fi

# Test ICON-ART-GLOBAL
if [[ -f work/icon-art-global-test/2018010100_0_24/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon-art-global test case already finished - skipping test.
else
  echo running icon-art-global test case...
  ./jenkins/scripts/test_icon-art-global.sh
fi

# Print success message
echo "Success!"

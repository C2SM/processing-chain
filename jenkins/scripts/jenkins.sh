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

skip=false
# Check if we are on Euler
if [[ $(hostname) == eu-* ]]; then
    skip=true
fi

# Activate conda environment
eval "$(conda shell.bash hook)"
conda activate proc-chain

# Setup spack
if [[ -d ext/spack-c2sm ]]; then
  echo spack folder already exists - skipping build...
else
  echo building spack...
  ./jenkins/scripts/setup-spack.sh
fi
echo activating spack...
. ext/spack-c2sm/setup-env.sh

# Preparation
size=$(du -sb input | awk '{print $1}')
if [[ $size -gt 12000000000 ]]; then
  echo input data already present - skipping download...
else
  echo downloading input data...
  ./jenkins/scripts/get_data.sh
fi

# Build icontools
if [[ -f ext/icontools/icontools/iconremap ]]; then
  echo icontools already installed - skipping build...
else
  echo building icontools...
  ./jenkins/scripts/build_icontools.sh
fi

# Build int2lm
if [[ "$skip" == true ]]; then
  echo skipping int2lm build on Euler...
elif [[ -f ext/int2lm/test/testsuite/int2lm ]]; then
  echo int2lm executable already exists - skipping build...
else
  echo building int2lm...
  ./jenkins/scripts/build_int2lm.sh
fi

# Build COSMO-GHG
if [[ "$skip" == true ]]; then
  echo skipping cosmo-ghg build on Euler...
elif [[ -f ext/cosmo-ghg/cosmo/ACC/cosmo_gpu ]]; then
  echo cosmo executable already exists - skipping build.
else
  echo building cosmo...
  ./jenkins/scripts/build_cosmo-ghg.sh
fi

# Build ICON
if [[ -f ext/icon/bin/icon ]]; then
  echo icon executable already exists - skipping build.
else
  echo building icon...
  ./jenkins/scripts/build_icon.sh
fi

# Build ICON-ART
if [[ -f ext/icon-art/bin/icon ]]; then
  echo icon-art executable already exists - skipping build.
else
  echo building icon-art...
  ./jenkins/scripts/build_icon-art.sh
fi

# Test COSMO-GHG
if [[ "$skip" == true ]]; then
  echo skipping cosmo-ghg test on Euler...
elif [[ -f work/cosmo-ghg-test/2015010106_2015010112/checkpoints/finished/post_cosmo && "$force_execution" == false ]]; then
  echo cosmo-ghg test case already finished - skipping test.
else
  echo running cosmo-ghg test case...
  ./jenkins/scripts/test_cosmo-ghg.sh
fi

# Test COSMO-GHG (spinup)
if [[ "$skip" == true ]]; then
  echo skipping cosmo-ghg-spinup test on Euler...
elif [[ -f work/cosmo-ghg-spinup-test/2015010109_2015010118/checkpoints/finished/post_cosmo && "$force_execution" == false ]]; then
  echo cosmo-ghg-spinup test case already finished - skipping test.
else
  echo running cosmo-ghg-spinup test case...
  ./jenkins/scripts/test_cosmo-ghg-spinup.sh
fi

# Test ICON
if [[ -f work/icon-test/2018010106_2018010112/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon test case already finished - skipping test.
else
  echo running icon test case...
  ./jenkins/scripts/test_icon.sh
fi

# Test ICON-ART
if [[ -f work/icon-art-oem-test/2018010106_2018010112/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon-art test case already finished - skipping test.
else
  echo running icon-art-oem test case...
  ./jenkins/scripts/test_icon-art-oem.sh
fi

# Test ICON-ART-GLOBAL
if [[ -f work/icon-art-global-test/2018010106_2018010112/checkpoints/finished/icon && "$force_execution" == false ]]; then
  echo icon-art-global test case already finished - skipping test.
else
  echo running icon-art-global test case...
  ./jenkins/scripts/test_icon-art-global.sh
fi

# Print success message
echo "Success!"

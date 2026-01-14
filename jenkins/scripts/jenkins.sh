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

# Check if we are on Euler
if [[ $(hostname) == eu-* ]]; then
    host=euler
elif [[ $(hostname) == santis* ]]; then
    host=santis
else
    echo "Unknown hostname: $(hostname)"
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

# Build ICON
if [[ -f ext/icon/bin/icon ]]; then
  echo icon executable already exists - skipping build.
else
  echo building icon...
  ./jenkins/scripts/build_icon.sh
fi

# Test ICON
if [[ "$host" == euler ]]; then
    if [[ -f work/icon-test-euler/2018010106_2018010112/checkpoints/finished/icon && "$force_execution" == false ]]; then
      echo icon test case already finished - skipping test.
    else
      echo running icon test case...
      ./jenkins/scripts/test_icon.sh
    fi
fi

# Print success message
echo "Success!"

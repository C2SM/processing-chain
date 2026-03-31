#!/bin/bash


# Argument parsing
force_execution=false
use_pip=false

while [[ "$#" -gt 0 ]]; do
  case $1 in
    -f|--force)
      force_execution=true
      shift
      ;;
    --pip)
      use_pip=true
      shift
      ;;
    *)
      echo "Unknown parameter: $1"
      exit 1
      ;;
  esac
done

set -e -x

# Check if we are on Euler and load modules
if [[ $(hostname) == eu-* ]]; then
    host=euler
    module load stack/2024-06 gcc/12.2.0 openmpi/4.1.6 python/3.12.8 || true
    module load cdo/2.2.2 nco/5.1.6 netcdf-c/4.9.2 || true
elif [[ $(hostname) == santis* ]]; then
    host=santis
else
    echo "Unknown hostname: $(hostname)"
fi


# Build environment if not present
if [[ "$use_pip" == true ]]; then
  if [[ ! -d venv ]]; then
    echo "Creating Python venv and installing requirements..."
    ./jenkins/scripts/setup_env.sh --pip
  else
    echo "Python venv already exists - skipping build."
  fi
  source venv/bin/activate
else
  if ! conda info --envs | grep -q "proc-chain"; then
    echo "Creating conda environment..."
    ./jenkins/scripts/setup_env.sh
  else
    echo "Conda environment 'proc-chain' already exists - skipping build."
  fi
  eval "$(conda shell.bash hook)"
  conda activate proc-chain
fi

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

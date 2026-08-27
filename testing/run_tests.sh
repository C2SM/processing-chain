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

# Prepare the environment (machine-specific)
if [[ $(hostname) == eu-* ]]; then
    host=euler
    source machines/euler/modules.sh || true
elif [[ $(hostname) == santis* ]]; then
    host=santis
else
    echo "Unknown hostname: $(hostname)"
fi


# Build environment if not present
if [[ "$use_pip" == true ]]; then
  if [[ ! -d venv ]]; then
    echo "Creating Python venv and installing requirements..."
    python3 -m venv venv
    venv/bin/pip install -r requirements.txt
  else
    echo "Python venv already exists - skipping build."
  fi
  source venv/bin/activate
else
  if ! conda info --envs | grep -q "proc-chain"; then
    echo "Creating conda environment..."
    conda env create -f environment.yml
  else
    echo "Conda environment 'proc-chain' already exists - skipping build."
  fi
  eval "$(conda shell.bash hook)"
  conda activate proc-chain
fi

# Preparation
size=$(du -sb input 2>/dev/null | awk '{print $1}')
if [[ ${size:-0} -gt 12000000000 ]]; then
  echo input data already present - skipping download...
else
  echo downloading input data...
  ./testing/scripts/get_data.sh
fi

# Test ICON
if [[ "$host" == euler ]]; then
    if [[ -f work/icon-test-euler/2018010106_2018010112/checkpoints/finished/icon && "$force_execution" == false ]]; then
      echo icon test case already finished - skipping test.
    else
      echo running icon test case...
      ./testing/scripts/test_icon.sh
    fi
fi

# Print success message
echo "Success!"

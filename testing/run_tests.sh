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
if [[ ! -d .venv ]]; then
  echo "Creating Python venv and installing requirements..."
  python3 -m venv .venv
  .venv/bin/pip install -r requirements.txt
else
  echo "Python venv already exists - skipping build."
fi
source .venv/bin/activate

# Prepare and run the test case.
# icon-test-euler reads its grid, extpar and ERA5 from a shared directory,
# so nothing has to be downloaded here. The other cases still use
# ./testing/scripts/get_data.sh, which has to be run separately.
if [[ "$host" == euler ]]; then
  ./testing/scripts/stage_icon-test-euler_input.sh

  # last chunk of the case period (see startdate/enddate in config.yaml)
  if [[ -f work/icon-test-euler/2013052506_2013052512/checkpoints/finished/icon && "$force_execution" == false ]]; then
    echo icon test case already finished - skipping test.
  else
    echo running icon test case...
    ./testing/scripts/test_icon.sh
  fi
fi

# Print success message
echo "Success!"

#!/usr/bin/env bash
# machines/euler/setup_env.sh
#
# Full environment setup for the Processing Chain on Euler.
# Sources the required system modules and activates the Python
# virtual environment.
#
# Usage (interactive session):
#   source machines/euler/setup_env.sh
#
# The virtual environment is expected at <repo_root>/venv by default.
# Override by setting PROC_CHAIN_VENV before sourcing:
#   export PROC_CHAIN_VENV=/path/to/your/venv
#   source machines/euler/setup_env.sh

# Resolve the repository root relative to this script's location
_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_REPO_ROOT="$(cd "${_SCRIPT_DIR}/../.." && pwd)"

# 1) Load system modules
source "${_SCRIPT_DIR}/modules.sh"

# 2) Activate the Python virtual environment (pip)
_VENV_DIR="${PROC_CHAIN_VENV:-${_REPO_ROOT}/venv}"
if [[ -f "${_VENV_DIR}/bin/activate" ]]; then
    source "${_VENV_DIR}/bin/activate"
    echo "Processing Chain environment activated (${_VENV_DIR})"
else
    echo "Warning: virtual environment not found at '${_VENV_DIR}'" >&2
    echo "Create it first:" >&2
    echo "  python3 -m venv ${_VENV_DIR}" >&2
    echo "  pip install -r ${_REPO_ROOT}/requirements.txt" >&2
fi

unset _SCRIPT_DIR _REPO_ROOT _VENV_DIR

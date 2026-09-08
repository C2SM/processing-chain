#!/usr/bin/env bash
# machines/santis/setup_env.sh
#
# Full environment setup for the Processing Chain on Santis.
# Starts the required uenv and activates the Python virtual environment
# inside it.
#
# IMPORTANT: 'uenv start' launches a NEW interactive shell.
# Sourcing this script directly is therefore NOT supported.
# Instead, run it as an executable to enter a prepared shell:
#
#   bash machines/santis/setup_env.sh
#
# Or, to activate the venv from a session already inside the uenv:
#
#   source machines/santis/setup_env.sh --no-uenv
#
# The virtual environment is expected at <repo_root>/.venv by default.
# Override by setting PROC_CHAIN_VENV before calling:
#   export PROC_CHAIN_VENV=/path/to/your/venv
#   bash machines/santis/setup_env.sh

_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_REPO_ROOT="$(cd "${_SCRIPT_DIR}/../.." && pwd)"

UENV_IMAGE="climtools/25.2:v1"
UENV_VIEW="climtools"
_VENV_DIR="${PROC_CHAIN_VENV:-${_REPO_ROOT}/.venv}"

_activate_venv() {
    if [[ -f "${_VENV_DIR}/bin/activate" ]]; then
        source "${_VENV_DIR}/bin/activate"
        echo "Processing Chain environment activated (${_VENV_DIR})"
    else
        echo "Warning: virtual environment not found at '${_VENV_DIR}'" >&2
        echo "Create it first (inside the uenv):" >&2
        echo "  python3 -m venv ${_VENV_DIR}" >&2
        echo "  pip install -r ${_REPO_ROOT}/requirements.txt" >&2
    fi
}

if [[ "${1:-}" == "--no-uenv" ]]; then
    # Already inside the uenv – just activate the venv
    _activate_venv
else
    # Launch a new shell inside the uenv and activate the venv there
    uenv start "${UENV_IMAGE}" --view="${UENV_VIEW}" -- bash -c \
        "source '${_VENV_DIR}/bin/activate' 2>/dev/null \
         && echo 'Processing Chain environment activated (${_VENV_DIR})' \
         || echo 'Warning: venv not found at ${_VENV_DIR}. Run: python3 -m venv ${_VENV_DIR} && pip install -r ${_REPO_ROOT}/requirements.txt' >&2; \
         exec bash"
fi

unset _SCRIPT_DIR _REPO_ROOT _VENV_DIR
unset -f _activate_venv

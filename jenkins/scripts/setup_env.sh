#!/bin/bash


set -ex

function error {
    echo "*** Error: $@" >&2
    exit 1
}

# Check if script is called correctly
[[ $(git rev-parse --show-toplevel 2>/dev/null) = $(pwd) ]] || error "$0 not launched from toplevel of repository"

# Set WORKSPACE to CWD if unset
if [[ -z "$WORKSPACE" ]]; then
	export WORKSPACE="$(pwd)"
fi

# Check for --pip argument
if [[ "$1" == "--pip" ]]; then
	# Create venv and install with pip
	python3 -m venv --system-site-packages "$WORKSPACE/venv"
	source "$WORKSPACE/venv/bin/activate"
	pip install -r "$WORKSPACE/requirements.txt"
	deactivate
else
	# Use Miniforge installer only for conda
	wget -O miniforge.sh https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
	rm -fr miniforge
	bash miniforge.sh -b -p "$WORKSPACE/miniforge"
	export PATH="$WORKSPACE/miniforge/bin:$PATH"
	conda config --set always_yes yes --set changeps1 no
	conda config --add channels conda-forge
	conda update -n base -c defaults conda
	conda env create -f "$WORKSPACE/environment.yml"
	source "$WORKSPACE/miniforge/etc/profile.d/conda.sh"
	conda activate proc-chain
	conda deactivate
	rm miniforge.sh
fi
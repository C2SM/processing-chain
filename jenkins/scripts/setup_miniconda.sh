#!/bin/bash

set -e -x

wget -O miniconda.sh https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
rm -fr miniconda
bash miniconda.sh -b -p "$WORKSPACE/miniconda"
export PATH="$WORKSPACE/miniconda/bin:$PATH"
conda config --set always_yes yes --set changeps1 no
conda config --add channels conda-forge
conda update -n base -c defaults conda
conda env create -f env/environment.yml
source "$WORKSPACE/miniconda/etc/profile.d/conda.sh"
conda activate proc-chain
conda deactivate
rm miniconda.sh
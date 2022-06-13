#!/bin/bash

#PROJECT_FOLDER=/project/g110/processing-chain
#rm -rf ${PROJECT_FOLDER}
#mkdir -p ${PROJECT_FOLDER}
PROJECT_FOLDER=/scratch/snx3000/mjaehn/Maintenance/processing-chain

# load python3
module load daint-gpu cray-python
export EASYBUILD_PREFIX=${PROJECT_FOLDER}
module load EasyBuild-custom
eb GEOS-3.10.2-CrayGNU-21.09-python3.eb -r
module load GEOS PROJ

VENV_PATH=${PROJECT_FOLDER}/.venv

rm -rf $VENV_PATH
mkdir -p ${VENV_PATH}

python3 -m venv ${VENV_PATH}
source ${VENV_PATH}/bin/activate

pip install --upgrade pip
pip install -r env/requirements.txt


#!/usr/bin/env bash
#SBATCH --job-name="icon-art-global_2021070200_24"
#SBATCH --account=em05
#SBATCH --time=00:30:00
#SBATCH --nodes=2
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=36
#SBATCH --cpus-per-task=1
#SBATCH --partition=debug
#SBATCH --constraint=mc
#SBATCH --hint=nomultithread

# -- OpenMP environment variables                                                 
export OMP_NUM_THREADS=1                                                       
export ICON_THREADS=1                                                          
export OMP_SCHEDULE=static,12                                                  
export OMP_DYNAMIC="false"                                                     
export OMP_STACKSIZE=200M         

# -- ECCODES path                                               
export ECCODES_DEFINITION_PATH=/store/empa/em05/eccodes_definitions/definitions.edzw-2.12.5-2
export ECCODES_DEFINITION_PATH=$ECCODES_DEFINITION_PATH:/store/empa/em05/easybuild.backup/software/ecCodes/2.12.5-CrayGNU-20.08/share/eccodes/definitions
export ECCODES_DEFINITION_PATH=$ECCODES_DEFINITION_PATH:/project/g110/spack-install/daint/eccodes/2.19.0/pgi/6skdmw5lsn6mjv4esxkyalf6xogllshi/share/eccodes/definitions/

set -x

# srun -n 1 python run_chain.py icon-art-global 2021-07-02 0 48 -j prepare_data_global icon_global
srun --nodes=1 --ntasks=1 --ntasks-per-node=1 --cpus-per-task=1 python run_chain.py icon-art-global 2021-07-02 0 48 -j prepare_data_global icon_global
# python run_chain.py icon-art-global 2021-07-02 0 48 -j prepare_data_global icon_global
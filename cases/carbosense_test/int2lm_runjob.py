import os 

def main(cfg,logfile,logfile_finish):
    output_file = os.path.join(cfg.int2lm_work,"run.job")
    api_dir = "/users/morsier/install/libgrib_api/1.13.1"
    with open(output_file,"w") as of:
        to_write = f"""#!/bin/bash -l
#SBATCH --job-name=int2lm_{cfg.inidate_yyyymmddhh}_{cfg.forecasttime}
#SBATCH --time={cfg.int2lm_walltime}
#SBATCH --nodes={cfg.int2lm_nodes}
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node={cfg.int2lm_ntasks_per_node}
#SBATCH --cpus-per-task=1
#SBATCH --partition={cfg.compute_queue}
#SBATCH --constraint=gpu
#SBATCH --account={cfg.compute_account}
#SBATCH --output={logfile}
#SBATCH --workdir={cfg.int2lm_work}

# Export env variables
export OMP_NUM_THREADS=1
export CRAY_CUDA_MPS=1

export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912

# Set this to avoid segmentation faults
ulimit -s unlimited
ulimit -a

# Load modules for post-processing
module load daint-gpu
module load NCO
module list

unset G2G
export MV2_USE_CUDA=0
export MV2_USE_GPUDIRECT=0

export api_dir={api_dir}
export GRIB_DEFINITION_PATH=${{api_dir}}/cosmo_definitions/definitions:${{api_dir}}/share/grib_api/definitions
export GRIB_SAMPLES_PATH=${{api_dir}}/cosmo_definitions/samples
export GRIB_API_DEBUG=0

rm -f YU*

echo "====================================================="
echo "============== JOB OUTPUT BEGINS ===================="
echo "============== StartTime: `date +%s` s"
echo "============== StartTime: `date`"
echo "====================================================="
        
srun -u -n {cfg.int2lm_np_tot} ./int2lm >> {logfile} 2>&1
pid=$?

echo "====================================================="
echo "============== JOB OUTPUT ENDS ===================="
echo "============== EndTime: `date +%s` s"
echo "============== EndTime: `date` s"
echo "====================================================="

[[ $pid == 0 ]] || {{ echo \"Executing INT2LM MPI command failed\" >> job.out; exit 1; }}

# check whether INT2LM was successful by checking for presence of last
# meteofile that should have been processed
firstsecs=$(date -u --date "{cfg.inidate_int2lm_yyyymmddhh[0:8]} {cfg.inidate_int2lm_yyyymmddhh[8:]}" +%s)

let lastsecs="firstsecs+{cfg.hstop_int2lm}*3600"
lastyyyymmddhh=$(date -u --date "1970-01-01 UTC +${{lastsecs}} sec" +%Y%m%d%H)
lastHourFile="lbfd${{lastyyyymmddhh}}.nc"

echo last file processed is $lastHourFile >> {logfile}

[[ ! -f ../output/${{lastHourFile}} ]] && {{ echo "INT2LM failed" >> {logfile}; exit 1; }}

cp {logfile} {logfile_finish}

 """
        of.write(to_write)

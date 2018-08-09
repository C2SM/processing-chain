import os 

def main(cfg,logfile,logfile_finish):
    output_file = os.path.join(cfg.cosmo_work,"run.job")
    to_write = f"""#!/usr/bin/env bash
#SBATCH --partition={cfg.compute_queue}
#SBATCH --account={cfg.compute_account}
#SBATCH --job-name="cosmo_{cfg.inidate_yyyymmddhh}_{cfg.forecasttime}"
#SBATCH --output={logfile}
#SBATCH --time={cfg.cosmo_walltime}
#SBATCH --workdir={cfg.cosmo_work}
#SBATCH --constraint=gpu
#SBATCH --ntasks={cfg.cosmo_np_tot}
#SBATCH --ntasks-per-node=1
#SBATCH --gres=gpu:1

export OMP_NUM_THREADS=1
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export MV2_USE_CUDA=1
export MV2_USE_GPUDIRECT=0
export COSMO_NPROC_NODEVICE=0
export MPICH_RDMA_ENABLED_CUDA=1

module load craype-accel-nvidia60

# Set this to avoid segmentation faults
ulimit -s unlimited
ulimit -a

# clean up
rm -f YU*
rm -f M_*
rm -f core.*


echo "====================================================="
echo "============== JOB OUTPUT BEGINS ===================="
echo "============== StartTime: `date +%s` s"
echo "============== StartTime: `date`"
echo "====================================================="

srun -u -n {cfg.cosmo_np_tot} ./cosmo >> {logfile} 2>&1
pid=$?

echo "====================================================="
echo "============== JOB OUTPUT ENDS ===================="
echo "============== EndTime: `date +%s` s"
echo "============== EndTime: `date` s"
echo "====================================================="

[[ $pid == 0 ]] || {{ echo "Executing COSMO failed" >> {logfile}; exit 1; }}


# check whether COSMO was successful by calculating name of the last
# output file that should have been created

hoursSim=$(expr {cfg.hstop} % 24)
daysSim=$(expr {cfg.hstop} / 24) # bash does not know floating points, it truncates to integer
hoursSimNice=$(printf "%02d" $hoursSim)
daysSimNice=$(printf "%02d" $daysSim)
lastHourFile="lfff${{daysSimNice}}${{hoursSimNice}}0000.nc"

[[ ! -f {cfg.cosmo_output}${{lastHourFile}} ]] || {{ echo "COSMO failed" >> {logfile}; exit 1; }}

# copy log file
cp {logfile} {logfile_finish}
 """

    with open(output_file,"w") as of:
        of.write(to_write)

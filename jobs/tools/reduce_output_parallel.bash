#!/bin/bash -l
#
#	Will start any set of jobs in parallel from a bash script
#
#SBATCH --job-name=reduce_output
#SBATCH --partition=normal
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=36
#SBATCH --time=02:00:00
#SBATCH --constraint=mc
#SBATCH --account=em05
#SBATCH --mem=120GB


#
#	setup fifo for job control
#
open_sem(){
    mkfifo pipe-$$
    exec 3<>pipe-$$
    rm pipe-$$
    local i=$1
    for((;i>0;i--)); do
        printf %s 000 >&3
    done
}


#
#	start job/script with job control
#
run_with_lock(){
    local x

    read -u 3 -n 3 x && ((0==x)) || exit $x
    (
    "$@"
    printf '%.3d' $? >&3
    )&
    echo $$ $!
}


#	dummy job
job(){
	echo $@
	sleep 5
}


##############################################
#	MAIN
##############################################

#	set number of parallel jobs
N=36

#	initialise fifo
open_sem $N

##############################################
#       Setup for own script
##############################################

export EASYBUILD_PREFIX=/store/empa/em05/easybuild
module load daint-gpu
module load EasyBuild-custom/cscs
source /store/empa/em05/pyvenv-3.6/bin/activate

pyscript="$1"
indir="$2"
outdir="$3"
input_start="$4" # "2017-10-15 00"                                             
input_end="$5" # "2017-10-16 12"  
nout_levels="$6"
hstep="$7"
csvfile="$8"
convert_gas="$9"

startdate=$(date -d "$input_start") || exit -1                                 
enddate=$(date -d "$input_end")     || exit -1 
step_end="$(($hstep - 1))"

# Parallel loop (36 cores available)
d="$startdate"
while [[ "$str_d" < "$input_end" || "$str_d" == "$input_end" ]]; do
  strdate_start=$(date -u -d "$d" +%Y%m%d%H)                                            
  strdate_end=$(date -u -d "${d} + ${step_end} hours" +%Y%m%d%H) 
  run_with_lock python "$pyscript" "$indir" "$outdir" "$strdate_start" "$strdate_end" "$nout_levels" "$csvfile" "$convert_gas"
  d=$(date -u -d "${d} + ${hstep} hours")                                                   
  str_d=$(date -u -d "$d" +%Y%m%d%H)
done

# make sure to wait until all jobs are finished
wait

exit 0


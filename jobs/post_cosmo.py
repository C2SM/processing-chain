#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copy cosmo output from scratch to project.
#

#####################
##  Problems:
##  - need to see if some variables are defined (hstart, inidate ...)

### DEVELOPMENT VERSION ###

import logging
import os
import shutil
import glob 
from subprocess import call
import sys

def main(starttime, hstart, hstop, cfg):
    cosmo_work = os.environ["cosmo_work"]
    cosmo_output = os.environ["cosmo_output"]
    int2lm_work = os.environ["int2lm_work"]
    log_working_dir = os.environ["log_working_dir"]
    log_finished_dir = os.environ['log_finished_dir']

    runscript = os.path.join(cosmo_work,"cp_cosmo.job")
    copy_path = os.join.path(cfg.output_root,start_time.strftime('%Y%m%d%H')+"_"+hstart+"_"+hstop)
    logfile = os.path.join(log_working_dir,"post_cosmo")

    try:
        os.makedirs(copy_path, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating output folder failed")
        raise

    if cfg.compute_host!="daint":
        logging.error("The copy script is supposed to be run on daint only, not on %s" %cfg.compute_host)
        sys.exit(1)

    with open(runscript,"w") as script:        
        to_write = [
            "#SBATCH --job-name=post_cosmo",
            "#SBATCH --nodes=1",
            "#SBATCH --partition=xfer",
            "#SBATCH --constraint=gpu",
            "#SBATCH --account="+cfg.compute_account]

        to_write.append("#SBATCH --output="+logfile)
        to_write.append("#SBATCH --workdir="+cosmo_work]
        to_write.append("cp -R "+int2lm_work + " " + os.join.path(copy_path,"int2lm_run"))
        to_write.append("cp -R "+cosmo_work + " " + os.join.path(copy_path,"cosmo_run"))
        to_write.append("cp -R "+cosmo_output + " " + os.join.path(copy_path,"cosmo_output"))
        to_write.append("cp -R "+log_finished_dir + " " + os.join.path(copy_path,"logs"))
        to_write.append("cp -R "+logfile + " " + os.join.path(log_finised_dir,"logs"))
        to_write.append("cp -R "+os.join.path(log_finished_dir,"post_cosmo") + " " + os.join.path(copy_path,"logs"))

        script.writelines(to_write)


    subprocess.call(["sbatch","--wait" ,runscript])
    shutil.copy(logfile,os.join.path(log_finished_dir,"post_cosmo"))

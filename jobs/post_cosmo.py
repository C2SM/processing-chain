#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copy cosmo output from scratch to store (or anywhere else)

### DEVELOPMENT VERSION ###

import logging
import os
import shutil
import datetime as dt
import glob 
from subprocess import call
import sys
from . import tools


def main(starttime, hstart, hstop, cfg):
    """Copy the output of a **COSMO**-run to a user-defined position.

    Write a runscript to copy all files (**COSMO** settings & output,
    **int2lm** settings, logfiles) from ``cfg.cosmo_work``,
    ``cfg.cosmo_output``, ``cfg.int2lm_work``, ``cfg.log_finished_dir`` to
    ``cfg.output_root/...`` .
    
    Submit the job to the xfer-queue.
    
    Parameters
    ----------	
    start_time : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the start_time
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    logfile=os.path.join(cfg.log_working_dir,"post_cosmo")
    cosmo_work = cfg.cosmo_work
    cosmo_output = cfg.cosmo_output
    int2lm_work = cfg.int2lm_work
    log_working_dir = cfg.log_working_dir
    log_finished_dir = cfg.log_finished_dir


    runscript = os.path.join(cosmo_work,"cp_cosmo.job")
    copy_path = os.path.join(cfg.output_root,starttime.strftime('%Y%m%d%H')+
                             "_"+str(int(hstart))+"_"+str(int(hstop)))


    date = dt.datetime.today()

    to_print = """POST_COSMO

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" %date.strftime("%s")
    
    logging.info(to_print)
    logging.info("Copy output, run directoy and logfiles to output path")

    tools.create_dir(copy_path, "output")

    if cfg.compute_host!="daint":
        logging.error("The copy script is supposed to be run on daint only, not on %s" %cfg.compute_host)
        sys.exit(1)

    with open(runscript,"w") as script:        
        to_write = [
            "#!/bin/bash",
            "",
            "#SBATCH --job-name=post_cosmo",
            "#SBATCH --nodes=1",
            "#SBATCH --partition=xfer",
            "#SBATCH --constraint=gpu",
            "#SBATCH --account="+cfg.compute_account]

        to_write.append("#SBATCH --output="+logfile)
        to_write.append("#SBATCH --open-mode=append")
        to_write.append("#SBATCH --workdir="+cosmo_work)
        to_write.append("mkdir -p %s" % copy_path)
        to_write.append("cp -R "+int2lm_work + " " + os.path.join(copy_path,"int2lm_run"))
        to_write.append("cp -R "+cosmo_work + " " + os.path.join(copy_path,"cosmo_run"))
        to_write.append("cp -R "+cosmo_output + " " + os.path.join(copy_path,"cosmo_output"))
        to_write.append("cp -R "+log_finished_dir + " " + os.path.join(copy_path,"logs"))
        to_write.append("cp -R "+logfile + " " + os.path.join(log_finished_dir,"post_cosmo"))
        to_write.append("cp -R "+os.path.join(log_finished_dir,"post_cosmo") + " " + os.path.join(copy_path,"logs"))

        script.write("\n".join(to_write))


    call(["sbatch","--wait" ,runscript])

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)

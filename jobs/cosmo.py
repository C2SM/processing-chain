#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Setup the namelist for a COSMO tracer run and submit the job to the queue
#
# result in case of success: forecast fields found in  
#                            ${cosmo_output}
#
# Dominik Brunner, July 2013
#
# 2013-07-21 Initial release, adopted from Christoph Knote's cosmo.bash (brd)
# 2018-07-10 Translated to Python (muq)

# Not tested yet. Note the comment.

### DEVELOPMENT VERSION ###

import logging
import os
import shutil 
from subprocess import call
import sys
from . import tools
import importlib
import subprocess


def main(starttime, hstart, hstop, cfg):
    """
    Setup the namelist for a COSMO tracer run and submit the job to the queue
    """
    logfile=os.path.join(cfg.log_working_dir,"cosmo")
    logfile_finish=os.path.join(cfg.log_finished_dir,"cosmo")

    logging.info('Setup the namelist for a COSMO tracer run and submit the job to the queue')

# change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if int(starttime.strftime("%Y%m%d%H")) < 2007080200:   #input starttime as a number
        multi_layer=".FALSE."
    else:
        multi_layer=".TRUE."
    setattr(cfg,"multi_layer",multi_layer)

# create directory
    try:
        os.makedirs(cfg.cosmo_work, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating cosmo_work folder failed")
        raise
  
    try:
        os.makedirs(cfg.cosmo_output, exist_ok=True)   #output_root not used in cfg
    except (OSError, PermissionError):
        logging.error("Creating cosmo_output folder failed")
        raise

    try:
        os.makedirs(cfg.cosmo_restart_out, exist_ok=True)   #can't find this root in cfg. Use a temporary name here.
    except (OSError, PermissionError):
        logging.error("Creating cosmo_restart_out folder failed")
        raise
    
# copy cosmo.exe
    try:
        # 'cosmo' file name or directory
        shutil.copy(cfg.cosmo_bin, os.path.join(cfg.cosmo_work,'cosmo'))
    except FileNotFoundError:
        logging.error("cosmo_bin not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying cosmo_bin failed")
        raise

    # Write INPUT_BGC from csv file
    # csv file with tracer definitions 
    tracer_csvfile = os.path.join(cfg.casename,'cosmo_tracers.csv')

    tracer_filename = os.path.join(cfg.chain_src_dir,'cases',tracer_csvfile)
    input_bgc_filename = os.path.join(cfg.cosmo_work,'INPUT_BGC')

    tools.write_cosmo_input_bgc.main(tracer_filename,input_bgc_filename)

    # Prepare namelist and submit job
    for section in ["AF","ORG","IO","DYN","PHY","DIA","ASS"]:
        with open(cfg.cosmo_namelist+section+".cfg") as input_file:
            to_write = input_file.read();

        output_file = os.path.join(cfg.cosmo_work,"INPUT_"+section)
        with open(output_file, "w") as outf:
            to_write = to_write.format(cfg=cfg,
                                       restart_start = cfg.hstart + cfg.restart_step,
                                       restart_stop = cfg.hstop,
                                       restart_step = cfg.restart_step)
            outf.write(to_write)

    # write run script (run.job)
    with open(cfg.cosmo_runjob) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.cosmo_work, "run.job")
    with open(output_file, "w") as outf:
        outf.write(to_write.format(
            cfg=cfg,
            logfile=logfile, logfile_finish=logfile_finish)
        )

    subprocess.call(["sbatch", "--wait", os.path.join(cfg.cosmo_work,'run.job')])


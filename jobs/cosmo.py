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

def main(starttime, cfg):
    """
    Setup the namelist for a COSMO tracer run and submit the job to the queue
    """
    logging.info('Setup the namelist for a COSMO tracer run and submit the job to the queue')

    np_io= 1

# Set number of nodes and cores for COSMO 
    if cfg.compute_queue="normal":
        walltime="08:00:00"
        np_x=5
        np_y=4
    elif cfg.compute_queue="debug":
        walltime="00:30:00"
        np_x=1
        np_y=1
        ppn=1    
    else: 
        logging.error("unsetted queueName %s" %cfg.compute_queue)
        sys.exit(1)

    np_tot = np_x * np_y + np_io     

# change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if starttime < 2007080200:   #input starttime as a number
        multi_layer=".FALSE."
    else:
        multi_layer=".TRUE."

# create directory
    try:
        os.makedirs(cfg.work_root, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating cosmo_work folder failed")
        raise
    #cosmo_work = cfg.work_root      #create "cosmo_work" or not?
  
    try:
        os.makedirs(cfg.output_root, exist_ok=True)   #output_root not used in cfg
    except (OSError, PermissionError):
        logging.error("Creating cosmo_output folder failed")
        raise
    #cosmo_output = cfg.output_root 

    try:
        os.makedirs(cfg.restart_out_root, exist_ok=True)   #can't find this root in cfg. Use a temporary name here.
    except (OSError, PermissionError):
        logging.error("Creating cosmo_restart_out folder failed")
        raise
    #cosmo_restart_out = cfg.restart_out_root
    
# copy cosmo.exe
    try:
        # 'cosmo' file name or directory
        shutil.copy(cfg.cosmo_bin, os.path.join(cfg.work_root,'cosmo')
    except FileNotFoundError:
        logging.error("cosmo_bin not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying cosmo_bin failed")
        raise

# Write INPUT_BGC from csv file
    # csv file with tracer definitions 
    tracer_csvfile = ''.join(cfg.casename,'_cosmo_tracers.csv')

    tracer_filename = os.path.join(cfg.chain_src_dir,'cases',tracer_csvfile)
    input_bgc_filename = os.path.join(cfg.work_root,'INPUT_BGC')

    tools.write_cosmo_input_bgc(os.path.join(tracer_filename,
                                              input_bgc_filename,
                                             )
                                )

# Prepare namelist and submit job
    #will these *.sh be converted to python as well?
    subprocess.call(["source", cfg.cosmo_namelist])
    subprocess.call(["source", cfg.cosmo_runjob])   
    subprocess.call(["sbatch", "--wait", os.path.join(cfg.work_root,'run.job')])


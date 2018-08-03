#!/usr/bin/env python
# -*- coding: utf-8 -*-
#                                                                              
# Setup the namelist for an INT2LM-trcr run and submit the job to the queue       
#                                                                              
# result in case of success: all INT2LM-trcr input-files are found in           
#                            ${int2lm_output}                                  
#                                                                              
# Dominik Brunner, July 2013                                                   
#                                                                              
# 2013-07-20 Initial release, based on Christoph Knote's int2lm.bash (brd)
# 2017-01-15 adapted for hypatia and project SmartCarb (brd)
# 2018-08-03 Translated to Python (jae)

### DEVELOPMENT VERSION ###

import os
import logging
import shutil
from . import tools

def main(starttime, hstart, hstop, cfg):
    """
    Setup the namelist for a INT2LM and submit the job to the queue
    """
    logging.info('Setup the namelist for INT2LM and submit the job to the queue')


# Set number of nodes and cores for INT2LM 
    walltime="24:00:00" # TODO: make walltime dependent on simulation period?
                        #       3 days ~ 10 hours walltime
                        # Even possible to adapt the following parameters...
    nodes = 2
    ntasks_per_node = 12
    np_x = 8
    np_y = 3
    np_io = 0

    np_tot = np_x * np_y + np_io     

# Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if starttime < 2007080200:   #input starttime as a number
        multi_layer=".FALSE."
    else:
        multi_layer=".TRUE."

# Create int2lm directory
    try:
        os.makedirs(cfg.work_root, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating int2lm_work folder failed")
        raise
    #int2lm_work = cfg.work_root      #create "int2lm_work" or not?
  
    try:
        os.makedirs(cfg.output_root, exist_ok=True)   #output_root not used in cfg
    except (OSError, PermissionError):
        logging.error("Creating int2lm_output folder failed")
        raise
    #int2lm_output = cfg.output_root 

# copy int2lm executable
    try:
        # 'int2lm' file name or directory
        shutil.copy(cfg.int2lm_bin, os.path.join(cfg.work_root,'int2lm')
    except FileNotFoundError:
        logging.error("int2lm_bin not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying int2lm_bin failed")
        raise

# Copy extpar file to input/extpar directory
    try:
        # 'int2lm' file name or directory
        shutil.copy(cfg.int2lm_extpar, os.path.join(cfg.work_root,'int2lm')
    except FileNotFoundError:
        logging.error("int2lm extpar file not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying int2lm extpar file failed")
        raise

# Write INPUT_ART from csv file
    # csv file with tracer definitions 
    tracer_csvfile = ''.join(cfg.casename,'_int2lm_tracers.csv')
    # csv file with tracer datasets
    set_csvfile = ''.join(cfg.casename,'_int2lm_datasets.csv')

    tracer_filename = os.path.join(cfg.chain_src_dir,'cases',tracer_csvfile)
    set_filename = os.path.join(cfg.chain_src_dir,'cases',set_csvfile) 
    input_art_filename = os.path.join(cfg.work_root,'INPUT_ART')

    tools.write_int2lm_input_art(os.path.join(tracer_filename,
                                              set_filename,
                                              input_art_filename,
                                             )
                                )

# Prepare namelist and submit job
    #will these *.sh be converted to python as well?
    subprocess.call(["source", cfg.int2lm_namelist])
    subprocess.call(["source", cfg.int2lm_runjob])   
    subprocess.call(["sbatch", "--wait", os.path.join(cfg.work_root,'run.job')])


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
import subprocess
import sys
import importlib


def main(starttime, hstart, hstop, cfg):
    """
    Setup the namelist for a INT2LM and submit the job to the queue
    """
    logfile=os.path.join(cfg.log_working_dir,"int2lm")
    logfile_finish=os.path.join(cfg.log_finished_dir,"int2lm")
    tools.change_logfile(logfile)

    logging.info('Setup the namelist for INT2LM and submit the job to the queue')


# Set number of nodes and cores for INT2LM 
    walltime="24:00:00" # TODO: make walltime dependent on simulation period?
                        #       3 days ~ 10 hours walltime
                        # Even possible to adapt the following parameters...
    nodes = 2 ; setattr(cfg,"nodes",nodes)
    ntasks_per_node = 12 ; setattr(cfg,"ntasks_per_node",ntasks_per_node)
    np_x = 8; setattr(cfg,"np_x",np_x)
    np_y = 3; setattr(cfg,"np_y",np_y)
    np_io = 0; setattr(cfg,"np_io",np_io)

    np_tot = np_x * np_y + np_io; setattr(cfg,"np_tot",np_tot)

    # Queue information
    if cfg.compute_queue=="debug":
        walltime="00:30:00"
    else:
        walltime="06:00:00"
    setattr(cfg,"walltime",walltime)

# Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if int(starttime.strftime("%Y%m%d%H")) < 2007080200:   #input starttime as a number
        multi_layer=".FALSE."
    else:
        multi_layer=".TRUE."
    setattr(cfg,"multi_layer",multi_layer)

# Create int2lm directory
    try:
        os.makedirs(cfg.int2lm_work, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating int2lm_work folder failed")
        raise
  
    try:
        os.makedirs(cfg.int2lm_output, exist_ok=True)   #output_root not used in cfg
    except (OSError, PermissionError):
        logging.error("Creating int2lm_output folder failed")
        raise

# copy int2lm executable
    try:
        # 'int2lm' file name or directory
        shutil.copy(cfg.int2lm_bin, os.path.join(cfg.int2lm_work,'int2lm'))
    except FileNotFoundError:
        logging.error("int2lm_bin not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying int2lm_bin failed")
        raise

# Copy extpar file to input/extpar directory
    try:
        # 'int2lm' file name or directory
        extpar_folder = os.path.join(cfg.int2lm_input,"extpar")
        os.makedirs(extpar_folder, exist_ok=True)   #output_root not used in cfg
        extpar_file = os.path.join(cfg.int2lm_extpar_dir,cfg.int2lm_extpar_file)
        shutil.copy(extpar_file, extpar_folder)
    except FileNotFoundError:
        logging.error("int2lm extpar file not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying int2lm extpar file failed")
        raise

# Write INPUT_ART from csv file
    # csv file with tracer definitions 
    tracer_csvfile = os.path.join(cfg.casename,'int2lm_tracers.csv')
    # csv file with tracer datasets
    set_csvfile = os.path.join(cfg.casename,'int2lm_datasets.csv')

    tracer_filename = os.path.join(cfg.chain_src_dir,'cases',tracer_csvfile)
    set_filename = os.path.join(cfg.chain_src_dir,'cases',set_csvfile) 
    input_art_filename = os.path.join(cfg.int2lm_work,'INPUT_ART')

    tools.write_int2lm_input_art.main(tracer_filename, set_filename, input_art_filename)

# Prepare namelist and submit job
    sys.path.append(os.path.dirname(cfg.int2lm_namelist))
    input_script = importlib.import_module(os.path.basename(cfg.int2lm_namelist))
    input_script.main(cfg)

    sys.path.append(os.path.dirname(cfg.int2lm_runjob))
    input_script = importlib.import_module(os.path.basename(cfg.int2lm_runjob))
    input_script.main(cfg,logfile,logfile_finish)

    subprocess.call(["sbatch", "--wait", os.path.join(cfg.int2lm_work,'run.job')])

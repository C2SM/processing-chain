#!/usr/bin/env python
# -*- coding: utf-8 -*-
#                                                                              
# Setup the namelist for int2lm and submit the job to the queue       
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
from datetime import datetime


def main(starttime, hstart, hstop, cfg):
    """
    Setup the namelist for int2lm and submit the job to the queue
    """
    logfile = os.path.join(cfg.log_working_dir, "int2lm")
    logfile_finish = os.path.join(cfg.log_finished_dir, "int2lm")

# Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if starttime < datetime.strpdate('2007-08-02', '%Y-%m-%d'): 
        multi_layer = ".FALSE."
    else:
        multi_layer = ".TRUE."
    setattr(cfg, "multi_layer", multi_layer)

# Create int2lm directory
    try:
        os.makedirs(cfg.int2lm_work, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating int2lm_work folder failed")
        raise
  
    try:
        os.makedirs(cfg.int2lm_output, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating int2lm_output folder failed")
        raise

# copy int2lm executable
    try:
        # 'int2lm' file name or directory
        shutil.copy(cfg.int2lm_bin, os.path.join(cfg.int2lm_work, 'int2lm'))
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
        os.makedirs(extpar_folder, exist_ok=True)
        extpar_file = os.path.join(cfg.int2lm_extpar_dir, 
                                   cfg.int2lm_extpar_file)
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

    tracer_filename = os.path.join(cfg.chain_src_dir, 'cases', tracer_csvfile)
    set_filename = os.path.join(cfg.chain_src_dir, 'cases', set_csvfile) 
    input_art_filename = os.path.join(cfg.int2lm_work, 'INPUT_ART')

    tools.write_int2lm_input_art.main(tracer_filename, set_filename,
                                      input_art_filename)

    # Prepare namelist and submit job
    with open(cfg.int2lm_namelist) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.int2lm_work, "INPUT")
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg))

    with open(cfg.int2lm_runjob) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.int2lm_work, "run.job")
    with open(output_file, "w") as outf:
        outf.write(to_write.format(
            cfg=cfg,
            ini_day = cfg.inidate_int2lm_yyyymmddhh[0:8],
            ini_hour = cfg.inidate_int2lm_yyyymmddhh[8:],
            logfile=logfile, logfile_finish = logfile_finish))

    subprocess.call(["sbatch", "--wait",
                     os.path.join(cfg.int2lm_work, "run.job")])

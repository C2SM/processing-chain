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

import os
import logging
import shutil
from . import tools
import subprocess
import sys
import importlib
from datetime import datetime


def main(starttime, hstart, hstop, cfg):
    """Setup the namelist for **int2lm** and submit the job to the queue.

    Necessary for both **COSMO** and **COSMOART** simulations.
 
    Decide if the soil model should be TERRA or TERRA multi-layer depending on
    ``startdate`` of the simulation.

    Create necessary directory structure to run **int2lm** (run and output
    directories, defined in ``cfg.int2lm`` and ``cfg.int2lm_output``).

    Copy the **int2lm**-executable from ``cfg.int2lm_bin`` to 
    ``cfg.int2lm_work/int2lm``.

    Copy the extpar-file from ``cfg.extpar_dir/cfg.extpar_file`` to
    ``cfg.int2lm_run/extpar``.

    **COSMOART**: Copy the ``libgrib_api`` files to
    ``cfg.int2lm_work/libgrib_api``.

    **COSMO**: Convert the tracer-csv-files into a **int2lm**-namelist file.

    Format the **int2lm**-namelist-template using the information in ``cfg``.

    Format the runscript-template and submit the job.

    Parameters
    ----------	
    starttime : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the starttime
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    logfile = os.path.join(cfg.log_working_dir, "int2lm")
    logfile_finish = os.path.join(cfg.log_finished_dir, "int2lm")

    # Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if starttime < datetime.strptime('2007-08-02', '%Y-%m-%d'):
        multi_layer = ".FALSE."
    else:
        multi_layer = ".TRUE."
    setattr(cfg, "multi_layer", multi_layer)

    # Create int2lm directory
    tools.create_dir(cfg.int2lm_work, "int2lm_work")
    tools.create_dir(cfg.int2lm_output, "int2lm_output")

    tools.copy_file(cfg.int2lm_bin, os.path.join(cfg.int2lm_work, "int2lm"))

    # Copy extpar file to input/extpar directory
    extpar_dir = os.path.join(cfg.int2lm_input, "extpar")
    extpar_file = os.path.join(cfg.int2lm_extpar_dir, cfg.int2lm_extpar_file)
    tools.create_dir(extpar_dir, "int2lm extpar")
    tools.copy_file(extpar_file, extpar_dir)

    # Copy landuse and plant-functional-type files
    if cfg.target is tools.Target.COSMOART:
        lu_file_src = os.path.join(cfg.int2lm_lu_dir, cfg.int2lm_lu_file)
        lu_file_dst = os.path.join(extpar_dir, 'landuse.nc')
        tools.copy_file(lu_file_src, lu_file_dst)

        pft_file_src = os.path.join(cfg.int2lm_pft_dir, cfg.int2lm_pft_file)
        pft_file_dst = os.path.join(extpar_dir, 'pft.nc')
        tools.copy_file(pft_file_src, pft_file_dst)

    # Copy libgrib_api
    if cfg.target is tools.Target.COSMOART:
        dest = os.path.join(cfg.int2lm_work, 'libgrib_api')
        try:
            # delete so no error when forcing this job
            shutil.rmtree(dest)
        except FileNotFoundError:
            pass
        try:
            shutil.copytree(src=cfg.int2lm_libgrib_dir,
                            dst=dest,
                            symlinks=True)
        except FileNotFoundError:
            logging.error("libgrib_api directory not found")
            raise
        except (PermissionError, OSError):
            logging.error("Copying libgrib_api failed")
            raise

    # Write INPUT_ART from csv file if present
    tracer_csvfile = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                  'int2lm_tracers.csv')
    if os.path.isfile(tracer_csvfile):
        datasets_csvfile = os.path.join(cfg.chain_src_dir, 'cases',
                                        cfg.casename, 'int2lm_datasets.csv')
        input_art_filename = os.path.join(cfg.int2lm_work, 'INPUT_ART')

        tools.write_int2lm_input_art.main(tracer_csvfile, datasets_csvfile,
                                          input_art_filename)

    # Prepare namelist
    with open(cfg.int2lm_namelist) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.int2lm_work, "INPUT")
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg))

    # Prepare runscript
    with open(cfg.int2lm_runjob) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.int2lm_work, "run.job")
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg,
                            ini_day=cfg.inidate_int2lm_yyyymmddhh[0:8],
                            ini_hour=cfg.inidate_int2lm_yyyymmddhh[8:],
                            logfile=logfile,
                            logfile_finish=logfile_finish))

    # Submit job
    exitcode = subprocess.call(
        ["sbatch", "--wait",
         os.path.join(cfg.int2lm_work, "run.job")])
    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

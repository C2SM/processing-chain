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
from datetime import datetime, timedelta
import pytz


def main(starttime, hstart, hstop, cfg, model_cfg):
    """Setup the namelist for **int2lm** and submit the job to the queue.

    Necessary for both **COSMO** and **COSMOART** simulations.
 
    Decide if the soil model should be TERRA or TERRA multi-layer depending on
    ``startdate`` of the simulation.

    Create necessary directory structure to run **int2lm** (run and output
    directories, defined in ``cfg.int2lm`` and ``cfg.int2lm['output']``).

    Copy the **int2lm**-executable from ``cfg.int2lm['binary_file']`` to 
    ``cfg.int2lm['work']/int2lm``.

    Copy the extpar-file ``cfg.int2lm['extpar_file']`` to
    ``cfg.int2lm_run/work``.

    **COSMOART**: Copy the ``libgrib_api`` files to
    ``cfg.int2lm['work']/libgrib_api``.

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
    # Int2lm processing always starts at hstart=0, thus modifying inidate
    inidate_int2lm_yyyymmddhh = (
        cfg.startdate + timedelta(hours=cfg.hstart)).strftime('%Y%m%d%H')
    setattr(cfg, 'inidate_int2lm_yyyymmddhh', inidate_int2lm_yyyymmddhh)
    hstart_int2lm = 0
    hstop_int2lm = cfg.forecasttime

    # Total number of processes
    np_tot = cfg.int2lm['np_x'] * cfg.int2lm['np_y']

    # Set folder names
    setattr(cfg, 'int2lm_run', os.path.join(cfg.chain_root, 'int2lm', 'run'))
    setattr(cfg, 'int2lm_output',
            os.path.join(cfg.chain_root, 'int2lm', 'output'))
    int2lm_run = os.path.join(cfg.int2lm_run)
    int2lm_output = os.path.join(cfg.int2lm_output)

    # Create int2lm directories
    tools.create_dir(int2lm_run, "int2lm_run")
    tools.create_dir(int2lm_output, "int2lm_output")

    tools.copy_file(cfg.int2lm['binary_file'],
                    os.path.join(int2lm_run, "int2lm"))

    # Copy extpar file to input/extpar directory
    extpar_dir = os.path.join(cfg.int2lm_input, "extpar")
    tools.create_dir(extpar_dir, "int2lm extpar")
    tools.copy_file(
        os.path.join(cfg.int2lm['extpar_dir'], cfg.int2lm['extpar_filename']),
        extpar_dir)

    # Copy landuse and plant-functional-type files
    if cfg.model == 'cosmo-art':
        lu_file_src = cfg.int2lm['lu_file']
        lu_file_dst = os.path.join(extpar_dir, 'landuse.nc')
        tools.copy_file(lu_file_src, lu_file_dst)

        pft_file_src = cfg.int2lm['pft_file']
        pft_file_dst = os.path.join(extpar_dir, 'pft.nc')
        tools.copy_file(pft_file_src, pft_file_dst)

        # Copy libgrib_api
        dest = os.path.join(cfg.int2lm['work'], 'libgrib_api')
        try:
            # delete so no error when forcing this job
            shutil.rmtree(dest)
        except FileNotFoundError:
            pass
        try:
            shutil.copytree(src=cfg.int2lm['libgrib_dir'],
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
        input_art_filename = os.path.join(int2lm_run, 'INPUT_ART')

        tools.write_int2lm_input_art.main(tracer_csvfile, datasets_csvfile,
                                          input_art_filename)

    # Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if starttime < datetime(2007, 8, 2, tzinfo=pytz.UTC):
        multi_layer = ".FALSE."
    else:
        multi_layer = ".TRUE."

    # Prepare namelist
    with open(os.path.join(cfg.case_path,
                           cfg.int2lm['namelist_filename'])) as input_file:
        int2lm_namelist = input_file.read()

    output_file = os.path.join(int2lm_run, "INPUT")
    with open(output_file, "w") as outf:
        outf.write(
            int2lm_namelist.format(
                cfg=cfg,
                **cfg.int2lm,
                inidate_int2lm_yyyymmddhh=inidate_int2lm_yyyymmddhh,
                hstart_int2lm=hstart_int2lm,
                hstop_int2lm=hstop_int2lm,
                multi_layer=multi_layer,
                meteo_prefix=cfg.meteo['prefix'],
            ))

    # Prepare runscript
    with open(os.path.join(cfg.case_path,
                           cfg.int2lm['runjob_filename'])) as input_file:
        int2lm_runscript = input_file.read()

    # Logfile variables
    logfile = os.path.join(cfg.log_working_dir, "int2lm")
    logfile_finish = os.path.join(cfg.log_finished_dir, "int2lm")

    output_file = os.path.join(int2lm_run, "run.job")
    with open(output_file, "w") as outf:
        outf.write(
            int2lm_runscript.format(cfg=cfg,
                                    **cfg.int2lm,
                                    int2lm_run=int2lm_run,
                                    ini_day=inidate_int2lm_yyyymmddhh[0:8],
                                    ini_hour=inidate_int2lm_yyyymmddhh[8:],
                                    np_tot=np_tot,
                                    hstop_int2lm=hstop_int2lm,
                                    logfile=logfile,
                                    logfile_finish=logfile_finish))

    # Submit job
    result = subprocess.run(
        ["sbatch", "--wait",
         os.path.join(int2lm_run, "run.job")])
    exitcode = result.returncode
    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

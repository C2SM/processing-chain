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

import logging
import os
import shutil 
from subprocess import call
import sys
from . import tools
import importlib
import subprocess


def main(starttime, hstart, hstop, cfg):
    """Setup the namelists for a **COSMO** tracer run and submit the job to
    the queue

    Necessary for both **COSMO** and **COSMOART** simulations.

    Decide if the soil model should be TERRA or TERRA multi-layer depending on
    ``startdate`` of the simulation.
    
    Create necessary directory structure to run **COSMO** (run, output and
    restart directories, defined in ``cfg.cosmo_work``, ``cfg.cosmo_output``
    and ``cfg.cosmo_restart_out``).
    
    Copy the **COSMO**-executable from 
    ``cfg.cosmo_bin`` to ``cfg.cosmo_work/cosmo``.
    
    Convert the tracer-csv-file to a **COSMO**-namelist file.
    
    Format the **COSMO**-namelist-templates
    (**COSMO**: ``AF,ORG,IO,DYN,PHY,DIA,ASS``,
    **COSMOART**: ``ART,ASS,DIA,DYN,EPS,INI,IO,ORG,PHY``)
    using the information in ``cfg``.

    Format the runscript-template and submit the job.
    
    
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
    tools.create_dir(cfg.cosmo_work, "cosmo_work")
    # muq: output_root not used in cfg
    tools.create_dir(cfg.cosmo_output, "cosmo_output")
    if not cfg.target is tools.Target.COSMOART:
        # cosmoart can't do restarts
        tools.create_dir(cfg.cosmo_restart_out, "cosmo_restart_out")

    # copy cosmo executable
    execname = cfg.target.name.lower()
    tools.copy_file(cfg.cosmo_bin, os.path.join(cfg.cosmo_work, execname))

    # Write INPUT_BGC from csv file
    # csv file with tracer definitions 
    if cfg.target is tools.Target.COSMO:
        tracer_csvfile = os.path.join(cfg.casename,'cosmo_tracers.csv')

        tracer_filename = os.path.join(cfg.chain_src_dir,'cases',tracer_csvfile)
        input_bgc_filename = os.path.join(cfg.cosmo_work,'INPUT_BGC')

        tools.write_cosmo_input_bgc.main(tracer_filename,input_bgc_filename,cfg)

    # Prepare namelist and submit job
    if cfg.target is tools.Target.COSMO:
        namelist_names = ['AF','ORG','IO','DYN','PHY','DIA','ASS']
    elif cfg.target is tools.Target.COSMOART:
        namelist_names = ['ART', 'ASS', 'DIA', 'DYN', 'EPS', 'INI', 'IO', 'ORG', 'PHY']
    for section in namelist_names:
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

    exitcode =  subprocess.call(["sbatch", "--wait",
                                os.path.join(cfg.cosmo_work,'run.job')])
    if exitcode != 0:
       raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

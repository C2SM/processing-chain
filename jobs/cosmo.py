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
import subprocess
from .tools import write_cosmo_input_ghg
from . import tools
from datetime import datetime, timedelta


def main(starttime, hstart, hstop, cfg, model_cfg):
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
    starttime : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the starttime
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    logfile = os.path.join(cfg.log_working_dir, "cosmo")
    logfile_finish = os.path.join(cfg.log_finished_dir, "cosmo")

    logging.info("Setup the namelist for a COSMO tracer run and "
                 "submit the job to the queue")

    # Change of soil model from TERRA to TERRA multi-layer on 2 Aug 2007
    if int(starttime.strftime("%Y%m%d%H")) < 2007080200:
        multi_layer = ".FALSE."
    else:
        multi_layer = ".TRUE."
    setattr(cfg, "multi_layer", multi_layer)

    # Create directories
    tools.create_dir(cfg.cosmo_work, "cosmo_work")
    tools.create_dir(cfg.cosmo_output, "cosmo_output")

    # If an laf* file is used for initialization,
    # copy this to to 'cosmo/input/initial/' or merge with fieldextra
    if hasattr(cfg, 'laf_startfile'):
        tools.create_dir(cfg.cosmo_input, "cosmo_input")
        ini_dir = os.path.join(cfg.cosmo_input, "initial")
        tools.create_dir(ini_dir, "cosmo_input_initial")
        startfiletime = datetime.strptime(cfg.laf_startfile[-10:], "%Y%m%d%H")
        starttime_real = starttime + timedelta(hours=hstart)
        if starttime_real >= startfiletime:
            starttime_last = starttime_real - timedelta(hours=cfg.restart_step)
            job_id_last_run = starttime_last.strftime('%Y%m%d%H') + \
                              "_" + str(int(hstart)) + \
                              "_" + str(int(hstop))
            work_root = os.path.dirname(os.path.dirname(cfg.chain_root))
            last_output_path = os.path.join(work_root, cfg.casename,
                                            job_id_last_run, 'cosmo', 'output')
            laf_output_refdate = starttime_real.strftime("%Y%m%d%H")
            last_laf_filename = "laf" + laf_output_refdate
            # At the beginning, use original laf_startfile
            if starttime_real == startfiletime:
                last_laf_startfile = cfg.laf_startfile
            else:
                last_laf_startfile = os.path.join(last_output_path,
                                                  last_laf_filename)

            # Check if fieldextra is used
            if hasattr(cfg, 'fieldextra_bin') and \
            hasattr(cfg, 'fieldextra_control_file'):
                # Check if merge should be done for initial file
                if not hasattr(cfg, 'do_merge_at_start'):
                    setattr(cfg, 'do_merge_at_start', False)
                if starttime_real == startfiletime and not cfg.do_merge_at_start:
                    # Just copy the existing laf file
                    tools.copy_file(last_laf_startfile, ini_dir)
                else:
                    out_file = os.path.join(ini_dir, last_laf_filename)
                    ifs_in_file = os.path.join(cfg.int2lm_output,
                                               last_laf_filename)
                    # Write control file for fieldextra script (merge.ctl)
                    with open(cfg.fieldextra_control_file) as input_file:
                        to_write = input_file.read()

                    output_file_merge = os.path.join(ini_dir, "merge.ctl")
                    with open(output_file_merge, "w") as outf:
                        outf.write(
                            to_write.format(
                                cfg=cfg,
                                in_file=last_laf_startfile,
                                ifs_in_file=ifs_in_file,
                                out_file=out_file,
                                laf_output_refdate=laf_output_refdate,
                            ))
                    # Execute fieldextra
                    with open(logfile, "a+") as log:
                        result = subprocess.run(
                            [cfg.fieldextra_bin, output_file_merge],
                            stdout=log)
                        exitcode = result.returncode
                    if exitcode != 0:
                        raise RuntimeError(
                            "Fieldextra returned exitcode {}".format(exitcode))
            else:
                # Just copy the existing laf file
                tools.copy_file(last_laf_startfile, ini_dir)
        else:
            raise ValueError(
                "Start time %s must not be smaller than in laf_starttime %s." %
                (str(starttime), str(startfiletime)))

    # Create restart directory if feature is present and
    # if there is no spinup
    if 'restart' in model_cfg['models'][cfg.model]['features'] and not \
       hasattr(cfg, 'spinup'):
        tools.create_dir(cfg.cosmo_restart_out, "cosmo_restart_out")

    # Copy cosmo executable
    execname = cfg.model.lower()
    tools.copy_file(cfg.cosmo_bin, os.path.join(cfg.cosmo_work, execname))
    setattr(cfg, "execname", execname)

    # Prepare namelist and submit job
    tracer_csvfile = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                  'cosmo_tracers.csv')
    if cfg.model == 'cosmo':
        namelist_names = ['ORG', 'IO', 'DYN', 'PHY', 'DIA', 'ASS', 'SAT']
    elif cfg.model == 'cosmo-ghg':
        namelist_names = ['AF', 'ORG', 'IO', 'DYN', 'GHG', 'PHY', 'DIA', 'ASS']
    elif cfg.model == 'cosmo-art':
        namelist_names = [
            'ART', 'ASS', 'DIA', 'DYN', 'EPS', 'INI', 'IO', 'ORG', 'PHY'
        ]
        if hasattr(cfg, 'oem_dir'):
            # When doing online emissions in COSMO-ART, an additional
            # namelist is required
            namelist_names += ['OAE']

    for section in namelist_names:
        with open(cfg.cosmo_namelist + section + ".cfg") as input_file:
            to_write = input_file.read()

        output_file = os.path.join(cfg.cosmo_work, "INPUT_" + section)
        with open(output_file, "w") as outf:
            if hasattr(cfg, 'spinup'):
                # no built-in restarts
                to_write = to_write.format(cfg=cfg,
                                           restart_start=12,
                                           restart_stop=0,
                                           restart_step=12)
            else:
                # built-in restarts
                to_write = to_write.format(cfg=cfg,
                                           restart_start=cfg.hstart +
                                           cfg.restart_step,
                                           restart_stop=cfg.hstop,
                                           restart_step=cfg.restart_step)
            outf.write(to_write)

    # Append INPUT_GHG namelist with tracer definitions from csv file
    if os.path.isfile(tracer_csvfile):
        if cfg.model == 'cosmo-ghg':
            input_ghg_filename = os.path.join(cfg.cosmo_work, 'INPUT_GHG')

            write_cosmo_input_ghg.main(tracer_csvfile, input_ghg_filename, cfg)

    # Write run script (run.job)
    with open(cfg.cosmo_runjob) as input_file:
        to_write = input_file.read()

    output_file = os.path.join(cfg.cosmo_work, "run.job")
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg,
                            logfile=logfile,
                            logfile_finish=logfile_finish))

    result = subprocess.run(
        ["sbatch", "--wait",
         os.path.join(cfg.cosmo_work, 'run.job')])
    exitcode = result.returncode
    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

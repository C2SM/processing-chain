#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import subprocess
from pathlib import Path

from datetime import datetime
from .tools import write_cosmo_input_ghg
from . import tools, prepare_cosmo

BASIC_PYTHON_JOB = True


def main(cfg):
    """Setup the namelists for a COSMO run and submit the job to the queue.

    Create necessary directory structure to run COSMO (run, output, and
    restart directories, defined in ``cfg.cosmo_run``, ``cfg.cosmo_output``,
    and ``cfg.cosmo_restart_out``).

    Copy the COSMO-executable from
    ``cfg.cosmo['binary_file']`` to ``cfg.cosmo_run/cfg.cosmo['execname']``.

    Convert the tracer csv file to a COSMO namelist file.

    Format the COSMO namelist templates using the information in ``cfg``.

    Format the runscript template and submit the job.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)
    prepare_cosmo.set_cfg_variables(cfg)

    logging.info("Setup the namelist for a COSMO tracer run and "
                 "submit the job to the queue")

    # Create directories
    tools.create_dir(cfg.cosmo_run, "cosmo_run")
    tools.create_dir(cfg.cosmo_output, "cosmo_output")

    # Total number of processes
    np_tot = int(cfg.cosmo['np_x'] * cfg.cosmo['np_y'] /
                 cfg.ntasks_per_node) + cfg.cosmo['np_io']

    # If an laf* file is used for initialization,
    # copy this to to 'cosmo/input/initial/' or merge with fieldextra
    if hasattr(cfg, 'laf_startfile'):
        tools.create_dir(cfg.cosmo_input, "cosmo_input")
        ini_dir = os.path.join(cfg.cosmo_input, "initial")
        tools.create_dir(ini_dir, "cosmo_input_initial")
        startfiletime = datetime.strptime(cfg.laf_startfile[-10:], "%Y%m%d%H")
        if cfg.startdate_sim >= startfiletime:
            work_root = os.path.dirname(os.path.dirname(cfg.chain_root))
            last_output_path = os.path.join(work_root, cfg.casename,
                                            cfg.chunk_id_prev, 'cosmo',
                                            'output')
            laf_output_refdate = cfg.startdate_sim.strftime("%Y%m%d%H")
            last_laf_filename = "laf" + laf_output_refdate
            # At the beginning, use original laf_startfile
            if cfg.startdate_sim == startfiletime:
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
                if cfg.startdate_sim == startfiletime and not cfg.do_merge_at_start:
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
                    with open(cfg.logfile, "a+") as log:
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
                (str(cfg.starttime_sim), str(startfiletime)))

    # Create restart directory if feature is present and
    # if there is no spinup
    if 'restart' in cfg.workflow['features'] and not \
       hasattr(cfg, 'spinup'):
        tools.create_dir(cfg.cosmo_restart_out, "cosmo_restart_out")

    # Copy cosmo executable
    cfg.cosmo_execname = Path(cfg.cosmo['binary_file']).name
    tools.copy_file(cfg.cosmo['binary_file'],
                    cfg.cosmo_run / cfg.cosmo_execname)

    # Prepare namelist and submit job
    tracer_csvfile = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                  'cosmo_tracers.csv')
    if hasattr(cfg, 'cams') or hasattr(cfg, 'mozart'):
        namelist_names = ['AF', 'ORG', 'IO', 'DYN', 'GHG', 'PHY', 'DIA', 'ASS']
    elif hasattr(cfg, 'photo_rate'):
        namelist_names = [
            'ART', 'ASS', 'DIA', 'DYN', 'EPS', 'INI', 'IO', 'ORG', 'PHY'
        ]
        if hasattr(cfg, 'oem_dir'):
            # When doing online emissions in COSMO-ART, an additional
            # namelist is required
            namelist_names += ['OAE']
    elif hasattr(cfg, 'cosmo'):
        namelist_names = ['ORG', 'IO', 'DYN', 'PHY', 'DIA', 'ASS', 'SAT']

    for section in namelist_names:
        namelist_file = os.path.join(
            cfg.chain_src_dir, 'cases', cfg.casename,
            cfg.cosmo['namelist_prefix'] + section + ".cfg")
        with open(namelist_file) as input_file:
            cosmo_namelist = input_file.read()

        output_file = os.path.join(cfg.cosmo_run, "INPUT_" + section)
        with open(output_file, "w") as outf:
            if hasattr(cfg, 'spinup'):
                # no built-in restarts
                cosmo_namelist = cosmo_namelist.format(cfg=cfg,
                                                       **cfg.cosmo,
                                                       **cfg.oem,
                                                       restart_start=12,
                                                       restart_stop=0,
                                                       restart_step=12)
            else:
                # built-in restarts
                cosmo_namelist = cosmo_namelist.format(
                    cfg=cfg,
                    **cfg.cosmo,
                    **cfg.oem,
                    restart_start=0,
                    restart_stop=cfg.restart_step_hours,
                    restart_step=cfg.restart_step_hours)
            outf.write(cosmo_namelist)

    # Append INPUT_GHG namelist with tracer definitions from csv file
    if os.path.isfile(tracer_csvfile):
        if hasattr(cfg, 'cams') or hasattr(cfg, 'mozart'):
            input_ghg_filename = os.path.join(cfg.cosmo_run, 'INPUT_GHG')

            write_cosmo_input_ghg.main(tracer_csvfile, input_ghg_filename, cfg)

    # Write run script (run.job)
    runscript_file = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                  cfg.cosmo['runjob_filename'])
    with open(runscript_file) as input_file:
        cosmo_runscript = input_file.read()

    Path(cfg.cosmo_run).mkdir(parents=True, exist_ok=True)
    script = (cfg.cosmo_run / 'run_cosmo.job')
    with open(script, "w") as outf:
        outf.write(
            cosmo_runscript.format(cfg=cfg,
                                   **cfg.cosmo,
                                   np_tot=np_tot,
                                   logfile=cfg.logfile,
                                   logfile_finish=cfg.logfile_finish))

    # Submit job
    cfg.submit('cosmo', script)

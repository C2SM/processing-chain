#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Setup the namelist for an ICON run and submit the job to the queue
#
# result in case of success: forecast fields found in
#                            ${icon_output}
#
# Michael Jähn, February 2021
#
# 2021-04-26 Initial release
# 2021-11-21 Updated for ICON-ART

import logging
import os
import subprocess
from . import tools, prepare_data


def main(cfg):
    """Setup the namelists for an **ICON** tracer run and submit the job to
    the queue

    Necessary for both **ICON** and **ICONART** simulations.

    Create necessary directory structure to run **ICON** (run, output and
    restart directories, defined in ``cfg.icon_work``, ``cfg.icon_output``
    and ``cfg.icon_restart_out``).

    Copy the **ICON**-executable from
    ``cfg.icon_binary_file`` to ``cfg.icon_work/icon.exe``.

    Use the tracer-csv-file to append **ICON**-namelist file.

    Format the **ICON**-namelist-templates:
    ``icon_master.namelist.cfg, icon_NAMELIST_NWP.cfg``,
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
    cfg = prepare_data.set_cfg_variables(cfg)

    logfile = os.path.join(cfg.log_working_dir, "icon")
    logfile_finish = os.path.join(cfg.log_finished_dir, "icon")

    logging.info("Setup the namelist for an ICON run and "
                 "submit the job to the queue")

    # Copy icon executable
    execname = 'icon.exe'
    tools.copy_file(cfg.icon_binary_file, os.path.join(cfg.icon_work,
                                                       execname))

    # Symlink the restart file to the last run into the icon/run folder
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.restart_file, cfg.restart_file_scratch)

    # Get name of initial file
    if hasattr(cfg, 'inicond_filename'):
        inidata_filename = os.path.join(cfg.icon_input_icbc,
                                        cfg.inicond_filename)
    else:
        inidata_filename = os.path.join(
            cfg.icon_input_icbc,
            cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                       cfg.meteo['nameformat']) + '.nc')

    # Write run script (run_icon.job)
    icon_runjob = os.path.join(cfg.case_path, cfg.icon_runjob_filename)
    with open(icon_runjob) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.icon_work, "run_icon.job")
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg,
                            inidata_filename=inidata_filename,
                            logfile=logfile,
                            logfile_finish=logfile_finish))

    # Submit run script
    sbatch_cmd = ['sbatch', '--parsable']
    if dep_cmd := cfg.get_dep_cmd('icon'):
        sbatch_cmd.append(dep_cmd)
    sbatch_cmd.append(os.path.join(cfg.icon_work, 'run_icon.job'))

    result = subprocess.run(sbatch_cmd, capture_output=True)
    cfg.job_ids['current']['icon'] = int(result.stdout),

    # Anything hapenning after submission only makes sense in sequential mode
    if not cfg.async:
        exitcode = result.returncode

        # In case of ICON-ART, ignore the "invalid pointer" error on successful run
        if cfg.workflow_name.startswith('icon-art'):
            if tools.grep("free(): invalid pointer", logfile)['success'] and \
               tools.grep("clean-up finished", logfile)['success']:
                exitcode = 0

        if exitcode != 0:
            raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

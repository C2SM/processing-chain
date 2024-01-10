#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
from . import tools, prepare_icon

BASIC_PYTHON_JOB = True
# FIXME: BASIC_PYTHON_JOB should be False, but with ICON-ART the sbatch job is
#        always failing because of the "invalid pointer" error. This is caught
#        if this module itself is submitted as an sbatch job.

def main(cfg):
    """Setup the namelists for an ICON tracer run and submit the job to
    the queue.

    Necessary for both ICON and ICONART simulations.

    Create necessary directory structure to run ICON (run, output, and
    restart directories, defined in ``cfg.icon_work``, ``cfg.icon_output``,
    and ``cfg.icon_restart_out``).

    Copy the ICON-executable from
    ``cfg.icon_binary_file`` to ``cfg.icon_work/icon.exe``.

    Use the tracer-csv-file to append ICON-namelist file.

    Format the ICON-namelist-templates:
    ``icon_master.namelist.cfg, icon_NAMELIST_NWP.cfg``,
    using the information in ``cfg``.

    Format the runscript-template and submit the job.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    if BASIC_PYTHON_JOB:
        launch_time = cfg.init_time_logging("icon")

    logging.info("Setup the namelist for an ICON run and "
                 "submit the job to the queue")

    # Copy icon executable
    execname = 'icon.exe'
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.copy_file(cfg.icon_binary_file, cfg.icon_work / execname)

    # Symlink the restart file to the last run into the icon/run folder
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.restart_file, cfg.restart_file_scratch)

    # Get name of initial file
    if hasattr(cfg, 'inicond_filename'):
        inidata_filename = cfg.icon_input_icbc / cfg.inicond_filename
    else:
        inidata_filename = cfg.icon_input_icbc / str(
            cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                       cfg.meteo['nameformat']) + '.nc')

    # Write run script (run_icon.job)
    template = (cfg.case_path / cfg.icon_runjob_filename).read_text()
    script_str = template.format(cfg=cfg,
                                 inidata_filename=inidata_filename,
                                 logfile=cfg.logfile,
                                 logfile_finish=cfg.logfile_finish)
    script = (cfg.icon_work / 'run_icon.job')
    script.write_text(script_str)

    # Submit run script
    cfg.submit('icon', script, logfile=cfg.logfile)

    if BASIC_PYTHON_JOB:
        cfg.finish_time_logging("icon", launch_time)
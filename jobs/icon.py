#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
from pathlib import Path
from . import tools, prepare_icon

BASIC_PYTHON_JOB = False


def main(cfg):
    """Setup the namelists for an ICON run and submit the job to
    the queue.

    Copy the ICON-executable from
    ``cfg.icon_binary_file`` to ``cfg.icon_work/icon.exe``.

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

    logging.info("Setup the namelist for an ICON run and "
                 "submit the job to the queue")

    # Copy icon executable
    cfg.icon_execname = Path(cfg.icon['binary_file']).name
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.copy_file(cfg.icon_binary_file, cfg.icon_work / cfg.icon_execname)

    # Symlink the restart file to the last run into the icon/run folder
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.restart_file, cfg.restart_file_scratch)

    # Get name of initial file
    if hasattr(cfg, 'inicond_filename'):
        inidata_filename = cfg.icon_input_icbc / cfg.inicond_filename
    elif (hasattr(cfg, 'inidata_prefix') and hasattr(cfg, 'inidata_nameformat')
          and hasattr(cfg, 'inidata_filename_suffix')):
        inidata_filename = cfg.icon_input_icbc / str(
            cfg.startdate.strftime(cfg.inidata_prefix +
                                   cfg.inidata_nameformat +
                                   cfg.inidata_filename_suffix))
    else:
        print('Missing inidate_filename!')
        inidata_filename = cfg.icon_input_icbc / str(
            cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                       cfg.meteo['nameformat']) + '.nc')

    # Write run script (run_icon.job)
    template = (cfg.case_path / cfg.icon_runjob_filename).read_text()
    script_str = template.format(cfg=cfg, inidata_filename=inidata_filename)
    script = (cfg.icon_work / 'run_icon.job')
    script.write_text(script_str)

    # Submit run script
    cfg.submit('icon', script)

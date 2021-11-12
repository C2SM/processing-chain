#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Setup the namelist for an ICON tracer run and submit the job to the queue
#
# result in case of success: forecast fields found in
#                            ${icon_output}
#
# Michael Jähn, February 2021
#
# 2021-XX-XX Initial release

import logging
import os
import subprocess
from .tools import write_cosmo_input_ghg
from . import tools
from datetime import datetime, timedelta


def main(starttime, hstart, hstop, cfg):
    """Setup the namelists for an **ICON** tracer run and submit the job to
    the queue

    Necessary for both **ICON** and **ICONART** simulations.

    Create necessary directory structure to run **ICON** (run, output and
    restart directories, defined in ``cfg.icon_work``, ``cfg.icon_output``
    and ``cfg.icon_restart_out``).

    Copy the **ICON**-executable from
    ``cfg.icon_bin`` to ``cfg.icon_work/icon.exe``.

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
    logfile = os.path.join(cfg.log_working_dir, "icon")
    logfile_finish = os.path.join(cfg.log_finished_dir, "icon")

    logging.info("Setup the namelist for an ICON run and "
                 "submit the job to the queue")

    # Create directories
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.create_dir(cfg.icon_input_oae, "icon_input_oae")
    tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
    tools.create_dir(cfg.icon_input_icbc_processed,
                     "icon_input_icbc_processed")
    tools.create_dir(cfg.icon_input_grid, "icon_input_grid")
    tools.create_dir(cfg.icon_input_mapping, "icon_input_mapping")
    tools.create_dir(cfg.icon_input_rad, "icon_input_rad")
    tools.create_dir(cfg.icon_output, "icon_output")
    tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

    # Copy grid files
    src_dir = cfg.input_root_grid
    dest_dir = cfg.icon_input_grid
    tools.copy_file(os.path.join(src_dir, cfg.radiation_grid_filename),
                    os.path.join(dest_dir, cfg.radiation_grid_filename))
    tools.copy_file(os.path.join(src_dir, cfg.dynamics_grid_filename),
                    os.path.join(dest_dir, cfg.dynamics_grid_filename))
    tools.copy_file(os.path.join(src_dir, cfg.map_file_latbc),
                    os.path.join(dest_dir, cfg.map_file_latbc))
    tools.copy_file(os.path.join(src_dir, cfg.extpar_filename),
                    os.path.join(dest_dir, cfg.extpar_filename))
    tools.copy_file(os.path.join(src_dir, cfg.lateral_boundary_grid),
                    os.path.join(dest_dir, cfg.lateral_boundary_grid))

    # Copy radiation files
    src_dir = cfg.input_root_rad
    dest_dir = cfg.icon_input_rad
    tools.copy_file(os.path.join(src_dir, cfg.cldopt_filename),
                    os.path.join(dest_dir, cfg.cldopt_filename))
    tools.copy_file(os.path.join(src_dir, cfg.lrtm_filename),
                    os.path.join(dest_dir, cfg.lrtm_filename))

    # Copy mapping file
    src_dir = cfg.input_root_mapping
    dest_dir = cfg.icon_input_mapping
    tools.copy_file(os.path.join(src_dir, cfg.map_file_ana),
                    os.path.join(dest_dir, cfg.map_file_ana))

    # Copy icon executable
    execname = 'icon.exe'
    tools.copy_file(cfg.icon_bin, os.path.join(cfg.icon_work, execname))

    # Tracer file
    tracer_csvfile = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                  'icon_tracers.csv')

    # Write master namelist
    with open(cfg.icon_namelist_master) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.icon_work, 'icon_master.namelist')
    with open(output_file, "w") as outf:
        to_write = to_write.format(cfg=cfg)
        outf.write(to_write)

    # Write NWP namelist
    with open(cfg.icon_namelist_nwp) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.icon_work, 'NAMELIST_NWP')
    with open(output_file, "w") as outf:
        to_write = to_write.format(cfg=cfg)
        outf.write(to_write)

    # Append NWP namelist with tracer definitions from csv file
    if os.path.isfile(tracer_csvfile):
        input_ghg_filename = os.path.join(cfg.icon_work, 'NAMELIST_NWP')
        write_cosmo_input_ghg.main(tracer_csvfile, input_ghg_filename, cfg)

    # Write run script (run_icon.job)
    with open(cfg.icon_runjob) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.icon_work, "run_icon.job")
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg,
                            logfile=logfile,
                            logfile_finish=logfile_finish))

    exitcode = subprocess.call(
        ["sbatch", "--wait",
         os.path.join(cfg.icon_work, 'run_icon.job')])
    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

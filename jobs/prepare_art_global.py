#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import shutil
import subprocess
from . import tools, prepare_icon
from pathlib import Path  # noqa: F401
from .tools.interpolate_data import create_oh_for_restart, create_oh_for_inicond  # noqa: F401
from .tools.fetch_external_data import fetch_era5, fetch_era5_nudging

BASIC_PYTHON_JOB = True


def main(cfg):
    """
    Prepare global ICON-ART simulations.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    launch_time = cfg.init_time_logging("prepare_art_global")
    logging.info("Prepare ICON-ART for global simulations")

    # -- Download ERA5 data and create the inicond file
    if cfg.era5_inicond and cfg.lrestart == '.FALSE.':
        # -- Fetch ERA5 data
        fetch_era5(cfg.startdate_sim, cfg.icon_input_icbc)

        # -- Copy ERA5 processing script (icon_era5_inicond.job) in workdir
        with open(cfg.icon_era5_inijob) as input_file:
            to_write = input_file.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_inicond.sh')
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))

        # -- Copy mypartab in workdir
        shutil.copy(
            os.path.join(os.path.dirname(cfg.icon_era5_inijob), 'mypartab'),
            os.path.join(cfg.icon_input_icbc, 'mypartab'))

        # -- Run ERA5 processing script
        process = subprocess.Popen([
            "bash",
            os.path.join(cfg.icon_input_icbc, 'icon_era5_inicond.sh')
        ],
                                   stdout=subprocess.PIPE)
        process.communicate()

    # -----------------------------------------------------
    # Create tracer initial conditions
    # -----------------------------------------------------

    # -- Download and add CAMS data to the inicond file if needed
    if cfg.species_inicond:

        if cfg.lrestart == '.FALSE.':

            ext_restart = ''
            filename = cfg.input_files_scratch_inicond_filename

            # -- Copy the script for processing external tracer data in workdir
            with open(os.path.join(cfg.case_path,
                                   cfg.icon_species_inijob)) as input_file:
                to_write = input_file.read()
            output_file = os.path.join(cfg.icon_input_icbc,
                                       cfg.icon_species_inijob)
            with open(output_file, "w") as outf:
                outf.write(
                    to_write.format(cfg=cfg,
                                    filename=filename,
                                    ext_restart=ext_restart,
                                    year=cfg.startdate_sim.year,
                                    month=cfg.startdate_sim.month,
                                    day=cfg.startdate_sim.day))

            # -- Run ERA5 processing script
            process = subprocess.Popen(["bash", output_file],
                                       stdout=subprocess.PIPE)
            process.communicate()

            # -- Create initial conditions for OH concentrations
            if 'TROH' in cfg.species2restart:
                create_oh_for_inicond(cfg, cfg.startdate_sim.month)

        else:

            # -- Check the extension of tracer variables in the restart file
            ds_restart = xr.open_dataset(cfg.restart_file)  # noqa: F841
            tracer_name = cfg.species2restart[0]  # noqa: F841
            # FIXME:
            # var_restart = [
            # IndexError: list index out of range
            # var_restart = [
            #     var for var in ds_restart.data_vars.keys()
            #     if var.startswith(tracer_name)
            # ][0]
            # ext_restart = var_restart.replace(tracer_name, '')

            # -- Change OH concentrations in the restart file
            # if 'TROH' in cfg.species2restart:
            #     create_oh_for_restart(cfg, cfg.startdate_sim.month,
            #                           ext_restart)

    # -----------------------------------------------------
    # Create meteorological and tracer nudging conditions
    # -----------------------------------------------------

    # -- If global nudging, download and process ERA5 and CAMS data
    if cfg.era5_global_nudging:

        for time in tools.iter_hours(cfg.startdate_sim,
                                     cfg.enddate_sim,
                                     step=cfg.nudging_step):

            # -- Give a name to the nudging file
            timestr = time.strftime('%Y%m%d%H')
            filename = 'era2icon_R2B03_{timestr}_nudging.nc'.format(
                timestr=timestr)

            # -- If initial time, copy the initial conditions to be used as boundary conditions
            if time == cfg.startdate_sim and cfg.era5_inicond:
                shutil.copy(cfg.input_files_scratch_inicond_filename,
                            os.path.join(cfg.icon_input_icbc, filename))
                continue

            # -- Fetch ERA5 data
            fetch_era5_nudging(time, cfg.icon_input_icbc)

            # -- Copy ERA5 processing script (icon_era5_nudging.job) in workdir
            with open(cfg.icon_era5_nudgingjob) as input_file:
                to_write = input_file.read()
            output_file = os.path.join(
                cfg.icon_input_icbc, 'icon_era5_nudging_{}.sh'.format(timestr))
            with open(output_file, "w") as outf:
                outf.write(to_write.format(cfg=cfg, filename=filename))

            # -- Copy mypartab in workdir
            if not os.path.exists(os.path.join(cfg.icon_input_icbc,
                                               'mypartab')):
                shutil.copy(
                    os.path.join(os.path.dirname(cfg.icon_era5_nudgingjob),
                                 'mypartab'),
                    os.path.join(cfg.icon_input_icbc, 'mypartab'))

            # -- Run ERA5 processing script
            process = subprocess.Popen([
                "bash",
                os.path.join(cfg.icon_input_icbc,
                             'icon_era5_nudging_{}.sh'.format(timestr))
            ],
                                       stdout=subprocess.PIPE)
            process.communicate()

            if cfg.species_global_nudging:

                # -- Copy CAMS processing script (icon_cams_nudging.job) in workdir
                with open(cfg.icon_species_nudgingjob) as input_file:
                    to_write = input_file.read()
                output_file = os.path.join(
                    cfg.icon_input_icbc,
                    'icon_cams_nudging_{}.sh'.format(timestr))
                with open(output_file, "w") as outf:
                    outf.write(to_write.format(cfg=cfg, filename=filename))

                # -- Run ERA5 processing script
                process = subprocess.Popen([
                    "bash",
                    os.path.join(cfg.icon_input_icbc,
                                 'icon_cams_nudging_{}.sh'.format(timestr))
                ],
                                           stdout=subprocess.PIPE)
                process.communicate()

    logging.info("OK")
    cfg.finish_time_logging("prepare_art_global", launch_time)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
from . import tools
from .tools.interpolate_data import create_oh_for_restart, create_oh_for_inicond
from .tools.fetch_external_data import fetch_era5, fetch_era5_nudging
from calendar import monthrange


def set_cfg_variables(cfg):
    if cfg.workflow_name.startswith('cosmo'):
        cfg.int2lm_root = cfg.chain_root / 'int2lm'
        cfg.int2lm_input = cfg.int2lm_root / 'input'
    elif cfg.workflow_name.startswith('icon'):
        cfg.icon_base = cfg.chain_root / 'icon'
        cfg.icon_input = cfg.icon_base / 'input'
        cfg.icon_input_icbc = cfg.icon_input / 'icbc'
        cfg.icon_work = cfg.icon_base / 'run'
        cfg.icon_output = cfg.icon_base / 'output'
        cfg.icon_output_reduced = cfg.icon_base / 'output_reduced'
        cfg.icon_restart_out = cfg.icon_base / 'restart'
        cfg.icon_restart_in = cfg.chain_root_prev / 'icon' / 'run'
        cfg.icon_input_icbc_prev = cfg.chain_root_prev / 'icon' / 'input' / 'icbc'

        cfg.input_files_scratch = {}
        for dsc, file in cfg.input_files.items():
            cfg.input_files[dsc] = (p := Path(file))
            cfg.input_files_scratch[dsc] = cfg.icon_input / p.name

        cfg.create_vars_from_dicts()

        cfg.ini_datetime_string = cfg.startdate.strftime('%Y-%m-%dT%H:00:00Z')
        cfg.end_datetime_string = cfg.enddate.strftime('%Y-%m-%dT%H:00:00Z')

        if cfg.workflow_name == 'icon-art-oem':
            cfg.startdate_sim_yyyymmdd_hh = cfg.startdate_sim.strftime(
                '%Y%m%d_%H')

        if cfg.workflow_name == 'icon-art-global':
            # Nudge type (global or nothing)
            cfg.nudge_type = 2 if cfg.era5_global_nudging else 0
            # Time step for global nudging in seconds
            cfg.nudging_step_seconds = cfg.nudging_step * 3600
            # Prescribed initial conditions for CH4, CO and/or OH
            cfg.iart_init_gas = 4 if cfg.species_inicond else 0

        if cfg.lrestart == '.TRUE.':
            cfg.restart_filename = 'restart_atm_DOM01.nc'
            cfg.restart_file = cfg.icon_restart_in / cfg.restart_filename
            cfg.restart_file_scratch = cfg.icon_work / cfg.restart_filename

        cfg.job_ids['current']['prepare_data'] = []


def main(cfg):
    """
    This function prepares the data for global ICON-ART simulations
    by downloading necessary meteorological and chemical data,
    creating the inicond file, and handling global nudging
    conditions. The workflow includes fetching ERA5 data, processing it, 
    and creating initial conditions for tracers (such as CH4, CO, and OH).

    The main steps performed by this function include:
    1. Downloading ERA5 data and creating the inicond file if specified in the
       configuration.
    2. Creating initial conditions for tracers (CH4, CO, OH) if needed,
       including handling restart scenarios.
    3. Handling global nudging conditions by downloading and processing ERA5 and
       CAMS data if configured.

    Note
    ----
    - The function utilizes external scripts for processing ERA5 data
      (`icon_era5_inicond.sh`, `icon_era5_nudging.sh`) and CAMS data
      (`icon_cams_nudging.sh`).
    - The `tools` module provides various utility functions used in the workflow.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("prepare_art_global")

    # -- Download ERA5 data and create the inicond file
    if cfg.era5_inicond and cfg.lrestart == '.FALSE.':
        # -- Fetch ERA5 data
        fetch_era5(cfg.startdate_sim, cfg.icon_input_icbc)

        # -- Copy ERA5 processing script (icon_era5_inicond.job) in workdir
        with open(cfg.icon_era5_inijob) as input_file:
            to_write = input_file.read()
        output_file = os.path.join(cfg.icon_input_icbc,
                                    'icon_era5_inicond.sh')
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))

        # -- Copy mypartab in workdir
        shutil.copy(
            os.path.join(os.path.dirname(cfg.icon_era5_inijob),
                            'mypartab'),
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
            with open(
                    os.path.join(
                        cfg.case_path,
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
            ds_restart = xr.open_dataset(cfg.restart_file)
            tracer_name = cfg.species2restart[0]
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
                shutil.copy(
                    cfg.input_files_scratch_inicond_filename,
                    os.path.join(cfg.icon_input_icbc, filename))
                continue

            # -- Fetch ERA5 data
            fetch_era5_nudging(time, cfg.icon_input_icbc)

            # -- Copy ERA5 processing script (icon_era5_nudging.job) in workdir
            with open(cfg.icon_era5_nudgingjob) as input_file:
                to_write = input_file.read()
            output_file = os.path.join(
                cfg.icon_input_icbc,
                'icon_era5_nudging_{}.sh'.format(timestr))
            with open(output_file, "w") as outf:
                outf.write(to_write.format(cfg=cfg, filename=filename))

            # -- Copy mypartab in workdir
            if not os.path.exists(
                    os.path.join(cfg.icon_input_icbc, 'mypartab')):
                shutil.copy(
                    os.path.join(
                        os.path.dirname(cfg.icon_era5_nudgingjob),
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
                    outf.write(
                        to_write.format(cfg=cfg, filename=filename))

                # -- Run ERA5 processing script
                process = subprocess.Popen([
                    "bash",
                    os.path.join(
                        cfg.icon_input_icbc,
                        'icon_cams_nudging_{}.sh'.format(timestr))
                ],
                                            stdout=subprocess.PIPE)
                process.communicate()

    cfg.finish_time_logging("prepare_art_global", launch_time)
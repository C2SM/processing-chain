#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from pathlib import Path
import logging
import shutil
import subprocess
from datetime import timedelta
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


def async_error(cfg, part="This part"):
    if cfg.is_async:
        raise NotImplementedError(
            f"{part} isn't ready for async execution yet")


def main(cfg):
    """
    **ICON and COSMO Data Preparation**

    This function prepares input data for ICON and COSMO simulations by creating necessary directories,
    copying meteorological files, and handling specific data processing for each model.

    **ICON:**
    - Create directories ``cfg.icon_input_icbc`` and ``cfg.icon_work``.

    **COSMO:**

    - Copy meteorological files to **int2lm** input.
    - Create the necessary directory ``cfg.int2lm_input/meteo``.
    - Copy meteorological files from the project directory (``cfg.meteo['dir']/cfg.meteo['prefix']YYYYMMDDHH``)
      to the int2lm input folder on scratch (``cfg.int2lm_input/meteo``).
    - For nested runs (meteorological files are COSMO output: ``cfg.meteo['prefix'] == 'lffd'``),
      also copy the ``*c.nc``-file with constant parameters.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.

    Raises
    ------
    RuntimeError
        If any subprocess returns a non-zero exit code during execution.
    """
    set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("prepare_data")

    if cfg.workflow_name.startswith('icon'):
        logging.info('ICON input data (IC/BC)')

        # Create directories
        tools.create_dir(cfg.icon_work, "icon_work")
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        tools.create_dir(cfg.icon_output, "icon_output")
        tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

        # Set logfile
        logfile = cfg.log_working_dir / 'prepare_data'
        logfile_finish = cfg.log_finished_dir / 'prepare_data'

        # Copy input files to scratch
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
            f'#SBATCH --account={cfg.compute_account}',
            f'#SBATCH --time=00:10:00',
            f'#SBATCH --partition={cfg.compute_queue}',
            f'#SBATCH --constraint={cfg.constraint}', '#SBATCH --nodes=1',
            f'#SBATCH --output={logfile}', '#SBATCH --open-mode=append',
            f'#SBATCH --chdir={cfg.icon_work}', ''
        ]
        for target, destination in zip(cfg.input_files.values(),
                                       cfg.input_files_scratch.values()):
            script_lines.append(f'rsync -av {target} {destination}')

        with (script := cfg.icon_work / 'copy_input.job').open('w') as f:
            f.write('\n'.join(script_lines))

        copy_id = cfg.submit('prepare_data', script)

    # If COSMO (and not ICON):
    else:
        async_error(cfg, part='COSMO')
        logging.info('COSMO analysis data for IC/BC')

        dest_path = cfg.int2lm_input / 'meteo'
        tools.create_dir(dest_path, "meteo input")

        source_nameformat = cfg.meteo['nameformat']
        if cfg.meteo['prefix'] == 'lffd':
            # nested runs use cosmoart-output as meteo data
            # have to copy the *c.nc-file
            src_file = (cfg.meteo['dir'] /
                        cfg.startdate_sim.strftime(source_nameformat + 'c.nc'))

            tools.copy_file(src_file, dest_path, output_log=True)

            logging.info("Copied constant-param file from {} to {}".format(
                src_file, dest_path))

            # extend nameformat with ending to match cosmo-output
            source_nameformat += '.nc'

        if cfg.meteo['prefix'] == 'efsf':
            source_nameformat = cfg.meteo['prefix'] + '%y%m%d%H'

        num_steps = 0
        meteo_dir = cfg.meteo['dir']
        subdir = meteo_dir / cfg.startdate_sim.strftime('%y%m%d%H')
        for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                     cfg.meteo['inc']):
            dest_path = cfg.int2lm_input / 'meteo'
            src_file = meteo_dir / time.strftime(source_nameformat)

            if cfg.meteo['prefix'] == 'efsf':
                if time == cfg.startdate_sim:
                    src_file = subdir / ('eas' + time.strftime('%Y%m%d%H'))
                    if not src_file.exists() and cfg.meteo.get('dir_alt') \
                        is not None:
                        meteo_dir = cfg.meteo['dir_alt']
                        subdir = meteo_dir / cfg.startdate_sim.strftime(
                            '%y%m%d%H')
                        src_file = subdir / ('eas' + time.strftime('%Y%m%d%H'))
                    dest_path = cfg.int2lm_input / 'meteo' / (
                        cfg.meteo['prefix'] + '00000000')
                else:
                    td = time - cfg.startdate_sim - timedelta(hours=6 *
                                                              num_steps)
                    days = str(td.days).zfill(2)
                    hours = str(td.seconds // 3600).zfill(2)
                    td_total = time - cfg.startdate_sim
                    days_total = str(td_total.days).zfill(2)
                    hours_total = str(td_total.seconds // 3600).zfill(2)

                    src_file = subdir / (cfg.meteo['prefix'] + days + hours +
                                         '0000')
                    dest_path = cfg.int2lm_input / 'meteo' / (
                        cfg.meteo['prefix'] + days_total + hours_total +
                        '0000')

                    # Next time, change directory
                    checkdir = meteo_dir / time.strftime('%y%m%d%H')
                    if checkdir.is_dir():
                        num_steps += 1
                        subdir = checkdir
                    elif cfg.meteo.get('dir_alt') is not None:
                        checkdir = cfg.meteo['dir_alt'] / time.strftime(
                            '%y%m%d%H')
                        if checkdir.is_dir():
                            num_steps += 1
                            subdir = checkdir
                            meteo_dir = cfg.meteo['dir_alt']
                            logging.info(
                                "Switching to other input directory from {} to {}"
                                .format(cfg.meteo['dir'],
                                        cfg.meteo['dir_alt']))
            elif not src_file.exists():
                # special case for MeteoSwiss COSMO-7 data
                archive = Path('/store/mch/msopr/owm/COSMO-7')
                yy = time.strftime("%y")
                path = archive / 'ANA' + yy
                src_file = path / time.strftime(source_nameformat)

            # copy meteo file from project folder to
            tools.copy_file(src_file, dest_path, output_log=True)

            logging.info("Copied file from {} to {}".format(
                src_file, dest_path))

        # Other IC/BC data
        inv_to_process = []
        if cfg.workflow_name == 'cosmo-ghg':
            try:
                CAMS = dict(fullname="CAMS",
                            nickname="cams",
                            executable="cams4int2cosmo",
                            indir=cfg.cams['dir_orig'],
                            outdir=cfg.cams['dir_proc'],
                            param=[{
                                'inc': cfg.cams['inc'],
                                'suffix': cfg.cams['suffix']
                            }])
                inv_to_process.append(CAMS)
            except AttributeError:
                pass
            try:
                CT = dict(fullname="CarbonTracker",
                          nickname="ct",
                          executable="ctnoaa4int2cosmo",
                          indir=cfg.ct_dir_orig,
                          outdir=cfg.ct_dir_proc,
                          param=cfg.ct_parameters)
                inv_to_process.append(CT)
            except AttributeError:
                pass
        elif cfg.workflow_name == 'cosmo-art':
            try:
                MOZART = dict(fullname='MOZART',
                              nickname='mozart',
                              executable='mozart2int2lm',
                              indir=cfg.mozart_file_orig,
                              outdir=cfg.mozart_dir_proc,
                              param=[{
                                  'inc': cfg.mozart_inc,
                                  'suffix': cfg.mozart_prefix
                              }])
                inv_to_process.append(MOZART)
            except AttributeError:
                pass

        if cfg.workflow_name == 'cosmo-ghg' or cfg.workflow_name == 'cosmo-art':
            logging.info("Processing " +
                         ", ".join([i["fullname"]
                                    for i in inv_to_process]) + " data")

            scratch_path = cfg.int2lm_input / 'icbc'
            tools.create_dir(scratch_path, "icbc input")

            for inv in inv_to_process:
                logging.info(inv["fullname"] + " files")
                tools.create_dir(inv["outdir"], "processed " + inv["fullname"])

                for p in inv["param"]:
                    inc = p["inc"]
                    for time in tools.iter_hours(cfg.startdate_sim,
                                                 cfg.enddate_sim, inc):
                        logging.info(time)

                        filename = inv["outdir"] / (p["suffix"] + "_" +
                                                    time.strftime("%Y%m%d%H") +
                                                    ".nc")
                        if not filename.exists():
                            logging.info(filename)
                            try:
                                to_call = getattr(tools, inv["executable"])
                                to_call.main(time, inv["indir"], inv["outdir"],
                                             p)
                            except:
                                logging.error("Preprocessing " +
                                              inv["fullname"] + " data failed")
                                raise

                        # copy to (temporary) run input directory
                        tools.copy_file(filename,
                                        scratch_path,
                                        output_log=True)

                        logging.info("OK")

    cfg.finish_time_logging("prepare_data", launch_time)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pathlib import Path
import logging
from . import tools

BASIC_PYTHON_JOB = True


def set_cfg_variables(cfg):
    cfg.icon_base = cfg.chain_root / 'icon'
    cfg.icon_input = cfg.icon_base / 'input'
    cfg.icon_input_icbc = cfg.icon_input / 'icbc'
    cfg.icon_work = cfg.icon_base / 'run'
    cfg.icon_output = cfg.icon_base / 'output'
    cfg.icon_output_reduced = cfg.icon_base / 'output_reduced'
    cfg.icon_restart_out = cfg.icon_base / 'restart'
    if cfg.chunk_id_prev:
        cfg.icon_restart_in = cfg.chain_root_prev / 'icon' / 'run'
        cfg.icon_input_icbc_prev = cfg.chain_root_prev / 'icon' / 'input' / 'icbc'

    cfg.input_files_scratch = {}
    for dsc, file in cfg.input_files.items():
        p = Path(file)
        if not p.is_absolute():
            # resolve relative input_files paths relative to the case directory
            p = (cfg.case_path / p).resolve()
        cfg.input_files[dsc] = p
        cfg.input_files_scratch[dsc] = cfg.icon_input / p.name

    cfg.create_vars_from_dicts()

    cfg.ini_datetime_string = cfg.startdate.strftime('%Y-%m-%dT%H:00:00Z')
    cfg.end_datetime_string = cfg.enddate.strftime('%Y-%m-%dT%H:00:00Z')

    if cfg.lrestart == '.TRUE.':
        cfg.restart_filename = 'restart_ATMO_DOM01.nc'
        cfg.restart_file = cfg.icon_restart_in / cfg.restart_filename
        cfg.restart_file_scratch = cfg.icon_work / cfg.restart_filename
        if hasattr(cfg, 'lbcdata_prefix'):
            cfg.ini_LBC_filename = cfg.startdate.strftime(
                cfg.lbcdata_prefix + cfg.lbcdata_nameformat +
                cfg.lbcdata_filename_suffix)
            cfg.ini_LBC_file = cfg.icon_input_icbc_prev / cfg.ini_LBC_filename
            cfg.ini_LBC_file_scratch = cfg.icon_input_icbc / cfg.ini_LBC_filename

    # Nudge type (global or nothing)
    cfg.nudge_type = 2 if hasattr(cfg,
                                  'era5') and cfg.era5_global_nudging else 0
    # Time step for global nudging in seconds
    cfg.nudging_step_seconds = cfg.nudging_step * 3600 if hasattr(
        cfg, 'nudging_step') else None
    # Prescribed initial conditions for CH4, CO and/or OH
    cfg.iart_init_gas = 4 if hasattr(
        cfg, 'species_inicond') and cfg.species_inicond else 0

    cfg.startdate_sim_yyyymmdd_hh = cfg.startdate_sim.strftime('%Y%m%d_%H')
    cfg.startdate_sim_yyyymmddhh = cfg.startdate_sim.strftime('%Y%m%d%H')
    cfg.enddate_sim_yyyymmddhh = cfg.enddate_sim.strftime('%Y%m%d%H')


def get_inidata_filename(cfg):
    """Return the initial-condition file ICON reads for the current chunk.

    Both ``icon`` and ``era5_ic`` rely on this name: the latter has to write
    exactly the file the former later reads.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.

    Returns
    -------
    pathlib.Path
        Absolute path of the initial-condition file in ``icon_input_icbc``.
    """
    if hasattr(cfg, 'inicond_filename'):
        return cfg.icon_input_icbc / cfg.inicond_filename
    if (hasattr(cfg, 'inidata_prefix') and hasattr(cfg, 'inidata_nameformat')
            and hasattr(cfg, 'inidata_filename_suffix')):
        return cfg.icon_input_icbc / str(
            cfg.startdate.strftime(cfg.inidata_prefix +
                                   cfg.inidata_nameformat +
                                   cfg.inidata_filename_suffix))
    return cfg.icon_input_icbc / str(
        cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                   cfg.meteo['nameformat']) + '.nc')


def main(cfg):
    """
    **ICON Data Preparation**

    This function prepares input data for ICON simulations by creating necessary directories,
    copying meteorological files, and handling specific data processing.

    - Create working directories and copy input files

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
    tools.change_logfile(cfg.logfile)

    # Create directories
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
    tools.create_dir(cfg.icon_output, "icon_output")
    tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

    logging.info('Copy ICON input data (IC/BC) to working directory')
    # Walltime of the copy job, kept separate from cfg.walltime['prepare_icon'],
    # which sizes the wrapper job that waits for this job and must therefore
    # also account for its queue time.
    walltime = getattr(cfg, 'walltime_jobs', {}).get('prepare_icon',
                                                     '00:10:00')

    # Copy input files to scratch
    if cfg.machine == 'santis':
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
            f'#SBATCH --account={cfg.compute_account}',
            f'#SBATCH --time={walltime}',
            f'#SBATCH --partition={cfg.compute_queue}',
            f'#SBATCH --constraint={cfg.constraint}', '#SBATCH --nodes=1',
            f'#SBATCH --output={cfg.logfile}', '#SBATCH --open-mode=append',
            f'#SBATCH --chdir={cfg.icon_work}', ''
        ]
    elif cfg.machine == 'euler':
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
            f'#SBATCH --time={walltime}',
            f'#SBATCH --constraint={cfg.constraint}', '#SBATCH --ntasks=1',
            f'#SBATCH --output={cfg.logfile}', '#SBATCH --open-mode=append',
            f'#SBATCH --chdir={cfg.icon_work}', ''
        ]
    script_lines.append('set -euo pipefail')
    for target, destination in zip(cfg.input_files.values(),
                                   cfg.input_files_scratch.values()):
        script_lines.append(f'rsync -av {target} {destination}')

    with (script := cfg.icon_work / 'copy_input.job').open('w') as f:
        f.write('\n'.join(script_lines))

    cfg.submit('prepare_icon', script)
    logging.info("OK")

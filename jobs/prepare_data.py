#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from pathlib import Path
import logging
import shutil
import subprocess
from datetime import timedelta
import xarray as xr
import numpy as np
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
    - Submit the runscript for the DWD ICON tools to remap the meteorological files.
    - All runscripts specified in ``cfg.icontools_runjobs`` are submitted.
    - The meteorological files are read from the original input directory (``cfg.input_root_meteo``),
      and the remapped meteorological files are saved in the input folder on scratch (``cfg.icon_input/icbc``).
    - The constant variable 'GEOSP' is added to the files not containing it using python-cdo bindings.

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
    model_cfg : dict
        Model configuration settings loaded from the ``config\/models.yaml`` file.

    Raises
    ------
    RuntimeError
        If any subprocess returns a non-zero exit code during execution.
    """
    cfg = set_cfg_variables(cfg, model_cfg)

    if cfg.workflow_name.startswith('icon'):
        logging.info('ICON input data (IC/BC)')

        #-----------------------------------------------------
        # Create directories
        #-----------------------------------------------------
        tools.create_dir(cfg.icon_work, "icon_work")
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        tools.create_dir(cfg.icon_output, "icon_output")
        tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

        #-----------------------------------------------------
        # Copy input files
        #-----------------------------------------------------
        wall_time = getattr(cfg, 'copy_input_walltime', '00:01:00')
        queue = getattr(cfg, 'copy_input_queue', 'normal')

        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
            f'#SBATCH --account={cfg.compute_account}',
            f'#SBATCH --time={walltime}', f'#SBATCH --partition={queue}',
            '#SBATCH --constraint=gpu', '#SBATCH --nodes=1', ''
        ]
        for target, destination in zip(cfg.input_files.values(),
                                       cfg.input_files_scratch.values()):
            script_lines.append(f'rsync -av {target} {destination}')

        with (script := cfg.icon_base / 'copy_input.job').open('w') as f:
            f.write('\n'.join(script_lines))

        cfg.submit('prepare_data', script)

        if cfg.workflow_name == 'icon-art-global':
            async_error(cfg, part='global ICON-ART')
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

        else:  # non-global ICON-ART
            async_error(cfg, part='non-global ICON-ART')
            #-----------------------------------------------------
            # Create LBC datafile lists (each at 00 UTC and others)
            #-----------------------------------------------------
            datafile_list = []
            datafile_list_rest = []
            datafile_list_chem = []
            for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                         cfg.meteo['inc']):
                meteo_file = os.path.join(
                    cfg.icon_input_icbc, cfg.meteo['prefix'] +
                    time.strftime(cfg.meteo['nameformat']))
                if cfg.workflow_name == 'icon-art' or cfg.workflow_name == 'icon-art-oem':
                    chem_file = os.path.join(
                        cfg.icon_input_icbc, cfg.chem['prefix'] +
                        time.strftime(cfg.chem_nameformat))
                    datafile_list_chem.append(chem_file + cfg.chem['suffix'])
                if meteo_file.endswith('00'):
                    datafile_list.append(meteo_file + cfg.meteo['suffix'])
                else:
                    datafile_list_rest.append(meteo_file + cfg.meteo['suffix'])
            datafile_list = ' '.join([str(v) for v in datafile_list])
            datafile_list_rest = ' '.join([str(v) for v in datafile_list_rest])
            datafile_list_chem = ' '.join([str(v) for v in datafile_list_chem])

            #-----------------------------------------------------
            # Write and submit runscripts
            #-----------------------------------------------------
            for runscript in cfg.icontools_runjobs:
                logfile = os.path.join(cfg.log_working_dir, 'prepare_data')
                logfile_finish = os.path.join(cfg.log_finished_dir,
                                              'prepare_data')
                with open(os.path.join(cfg.case_path,
                                       runscript)) as input_file:
                    to_write = input_file.read()
                output_run = os.path.join(cfg.icon_work, "%s.job" % runscript)
                with open(output_run, "w") as outf:
                    outf.write(
                        to_write.format(cfg=cfg,
                                        meteo=cfg.meteo,
                                        logfile=logfile,
                                        logfile_finish=logfile_finish,
                                        datafile_list=datafile_list,
                                        datafile_list_rest=datafile_list_rest,
                                        datafile_list_chem=datafile_list_chem))
                logging.info(f" Starting icontools runscript {runscript}.")
                result = subprocess.run([
                    "sbatch", "--wait",
                    os.path.join(cfg.icon_work, "%s.job" % runscript)
                ])
                exitcode = result.returncode
                if exitcode != 0:
                    raise RuntimeError(
                        "sbatch returned exitcode {}".format(exitcode))
                logging.info(f"{runscript} successfully executed.")

            #-----------------------------------------------------
            # Add GEOSP to all meteo files
            #-----------------------------------------------------
            for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                         cfg.meteo['inc']):
                # Specify file names
                geosp_filename = time.replace(
                    hour=0).strftime(cfg.meteo['prefix'] +
                                     cfg.meteo['nameformat']) + '_lbc.nc'
                geosp_file = os.path.join(cfg.icon_input_icbc, geosp_filename)
                src_filename = time.strftime(
                    cfg.meteo['prefix'] + cfg.meteo['nameformat']) + '_lbc.nc'
                src_file = os.path.join(cfg.icon_input_icbc, src_filename)
                merged_filename = time.strftime(
                    cfg.meteo['prefix'] +
                    cfg.meteo['nameformat']) + '_merged.nc'
                merged_file = os.path.join(cfg.icon_input_icbc,
                                           merged_filename)

                # Copy GEOSP file from last run if not present
                if not os.path.exists(geosp_file):
                    geosp_src_file = os.path.join(cfg.icon_input_icbc_prev,
                                                  geosp_filename)
                    tools.copy_file(geosp_src_file,
                                    cfg.icon_input_icbc,
                                    output_log=True)

                # Load GEOSP data array as da_geosp at time 00:
                ds = xr.open_dataset(src_file)
                ds_geosp = xr.open_dataset(geosp_file)
                da_geosp = ds_geosp['GEOSP']

                # Merge GEOSP-dataset with other timesteps
                if (time.hour != 0):
                    # Change values of time dimension to current time
                    da_geosp = da_geosp.assign_coords(
                        time=[np.datetime64(time)])
                    # Merge GEOSP into temporary file
                    ds_merged = xr.merge([ds, da_geosp])
                    ds_merged.attrs = ds.attrs
                    ds_merged.to_netcdf(merged_file)
                    # Logging info for merging GEOSP
                    logging.info("Added GEOSP to file {}".format(merged_file))
                    # Rename file to get original file name
                    tools.rename_file(merged_file, src_file)

            #-----------------------------------------------------
            # Add Q (copy of QV) and/or PS to initial file
            #-----------------------------------------------------
            if cfg.workflow_name.startswith('icon-art'):
                meteo_file = os.path.join(
                    cfg.icon_input_icbc,
                    cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                               cfg.meteo['nameformat']) +
                    '.nc')
                if os.path.isfile(meteo_file):
                    merged_file = os.path.join(
                        cfg.icon_input_icbc,
                        cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                                   cfg.meteo['nameformat']) +
                        '_merged.nc')
                    ds = xr.open_dataset(meteo_file)
                    merging = False
                    if 'PS' not in ds:
                        if 'LNPS' not in ds:
                            raise KeyError(
                                f"'LNPS' must be found in the initial conditions file {meteo_file}"
                            )
                        merging = True
                        ds['PS'] = ds['LNPS']
                        ds['PS'].attrs = ds['LNPS'].attrs
                        ds['PS'] = np.exp(ds['PS'])
                        ds['PS'] = ds['PS'].squeeze(dim='lev_2')
                        ds['PS'].attrs["long_name"] = 'surface pressure'
                        ds['PS'].attrs['units'] = 'Pa'
                        logging.info(f"Added PS to file {meteo_file}")
                    if 'Q' not in ds:
                        merging = True
                        ds['Q'] = ds['QV']
                        logging.info(f"Added Q to file {meteo_file}")
                    if merging:
                        ds.to_netcdf(merged_file)
                        tools.rename_file(merged_file, meteo_file)

            #-----------------------------------------------------
            # In case of OEM: merge chem tracers with meteo-files
            #-----------------------------------------------------
            if cfg.workflow_name == 'icon-art-oem':
                for time in tools.iter_hours(cfg.startdate_sim,
                                             cfg.enddate_sim,
                                             cfg.meteo['inc']):
                    if time == cfg.startdate_sim:
                        #------------
                        # Merge IC:
                        #------------
                        meteo_file = os.path.join(
                            cfg.icon_input_icbc,
                            time.strftime(cfg.meteo['prefix'] +
                                          cfg.meteo['nameformat']) + '.nc')
                        if os.path.isfile(meteo_file):
                            chem_file = os.path.join(
                                cfg.icon_input_icbc, cfg.chem['prefix'] +
                                time.strftime(cfg.chem['nameformat']) + '.nc')
                            merged_file = os.path.join(
                                cfg.icon_input_icbc,
                                time.strftime(cfg.meteo['prefix'] +
                                              cfg.meteo['nameformat']) +
                                '_merged.nc')
                            ds_meteo = xr.open_dataset(meteo_file)
                            ds_chem = xr.open_dataset(chem_file)
                            # LNPS --> PS
                            ds_chem['PS'] = ds_chem['LNPS']
                            ds_chem['PS'].attrs = ds_chem['LNPS'].attrs
                            ds_chem['PS'] = ds_chem['PS'].squeeze(dim='lev_2')
                            ds_chem['PS'].attrs[
                                "long_name"] = 'surface pressure'
                            # merge:
                            ds_merged = xr.merge([ds_meteo, ds_chem],
                                                 compat="override")
                            #ds_merged.attrs = ds.attrs
                            ds_merged.to_netcdf(merged_file)
                            # Rename file to get original file name
                            tools.rename_file(merged_file, meteo_file)
                            tools.remove_file(chem_file)
                            logging.info(
                                "Added chemical tracer to file {}".format(
                                    merged_file))

                    #------------
                    # Merge LBC:
                    #------------
                    meteo_file = os.path.join(
                        cfg.icon_input_icbc,
                        time.strftime(cfg.meteo['prefix'] +
                                      cfg.meteo['nameformat']) + '_lbc.nc')
                    chem_file = os.path.join(
                        cfg.icon_input_icbc, cfg.chem['prefix'] +
                        time.strftime(cfg.chem_nameformat) + '_lbc.nc')
                    merged_file = os.path.join(
                        cfg.icon_input_icbc,
                        time.strftime(cfg.meteo['prefix'] +
                                      cfg.meteo['nameformat']) + '_merged.nc')
                    ds_meteo = xr.open_dataset(meteo_file)
                    ds_chem = xr.open_dataset(chem_file)
                    # LNPS --> PS
                    ds_chem['PS'] = ds_chem['LNPS']
                    ds_chem['PS'].attrs = ds_chem['LNPS'].attrs
                    ds_chem['PS'].attrs["long_name"] = 'surface pressure'
                    ds_chem['TRCH4_chemtr'] = ds_chem['CH4_BG']
                    # merge:
                    ds_merged = xr.merge([ds_meteo, ds_chem],
                                         compat="override")
                    #ds_merged.attrs = ds.attrs
                    ds_merged.to_netcdf(merged_file)
                    # Rename file to get original file name
                    tools.rename_file(merged_file, meteo_file)
                    tools.remove_file(chem_file)
                    logging.info(
                        "Added chemical tracer to file {}".format(merged_file))

    # If COSMO (and not ICON):
    else:
        async_error(cfg, part='COSMO')
        logging.info('COSMO analysis data for IC/BC')

        dest_path = os.path.join(cfg.int2lm_input, 'meteo')
        tools.create_dir(dest_path, "meteo input")

        source_nameformat = cfg.meteo['nameformat']
        if cfg.meteo['prefix'] == 'lffd':
            # nested runs use cosmoart-output as meteo data
            # have to copy the *c.nc-file
            src_file = os.path.join(
                cfg.meteo['dir'],
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
        subdir = os.path.join(meteo_dir,
                              cfg.startdate_sim.strftime('%y%m%d%H'))
        for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                     cfg.meteo['inc']):
            dest_path = os.path.join(cfg.int2lm_input, 'meteo')
            src_file = os.path.join(meteo_dir,
                                    time.strftime(source_nameformat))

            if cfg.meteo['prefix'] == 'efsf':
                if time == cfg.startdate_sim:
                    src_file = os.path.join(subdir,
                                            'eas' + time.strftime('%Y%m%d%H'))
                    if not os.path.isfile(src_file) and cfg.meteo.get('dir_alt') \
                        is not None:
                        meteo_dir = cfg.meteo['dir_alt']
                        subdir = os.path.join(
                            meteo_dir, cfg.startdate_sim.strftime('%y%m%d%H'))
                        src_file = os.path.join(
                            subdir, 'eas' + time.strftime('%Y%m%d%H'))
                    dest_path = os.path.join(cfg.int2lm_input, 'meteo',
                                             cfg.meteo['prefix'] + '00000000')
                else:
                    td = time - cfg.startdate_sim - timedelta(hours=6 *
                                                              num_steps)
                    days = str(td.days).zfill(2)
                    hours = str(td.seconds // 3600).zfill(2)
                    td_total = time - cfg.startdate_sim
                    days_total = str(td_total.days).zfill(2)
                    hours_total = str(td_total.seconds // 3600).zfill(2)

                    src_file = os.path.join(
                        subdir, cfg.meteo['prefix'] + days + hours + '0000')
                    dest_path = os.path.join(
                        cfg.int2lm_input, 'meteo', cfg.meteo['prefix'] +
                        days_total + hours_total + '0000')

                    # Next time, change directory
                    checkdir = os.path.join(meteo_dir,
                                            time.strftime('%y%m%d%H'))
                    if os.path.isdir(checkdir):
                        num_steps += 1
                        subdir = checkdir
                    elif cfg.meteo.get('dir_alt') is not None:
                        checkdir = os.path.join(cfg.meteo['dir_alt'],
                                                time.strftime('%y%m%d%H'))
                        if os.path.isdir(checkdir):
                            num_steps += 1
                            subdir = checkdir
                            meteo_dir = cfg.meteo['dir_alt']
                            logging.info(
                                "Switching to other input directory from {} to {}"
                                .format(cfg.meteo['dir'],
                                        cfg.meteo['dir_alt']))
            elif not os.path.exists(src_file):
                # special case for MeteoSwiss COSMO-7 data
                archive = '/store/mch/msopr/owm/COSMO-7'
                yy = time.strftime("%y")
                path = '/'.join([archive, 'ANA' + yy])
                src_file = os.path.join(path, time.strftime(source_nameformat))

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

            scratch_path = os.path.join(cfg.int2lm_input, 'icbc')
            tools.create_dir(scratch_path, "icbc input")

            for inv in inv_to_process:
                logging.info(inv["fullname"] + " files")
                tools.create_dir(inv["outdir"], "processed " + inv["fullname"])

                for p in inv["param"]:
                    inc = p["inc"]
                    for time in tools.iter_hours(cfg.startdate_sim,
                                                 cfg.enddate_sim, inc):
                        logging.info(time)

                        filename = os.path.join(
                            inv["outdir"], p["suffix"] + "_" +
                            time.strftime("%Y%m%d%H") + ".nc")
                        if not os.path.exists(filename):
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

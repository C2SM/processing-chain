#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Prepare initial and boundary conditions
#
# In case of ICON:
# Prepare input for meteorological initial and boundary conditions
# by remapping the files onto the ICON grid (for IC) and the
# auxillary lateral-boundary grid (for BC) with the DWD ICON tools
# and saving them in the input folder.
# Currently, the input files are assumed to be ifs data.
# The files are read-in in grib2-format and the the remapped
# files are saved in netCDF-format (currently only netCDF works
# for ICON when then the simulation is driven by ifs-data).
#
# result in case of success: all meteo input-files necessary are found in
#                            ${int2lm_input}/meteo/
#
# Dominik Brunner, July 2013
#
# 2013-07-16 Initial release, based on Christoph Knote script
# 2017-01-15 Modified for hypatia and project SmartCarb
# 2018-06-21 Translated to Python (kug)
# 2021-02-28 Modified for ICON-simulations (stem)
# 2021-11-12 Modified for ICON-ART-simulations (mjaehn)

import os
import glob
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


def set_cfg_variables(cfg, startdate, enddate):

    # TODO: Change setattr() to direct assignment
    if cfg.model.startswith('cosmo'):
        setattr(cfg, 'int2lm_root', os.path.join(cfg.chain_root, 'int2lm'))
        setattr(cfg, 'int2lm_input', os.path.join(cfg.int2lm_root, 'input'))
    elif cfg.model.startswith('icon'):
        setattr(cfg, 'icon_base', os.path.join(cfg.chain_root, 'icon'))
        setattr(cfg, 'icon_input', os.path.join(cfg.chain_root, 'icon',
                                                'input'))
        setattr(cfg, 'icon_input_icbc',
                os.path.join(cfg.chain_root, 'icon', 'input', 'icbc'))
        setattr(cfg, 'icon_work', os.path.join(cfg.chain_root, 'icon', 'run'))
        setattr(cfg, 'icon_output',
                os.path.join(cfg.chain_root, 'icon', 'output'))
        setattr(cfg, 'icon_output_reduced',
                os.path.join(cfg.chain_root, 'icon', 'output_reduced'))
        setattr(cfg, 'icon_restart_out',
                os.path.join(cfg.chain_root, 'icon', 'restart'))
        setattr(cfg, 'icon_restart_in',
                os.path.join(cfg.chain_root_last_run, 'icon', 'run'))
        setattr(cfg, 'icon_input_icbc_last_run',
                os.path.join(cfg.chain_root_last_run, 'icon', 'input', 'icbc'))

        cfg.input_files_scratch = {}
        for varname in cfg.input_files:
            cfg.input_files_scratch[varname] = os.path.join(
                cfg.icon_input, os.path.basename(cfg.input_files[varname]))
        cfg.create_vars_from_dicts()

        cfg.ini_datetime_string = startdate.strftime('%Y-%m-%dT%H:00:00Z')
        cfg.end_datetime_string = enddate.strftime('%Y-%m-%dT%H:00:00Z')

        if cfg.model == 'icon-art-oem':
            cfg.startdate_sim_yyyymmdd_hh = cfg.startdate_sim.strftime(
                '%Y%m%d_%H')

    return cfg


def main(startdate, enddate, cfg, model_cfg):
    """
    **ICON** 

     Create necessary directories ``cfg.icon_input_icbc``
     and ''cfg.icon_work''

     Submitting the runscript for the DWD ICON tools to remap the meteo files.

     All runscripts specified in ``cfg.icontools_runjobs`` are submitted.

     The meteo files are read-in from the original input directory 
     (``cfg.input_root_meteo``) and the remapped meteo files are
     saved in the input folder on scratch (``cfg.icon_input/icbc``).

     The constant variable 'GEOSP' is added to the files not containing it
     using python-cdo bindings.

    **COSMO**

     Copy meteo files to **int2lm** input.

     Create necessary directory ``cfg.int2lm_input/meteo``. Copy meteo files
     from project directory (``cfg.meteo['dir']/cfg.meteo['prefix']YYYYMMDDHH``) to
     int2lm input folder on scratch (``cfg.int2lm_input/meteo``).

     For nested runs (meteo files are cosmo-output: ``cfg.meteo['prefix'] == 
     'lffd'``), also the ``*c.nc``-file with constant parameters is copied.

    
    Parameters
    ----------
    startdate : datetime-object
        The start date of the simulation
    enddate : datetime-object
        The end date of the simulation
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """

    cfg = set_cfg_variables(cfg, startdate, enddate)

    if cfg.model.startswith('icon'):
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
        for varname in cfg.input_files:
            varname_scratch = f'{varname}_scratch'
            tools.copy_file(cfg.input_files[varname],
                            cfg.input_files_scratch[varname],
                            output_log=True)

        if cfg.model == 'icon-art-global':
            # -- Download ERA5 data and create the inicond file
            if cfg.era5_inicond and cfg.lrestart == '.FALSE.':
                # -- Fetch ERA5 data
                fetch_era5(startdate, cfg.icon_input_icbc)

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
                    filename = cfg.inicond_filename_scratch

                    # -- Copy the script for processing external tracer data in workdir
                    with open(cfg.icon_species_inijob) as input_file:
                        to_write = input_file.read()
                    output_file = os.path.join(cfg.icon_input_icbc,
                                               'icon_species_inicond.sh')
                    with open(output_file, "w") as outf:
                        outf.write(
                            to_write.format(cfg=cfg,
                                            filename=filename,
                                            ext_restart=ext_restart,
                                            year=startdate.year,
                                            month=startdate.month,
                                            day=startdate.day))

                    # -- Run ERA5 processing script
                    process = subprocess.Popen([
                        "bash",
                        os.path.join(cfg.icon_input_icbc,
                                     'icon_species_inicond.sh')
                    ],
                                               stdout=subprocess.PIPE)
                    process.communicate()

                    # -- Create initial conditions for OH concentrations
                    if 'TROH' in cfg.species2restart:
                        create_oh_for_inicond(cfg, startdate.month)

                else:

                    # -- Check the extension of tracer variables in the restart file
                    ds_restart = xr.open_dataset(cfg.restart_filename_scratch)
                    tracer_name = cfg.species2restart[0]
                    var_restart = [
                        var for var in ds_restart.data_vars.keys()
                        if var.startswith(tracer_name)
                    ][0]
                    ext_restart = var_restart.replace(tracer_name, '')
                    filename = cfg.restart_filename_scratch

                    # -- Change OH concentrations in the restart file
                    if 'TROH' in cfg.species2restart:
                        create_oh_for_restart(cfg, startdate.month,
                                              ext_restart)

            # -----------------------------------------------------
            # Create meteorological and tracer nudging conditions
            # -----------------------------------------------------

            # -- If global nudging, download and process ERA5 and CAMS data
            if cfg.era5_global_nudging:

                for time in tools.iter_hours(startdate,
                                             enddate,
                                             step=cfg.nudging_step):

                    # -- Give a name to the nudging file
                    timestr = time.strftime('%Y%m%d%H')
                    filename = 'era2icon_R2B03_{timestr}_nudging.nc'.format(
                        timestr=timestr)

                    # -- If initial time, copy the initial conditions to be used as boundary conditions
                    if time == startdate and cfg.era5_inicond:
                        shutil.copy(
                            cfg.inicond_filename_scratch,
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

            # -----------------------------------------------------
            # Create symlink to the restart file if lrestart is True
            # -----------------------------------------------------
            if cfg.lrestart == '.TRUE.':
                os.symlink(cfg.restart_filename_scratch,
                           os.path.join(cfg.icon_work, 'restart_atm_DOM01.nc'))

        else:  # non-global ICON-ART
            #-----------------------------------------------------
            # Create LBC datafile lists (each at 00 UTC and others)
            #-----------------------------------------------------
            datafile_list = []
            datafile_list_rest = []
            datafile_list_chem = []
            for time in tools.iter_hours(startdate, enddate, cfg.meteo['inc']):
                meteo_file = os.path.join(
                    cfg.icon_input_icbc, cfg.meteo['prefix'] +
                    time.strftime(cfg.meteo['nameformat']))
                if cfg.model == 'icon-art' or cfg.model == 'icon-art-oem':
                    chem_file = os.path.join(
                        cfg.icon_input_icbc,
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
                result = subprocess.run([
                    "sbatch", "--wait",
                    os.path.join(cfg.icon_work, "%s.job" % runscript)
                ])
                exitcode = result.returncode
                if exitcode != 0:
                    raise RuntimeError(
                        "sbatch returned exitcode {}".format(exitcode))
                logging.info("%s successfully executed." % runscript)

            #-----------------------------------------------------
            # Add GEOSP to all meteo files
            #-----------------------------------------------------
            for time in tools.iter_hours(startdate, enddate, cfg.meteo['inc']):
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
                    geosp_src_file = os.path.join(cfg.icon_input_icbc_last_run,
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
            if cfg.model.startswith('icon-art'):
                meteo_file = os.path.join(
                    cfg.icon_input_icbc,
                    startdate.strftime(cfg.meteo['prefix'] +
                                       cfg.meteo['nameformat']) + '.nc')
                merged_file = os.path.join(
                    cfg.icon_input_icbc,
                    startdate.strftime(cfg.meteo['prefix'] +
                                       cfg.meteo['nameformat']) + '_merged.nc')
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
            if cfg.model == 'icon-art-oem':
                for time in tools.iter_hours(startdate, enddate,
                                             cfg.meteo['inc']):
                    if time == startdate:
                        #------------
                        # Merge IC:
                        #------------
                        meteo_file = os.path.join(
                            cfg.icon_input_icbc,
                            time.strftime(cfg.meteo['prefix'] +
                                          cfg.meteo['nameformat']) + '.nc')
                        chem_file = os.path.join(
                            cfg.icon_input_icbc,
                            time.strftime(cfg.chem_nameformat) + '.nc')
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
                        ds_chem['PS'].attrs["long_name"] = 'surface pressure'
                        # merge:
                        ds_merged = xr.merge([ds_meteo, ds_chem],
                                             compat="override")
                        #ds_merged.attrs = ds.attrs
                        ds_merged.to_netcdf(merged_file)
                        # Rename file to get original file name
                        tools.rename_file(merged_file, meteo_file)
                        tools.remove_file(chem_file)
                        logging.info("Added chemical tracer to file {}".format(
                            merged_file))

                    #------------
                    # Merge LBC:
                    #------------
                    meteo_file = os.path.join(
                        cfg.icon_input_icbc,
                        time.strftime(cfg.meteo['prefix'] +
                                      cfg.meteo['nameformat']) + '_lbc.nc')
                    chem_file = os.path.join(
                        cfg.icon_input_icbc,
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
        logging.info('COSMO analysis data for IC/BC')

        dest_path = os.path.join(cfg.int2lm_input, 'meteo')
        tools.create_dir(dest_path, "meteo input")

        source_nameformat = cfg.meteo['nameformat']
        if cfg.meteo['prefix'] == 'lffd':
            # nested runs use cosmoart-output as meteo data
            # have to copy the *c.nc-file
            src_file = os.path.join(
                cfg.meteo['dir'],
                startdate.strftime(source_nameformat + 'c.nc'))

            tools.copy_file(src_file, dest_path, output_log=True)

            logging.info("Copied constant-param file from {} to {}".format(
                src_file, dest_path))

            # extend nameformat with ending to match cosmo-output
            source_nameformat += '.nc'

        if cfg.meteo['prefix'] == 'efsf':
            source_nameformat = cfg.meteo['prefix'] + '%y%m%d%H'

        num_steps = 0
        meteo_dir = cfg.meteo['dir']
        subdir = os.path.join(meteo_dir, startdate.strftime('%y%m%d%H'))
        for time in tools.iter_hours(startdate, enddate, cfg.meteo['inc']):
            dest_path = os.path.join(cfg.int2lm_input, 'meteo')
            src_file = os.path.join(meteo_dir,
                                    time.strftime(source_nameformat))

            if cfg.meteo['prefix'] == 'efsf':
                if time == startdate:
                    src_file = os.path.join(subdir,
                                            'eas' + time.strftime('%Y%m%d%H'))
                    if not os.path.isfile(src_file) and cfg.meteo.get('dir_alt') \
                        is not None:
                        meteo_dir = cfg.meteo['dir_alt']
                        subdir = os.path.join(meteo_dir,
                                              startdate.strftime('%y%m%d%H'))
                        src_file = os.path.join(
                            subdir, 'eas' + time.strftime('%Y%m%d%H'))
                    dest_path = os.path.join(cfg.int2lm_input, 'meteo',
                                             cfg.meteo['prefix'] + '00000000')
                else:
                    td = time - startdate - timedelta(hours=6 * num_steps)
                    days = str(td.days).zfill(2)
                    hours = str(td.seconds // 3600).zfill(2)
                    td_total = time - startdate
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
        if cfg.model == 'cosmo-ghg':
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
        elif cfg.model == 'cosmo-art':
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

        if cfg.model == 'cosmo-ghg' or cfg.model == 'cosmo-art':
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
                    for time in tools.iter_hours(startdate, enddate, inc):
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

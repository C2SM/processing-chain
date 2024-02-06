#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import xarray as xr
import numpy as np
from . import tools, prepare_icon

BASIC_PYTHON_JOB = True


def main(cfg):
    """
    - Add GEOSP to all meteo files
    - Submit the runscript for the DWD ICON tools to remap the meteorological files.
    - All runscripts specified in ``cfg.icontools_runjobs`` are submitted.
    - The meteorological files are read from the original input directory
      (``cfg.input_root_meteo``), and the remapped meteorological files are saved
      in the input folder on scratch (``cfg.icon_input/icbc``).
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)

    #-----------------------------------------------------
    # Create LBC datafile lists (each at 00 UTC and others)
    #-----------------------------------------------------
    datafile_list = []
    datafile_list_rest = []
    datafile_list_chem = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo['inc']):
        meteo_file = cfg.icon_input_icbc / (
            cfg.meteo['prefix'] + time.strftime(cfg.meteo['nameformat']))
        if hasattr(cfg, 'art_input_folder'):
            chem_file = cfg.icon_input_icbc / (
                cfg.chem['prefix'] + time.strftime(cfg.chem_nameformat))
            datafile_list_chem.append(str(chem_file) + cfg.chem['suffix'])
        if str(meteo_file).endswith('00'):
            datafile_list.append(str(meteo_file) + cfg.meteo['suffix'])
        else:
            datafile_list_rest.append(str(meteo_file) + cfg.meteo['suffix'])
    datafile_list = ' '.join([str(v) for v in datafile_list])
    datafile_list_rest = ' '.join([str(v) for v in datafile_list_rest])
    datafile_list_chem = ' '.join([str(v) for v in datafile_list_chem])

    #-----------------------------------------------------
    # Write and submit ICONTOOLS runscripts
    #-----------------------------------------------------
    dep_id = None
    for runscript in cfg.icontools_runjobs:
        with (cfg.case_path / runscript).open() as input_file:
            to_write = input_file.read()
        runscript_path = cfg.icon_work / f"{runscript}.job"
        with runscript_path.open("w") as outf:
            outf.write(
                to_write.format(cfg=cfg,
                                meteo=cfg.meteo,
                                logfile=cfg.logfile,
                                logfile_finish=cfg.logfile_finish,
                                datafile_list=datafile_list,
                                datafile_list_rest=datafile_list_rest,
                                datafile_list_chem=datafile_list_chem))

        # Submitting icontools runscripts sequentially
        logging.info(f" Starting icontools runscript {runscript}.")
        dep_id = cfg.submit('icontools', runscript_path, add_dep=dep_id)

    logging.info("Add GEOSP to all meteo files")
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo['inc']):
        # Specify file names
        geosp_filename = time.replace(
            hour=0).strftime(cfg.meteo['prefix'] +
                             cfg.meteo['nameformat']) + '_lbc.nc'
        geosp_file = os.path.join(cfg.icon_input_icbc, geosp_filename)
        src_filename = time.strftime(cfg.meteo['prefix'] +
                                     cfg.meteo['nameformat']) + '_lbc.nc'
        src_file = os.path.join(cfg.icon_input_icbc, src_filename)
        merged_filename = time.strftime(cfg.meteo['prefix'] +
                                        cfg.meteo['nameformat']) + '_merged.nc'
        merged_file = os.path.join(cfg.icon_input_icbc, merged_filename)

        # Copy GEOSP file from last run if not present
        if hasattr(cfg, 'icon_input_icbc_prev') and not os.path.exists(geosp_file):
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
            da_geosp = da_geosp.assign_coords(time=[np.datetime64(time)])
            # Merge GEOSP into temporary file
            ds_merged = xr.merge([ds, da_geosp])
            ds_merged.attrs = ds.attrs
            ds_merged.to_netcdf(merged_file)
            # Logging info for merging GEOSP
            logging.info("Added GEOSP to file {}".format(merged_file))
            # Rename file to get original file name
            tools.rename_file(merged_file, src_file)
    logging.info('OK')

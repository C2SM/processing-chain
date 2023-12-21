#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from pathlib import Path
import logging
from datetime import timedelta
import xarray as xr
import numpy as np
from . import tools, prepare_data

def main(cfg):
    """
    Add GEOSP
    """
    prepare_data.set_cfg_variables(cfg)

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

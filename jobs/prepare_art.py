#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import numpy as np
from . import tools, prepare_icon


def main(cfg):
    """
    Prepare ICON-ART simulations.

    - Add GEOSP to all meteo files that don't contain it
    - Add Q (copy of QV) and/or PS to initial file
    """
    prepare_icon.set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("prepare_art")

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

    logging.info('Add Q (copy of QV) and/or PS to initial file')
    meteo_file = os.path.join(
        cfg.icon_input_icbc,
        cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                   cfg.meteo['nameformat']) + '.nc')
    if os.path.isfile(meteo_file):
        merged_file = os.path.join(
            cfg.icon_input_icbc,
            cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
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
    logging.info('OK')

    cfg.finish_time_logging("prepare_art", launch_time)

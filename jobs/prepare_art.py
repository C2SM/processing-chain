#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import numpy as np
from . import tools, prepare_icon

BASIC_PYTHON_JOB = True


def main(cfg):
    """
    Prepare ICON-ART simulations.

    - Add GEOSP to all meteo files that don't contain it
    - Add Q (copy of QV) and/or PS to initial file
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)

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

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
from . import tools, prepare_data


def main(cfg):
    """
    This function sets up the necessary configuration variables, logs the
    initialization time, and merges chemical tracers with meteorological files if
    OEM (Online Emission Model) is enabled. The merging process involves
    combining initial and lateral boundary condition (IC and LBC) files for the
    ICON model.

    **IC Merging (Initial Conditions):**
    For the initial time step, it merges the IC files by:
    - Combining meteorological and chemical files.
    - Adjusting variables such as converting LNPS to PS for surface pressure.
    - Saving the merged dataset to a new NetCDF file.
    - Renaming and cleaning up the files.

    **LBC Merging (Lateral Boundary Conditions):**
    For subsequent time steps, it merges the LBC files by:
    - Combining meteorological and chemical files.
    - Adjusting variables such as converting LNPS to PS for surface pressure
      and renaming specific variables.
    - Saving the merged dataset to a new NetCDF file.
    - Renaming and cleaning up the files.

    This function utilizes the :func:`prepare_data.set_cfg_variables`
    function to set configuration variables, :mod:`xarray` (xr) for handling
    NetCDF files, and various utility functions from the :mod:`tools` module for
    file operations.
    """
    prepare_data.set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("prepare_art")

    if cfg.input_files['oem_gridded_emissions_nc']:
        for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
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
                                      cfg.meteo['nameformat']) + '_merged.nc')
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
                    logging.info(
                        "Added chemical tracer to file {}".format(merged_file))

            #------------
            # Merge LBC:
            #------------
            meteo_file = os.path.join(
                cfg.icon_input_icbc,
                time.strftime(cfg.meteo['prefix'] + cfg.meteo['nameformat']) +
                '_lbc.nc')
            chem_file = os.path.join(
                cfg.icon_input_icbc, cfg.chem['prefix'] +
                time.strftime(cfg.chem_nameformat) + '_lbc.nc')
            merged_file = os.path.join(
                cfg.icon_input_icbc,
                time.strftime(cfg.meteo['prefix'] + cfg.meteo['nameformat']) +
                '_merged.nc')
            ds_meteo = xr.open_dataset(meteo_file)
            ds_chem = xr.open_dataset(chem_file)
            # LNPS --> PS
            ds_chem['PS'] = ds_chem['LNPS']
            ds_chem['PS'].attrs = ds_chem['LNPS'].attrs
            ds_chem['PS'].attrs["long_name"] = 'surface pressure'
            ds_chem['TRCH4_chemtr'] = ds_chem['CH4_BG']
            # merge:
            ds_merged = xr.merge([ds_meteo, ds_chem], compat="override")
            #ds_merged.attrs = ds.attrs
            ds_merged.to_netcdf(merged_file)
            # Rename file to get original file name
            tools.rename_file(merged_file, meteo_file)
            tools.remove_file(chem_file)
            logging.info(
                "Added chemical tracer to file {}".format(merged_file))

    cfg.finish_time_logging("prepare_art_oem", launch_time)
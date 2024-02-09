#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
from . import tools, prepare_icon

BASIC_PYTHON_JOB = True


def main(cfg):
    """
    ICON-ART-OEM preparations
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    logging.info('Merging IC and LBC')

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
            # Remapping chemical tracer names
            if "remap_tracers" in cfg.chem:
                for chem_in, chem_out in cfg.chem['remap_tracers'].items():
                    ds_chem[chem_out] = ds_chem[chem_in]
            # merge:
            ds_merged = xr.merge([ds_meteo, ds_chem], compat="override")
            #ds_merged.attrs = ds.attrs
            ds_merged.to_netcdf(merged_file)
            # Rename file to get original file name
            tools.rename_file(merged_file, meteo_file)
            tools.remove_file(chem_file)
            logging.info(
                "Added chemical tracer to file {}".format(merged_file))

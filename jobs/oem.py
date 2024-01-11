#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging

from . import tools, cosmo

BASIC_PYTHON_JOB = True


def main(cfg):
    """Copy emission and profile files to the **cosmo** or **icon** input
    directory.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.

    Raises
    ------
    RuntimeError
        If an error occurs during the process.
    """
    tools.change_logfile(cfg.logfile)
    cosmo.set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("oem")

    oem_dir = cfg.oem['dir']
    oem_gridded_emissions_nc = os.path.join(oem_dir,
                                            cfg.oem['gridded_emissions_nc'])
    oem_vertical_profiles_nc = os.path.join(oem_dir,
                                            cfg.oem['vertical_profiles_nc'])

    # Temporal profiles can be given as hourofday, dayofweek, monthofyear
    # AND/OR as hourofyear. We copy all files indicated in cfg, but make
    # sure at least one type is present
    hod_tps = True
    hoy_tps = True
    try:
        oem_hourofday_nc = os.path.join(oem_dir, cfg.oem['hourofday_nc'])
        oem_dayofweek_nc = os.path.join(oem_dir, cfg.oem['dayofweek_nc'])
        oem_monthofyear_nc = os.path.join(oem_dir, cfg.oem['monthofyear_nc'])
    except AttributeError:
        hod_tps = False
    try:
        oem_hourofyear_nc = os.path.join(oem_dir, cfg.oem['hourofyear_nc'])
    except AttributeError:
        hoy_tps = False

    if not (hod_tps or hoy_tps):
        raise RuntimeError("At least one of (hod/dow/moy) or (hoy) netcdfs "
                           " have to be given for online emissions")

    if cfg.workflow_name.startswith('icon'):
        input_dir = cfg.icon_input
    else:
        input_dir = cfg.cosmo_input
    dest_dir = os.path.join(input_dir, "oem")
    tools.create_dir(dest_dir, "online emissions input")

    logging.info("Copying oem files from {} to {}".format(oem_dir, dest_dir))

    if hod_tps:
        tools.copy_file(
            oem_gridded_emissions_nc,
            os.path.join(dest_dir, cfg.oem['gridded_emissions_nc']))
        tools.copy_file(
            oem_vertical_profiles_nc,
            os.path.join(dest_dir, cfg.oem['vertical_profiles_nc']))
        tools.copy_file(oem_hourofday_nc,
                        os.path.join(dest_dir, cfg.oem['hourofday_nc']))
        tools.copy_file(oem_dayofweek_nc,
                        os.path.join(dest_dir, cfg.oem['dayofweek_nc']))
        tools.copy_file(oem_monthofyear_nc,
                        os.path.join(dest_dir, cfg.oem['monthofyear_nc']))
    if hoy_tps:
        tools.copy_file(oem_hourofyear_nc,
                        os.path.join(dest_dir, cfg.oem['hourofyear_nc']))

    # Additional files for ICON simulations
    if cfg.oem.get('ens_reg_nc') is not None:
        tools.copy_file(os.path.join(oem_dir, cfg.oem['ens_reg_nc']),
                        os.path.join(dest_dir, cfg.oem['ens_reg_nc']))
    if cfg.oem.get('oem_ens_lambda_nc') is not None:
        tools.copy_file(os.path.join(oem_dir, cfg.oem['ens_lambda_nc']),
                        os.path.join(dest_dir, cfg.oem['ens_lambda_nc']))

    cfg.finish_time_logging("oem", launch_time)

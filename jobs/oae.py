#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import logging

from . import tools


def main(starttime, hstart, hstop, cfg, model_cfg):
    """Copy emission and profile files to the **cosmo** or **icon** input
    directory.

    Parameters
    ----------
    starttime : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the starttime
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """

    oae_dir = cfg.oae_dir
    oae_gridded_emissions_nc = os.path.join(oae_dir,
                                            cfg.oae_gridded_emissions_nc)
    oae_vertical_profiles_nc = os.path.join(oae_dir,
                                            cfg.oae_vertical_profiles_nc)

    # Temporal profiles can be given as hourofday, dayofweek, monthofyear
    # AND/OR as hourofyear. We copy all files indicated in cfg, but make
    # sure at least one type is present
    hod_tps = True
    hoy_tps = True
    try:
        oae_hourofday_nc = os.path.join(oae_dir, cfg.oae_hourofday_nc)
        oae_dayofweek_nc = os.path.join(oae_dir, cfg.oae_dayofweek_nc)
        oae_monthofyear_nc = os.path.join(oae_dir, cfg.oae_monthofyear_nc)
    except AttributeError:
        hod_tps = False
    try:
        oae_hourofyear_nc = os.path.join(oae_dir, cfg.oae_hourofyear_nc)
    except AttributeError:
        hoy_tps = False

    if not (hod_tps or hoy_tps):
        raise RuntimeError("At least one of (hod/dow/moy) or (hoy) netcdfs "
                           " have to be given for online emissions")

    if cfg.model.startswith('icon'):
        input_dir = cfg.icon_input
    else:
        input_dir = cfg.cosmo_input
    dest_dir = os.path.join(input_dir, "oae")
    tools.create_dir(dest_dir, "online emissions input")

    logging.info("Copying oae files from {} to {}".format(oae_dir, dest_dir))

    if hod_tps:
        tools.copy_file(oae_gridded_emissions_nc,
                        os.path.join(dest_dir, cfg.oae_gridded_emissions_nc))
        tools.copy_file(oae_vertical_profiles_nc,
                        os.path.join(dest_dir, cfg.oae_vertical_profiles_nc))
        tools.copy_file(oae_hourofday_nc,
                        os.path.join(dest_dir, cfg.oae_hourofday_nc))
        tools.copy_file(oae_dayofweek_nc,
                        os.path.join(dest_dir, cfg.oae_dayofweek_nc))
        tools.copy_file(oae_monthofyear_nc,
                        os.path.join(dest_dir, cfg.oae_monthofyear_nc))
    if hoy_tps:
        tools.copy_file(oae_hourofyear_nc,
                        os.path.join(dest_dir, cfg.oae_hourofyear_nc))

    # Additional files for ICON simulations
    if hasattr(cfg, 'oae_ens_reg_nc'):
        tools.copy_file(os.path.join(oae_dir, cfg.oae_ens_reg_nc),
                        os.path.join(dest_dir, cfg.oae_ens_reg_nc))
    if hasattr(cfg, 'oae_ens_lambda_nc'):
        tools.copy_file(os.path.join(oae_dir, cfg.oae_ens_lambda_nc),
                        os.path.join(dest_dir, cfg.oae_ens_lambda_nc))

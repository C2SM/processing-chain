#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import logging

from . import tools


def main(starttime, hstart, hstop, cfg):
    """Copy emission and profile files to the **cosmo** input directory.

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

    dest_emissions = 'emissions.nc'
    dest_vertical_profiles = 'vertical_profiles.nc'
    dest_dayofweek = 'dayofweek.nc'
    dest_hourofday = 'hourofday.nc'
    dest_hourofyear = 'hourofyear.nc'
    dest_monthofyear = 'monthofyear.nc'

    oae_dir = cfg.oae_dir
    oae_gridded_emissions_nc = os.path.join(oae_dir,
                                            cfg.oae_gridded_emissions_nc)
    oae_vertical_profiles_nc = os.path.join(oae_dir,
                                            cfg.oae_vertical_profiles_nc)
    oae_dayofweek_nc = os.path.join(oae_dir, cfg.oae_dayofweek_nc)
    oae_hourofday_nc = os.path.join(oae_dir, cfg.oae_hourofday_nc)
    oae_hourofyear_nc = os.path.join(oae_dir, cfg.oae_hourofyear_nc)
    oae_monthofyear_nc = os.path.join(oae_dir, cfg.oae_monthofyear_nc)

    dest_dir = os.path.join(cfg.cosmo_input, "oae")
    tools.create_dir(dest_dir, "online emissions input")

    logging.info("Copying oae files from {} to {}"
                 .format(oae_dir, dest_dir))

    tools.copy_file(oae_gridded_emissions_nc,
                    os.path.join(dest_dir, dest_emissions))
    tools.copy_file(oae_vertical_profiles_nc,
                    os.path.join(dest_dir, dest_vertical_profiles))
    tools.copy_file(oae_dayofweek_nc,
                    os.path.join(dest_dir, dest_dayofweek))
    tools.copy_file(oae_hourofday_nc,
                    os.path.join(dest_dir, dest_hourofday))
    tools.copy_file(oae_hourofyear_nc,
                    os.path.join(dest_dir, dest_hourofyear))
    tools.copy_file(oae_monthofyear_nc,
                    os.path.join(dest_dir, dest_monthofyear))

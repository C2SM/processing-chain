#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import logging
import shutil

from . import tools


def main(starttime, hstart, hstop, cfg):
    """Copy MODIS surface reflectance data to the **cosmo** input directory.

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

    dest_modis = 'modis.nc'
    modis_dir = cfg.modis_dir

    modis_data_nc = os.path.join(modis_dir, cfg.modis_filename)

    dest_dir = os.path.join(cfg.cosmo_input,"modis")
    tools.create_dir(dest_dir, "modis data for vprm")

    logging.info("Copying MODIS file from {} to {}"
                 .format(modis_dir, dest_dir))

    tools.copy_file(modis_data_nc, os.path.join(dest_dir, dest_modis))


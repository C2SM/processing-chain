#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import logging
import shutil

from . import tools


def main(starttime, hstart, hstop, cfg, model_cfg):
    """Copy MODIS surface reflectance data and vegatation class fraction file
    to the **cosmo** input directory.

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
    dest_vegetation = 'vegetation.nc'
    modis_dir = cfg.online_vprm_dir
    vegetation_dir = cfg.online_vprm_dir

    dest_dir = os.path.join(cfg.cosmo_input, "vprm")
    tools.create_dir(dest_dir, "input data for vprm")

    modis_data_nc = os.path.join(modis_dir, cfg.modis_filename)
    logging.info("Copying MODIS file from {} to {}".format(
        modis_dir, dest_dir))
    tools.copy_file(modis_data_nc, os.path.join(dest_dir, dest_modis))

    vegetation_data_nc = os.path.join(vegetation_dir, cfg.vegetation_filename)
    logging.info("Copying vegetation class fraction file from {} to {}".format(
        vegetation_dir, dest_dir))
    tools.copy_file(vegetation_data_nc, os.path.join(dest_dir,
                                                     dest_vegetation))

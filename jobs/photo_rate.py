#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import shutil

from . import tools

def main(starttime,hstart,hstop,cfg):
    """Copy photolysis-rate file to the **COSMO** input directory.

    Only necessary for **COSMOART** simulations.

    Copy the photolysis-rate file from the project (``cfg.photo_rate_file``) to
    the **COSMO** input folder on scratch (``cfg.cosmo_input/art_photolysis``).

    Parameters
    ----------	
    start_time : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the start_time
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    tools.check_target(cfg, 'cosmoart')
    
    logging.info("Copying photolysis-rate file from {} to {}"
                 .format(cfg.photo_rate_file,
                         os.path.join(cfg.cosmo_input,
                                      "art_photolysis",
                                      "papa_data.p")))

    try:
        os.makedirs(os.path.join(cfg.cosmo_input,"art_photolysis"), exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating photolysis rate input dir failed")
        raise

    src_file = cfg.photo_rate_file
    dest_path = os.path.join(cfg.cosmo_input, 'art_photolysis', 'papa_data.p')
    try:
        shutil.copy(src_file, dest_path)
    except FileNotFoundError:
        logging.error("Emission input file not found at {}, or output"
                      " directory doesn't exist to copy {}"
                      .format(src_file, dest_path))
        raise
    except (PermissionError, OSError):
        logging.error("Copying emission data file failed")
        raise

    logging.info("Finished")

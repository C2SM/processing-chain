#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import shutil

from . import tools

def main(starttime, hstart, hstop, cfg):
    """Copy ifs_hres_bc files to the **int2lm** input directory.

    Only necessary for **COSMOART** simulations.

    Copy ifs_hres files from project folder (``cfg.ifs_hres_dir``) to
    **int2lm** input folder on scratch (``cfg.int2lm_input/ifs_hres_bc``).

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

    logging.info("Copying ifs_hres_bc files from {} to {}"
                 .format(cfg.ifs_hres_dir,
                         os.path.join(cfg.int2lm_input,"ifs_hres_bc")))

    try:
        os.makedirs(os.path.join(cfg.int2lm_input,"ifs_hres_bc"), exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating ifs hres input dir failed")
        raise

    for time in tools.iter_hours(starttime, hstart, hstop, cfg.ifs_hres_inc):
        logging.info(time)
        src_file = os.path.join(cfg.ifs_hres_dir,
                                time.strftime(cfg.ifs_basename+'%Y%m%d%H'))
        dest_path = os.path.join(cfg.int2lm_input,
                                 time.strftime('ifs_hres_bc/eas%Y%m%d%H'))
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
        logging.info("Copied {}".format(time.strftime('ifs_hres_bc/eas%Y%m%d%H')))

    logging.info("Finished")

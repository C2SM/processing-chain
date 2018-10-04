#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import shutil

from datetime import timedelta
from . import tools

def main(starttime,hstart,hstop,cfg):
    """Copy and rename the obs_nudging files to the **COSMO** input directory.

    In the folder ``cfg.obs_nudging_dir``, the files are saved in the format
    ``{prefix}-YYYYMMDD000000-YYYYMMDD000000``. **COSMO** expects files in the 
    format ``{prefix}x``, where ```x`` is ``[nothing], .2, .3, .4, ...``. This 
    job handles this filename-change and copies them to the **COSMO** input 
    folder on scratch (``cfg.cosmo_input/obs_nudging``).

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

    dest_dir = os.path.join(cfg.cosmo_input,"obs_nudging")

    logging.info("Copying obs_nudging files from {} to {}"
                 .format(cfg.obs_nudging_dir,
                         dest_dir))

    try:
        os.makedirs(dest_dir, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating obs nudging input dir failed")
        raise

    count = 0  # running index for COSMOART
    for time in tools.iter_hours(starttime, hstart, hstop, step=24):
        count += 1
        for prefix in cfg.obs_nudging_prefixes:
            src_filename = (prefix +
                            time.strftime(cfg.obs_nudging_date_format) +
                            (time + timedelta(days=1))
                            .strftime(cfg.obs_nudging_date_format)
                           )
            src_path = os.path.join(cfg.obs_nudging_dir, src_filename)
            if count == 1:
                dest_path = os.path.join(dest_dir, prefix)
            else:
                dest_path = os.path.join(dest_dir, "{}.{}".format(prefix, count))

            try:
                shutil.copy(src_path, dest_path)
            except FileNotFoundError:
                logging.error("Emission input file not found at {}, or output"
                              " directory doesn't exist to copy {}"
                              .format(src_file, dest_path))
                raise
            except (PermissionError, OSError):
                logging.error("Copying emission data file failed")
                raise
            logging.info("Copied file {} to {}".format(src_filename, dest_path))

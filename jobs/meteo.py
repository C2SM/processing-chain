#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Prepare input for meteorological initial and boundary conditions
# by copying files from OSM archive to INT2LM input folder
# Currently, the input files are assumed to be COSMO-7 or
# COSMO pre-2007 analysis files.
#
# result in case of success: all meteo input-files necessary are found in 
#                            ${int2lm_input}/meteo/
#
# Dominik Brunner, July 2013
#
# 2013-07-16 Initial release, based on Christoph Knote script
# 2017-01-15 Modified for hypatia and project SmartCarb
# 2018-06-21 Translated to Python (kug)

### DEVELOPMENT VERSION ###

import os
import logging
import shutil

from datetime import timedelta

from . import tools


def main(starttime, hstart, hstop, cfg):
    """Copy meteo files to **int2lm** input.

    Create necessary directory ``cfg.int2lm_input/meteo``. Copy meteo files
    from project directory (``cfg.meteo_dir/cfg.meteo_prefixYYYYMMDDHH``) to
    int2lm input folder on scratch (``cfg.int2lm_input/meteo``).

    For nested runs (meteo files are cosmo-output: ``cfg.meteo_prefix == 
    'lffd'``), also the ``*c.nc``-file with constant parameters is copied.
    
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
    logging.info('COSMO analysis data for IC/BC')

    dest_path = os.path.join(cfg.int2lm_input, 'meteo')
    tools.create_dir(dest_path, "meteo input")

    source_nameformat = cfg.meteo_prefix + '%Y%m%d%H'
    if cfg.meteo_prefix == 'lffd':
        # nested runs use cosmoart-output as meteo data
        # have to copy the *c.nc-file
        time = starttime + timedelta(hours = hstart)
        src_file = os.path.join(cfg.meteo_dir,
                                time.strftime(source_nameformat + 'c.nc'))
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
        logging.info("Copied constant-param file from {} to {}"
                     .format(src_file, dest_path))

        # extend nameformat with ending to match cosmo-output
        source_nameformat += '.nc'

    for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
        src_file = os.path.join(cfg.meteo_dir,
                                time.strftime(source_nameformat))

        if not os.path.exists(src_file):
            # TODO: if meteo file not in cfg.meteo_dir, try copy file from
            # /store/s83/osm//LA%Y/%Y%m%d/coars/laf%Y%m%d%H on ela.cscs.ch
            # using scp (requires account with access to MeteoSwiss
            pass

        # copy meteo file from project folder to
        try:
            shutil.copy(src_file, dest_path)
        except FileNotFoundError:
            logging.error("Meteo input not found at %s" % src_file)
            raise
        except (PermissionError, OSError):
            logging.error("Copying meteo data file failed")
            raise

        logging.info("Copied file from {} to {}".format(src_file, dest_path))

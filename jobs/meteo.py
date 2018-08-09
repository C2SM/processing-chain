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

from . import tools


def main(starttime, hstart, hstop, cfg):
    """
    Copy meteo files from project folder (cfg.meteo_dir) to INT2LM input folder
    on scratch (cfg.int2lm_input/meteo)
    """
    logfile=os.path.join(cfg.log_working_dir,"meteo")
    logfile_finish=os.path.join(cfg.log_finished_dir,"meteo")
    tools.change_logfile(logfile)
        

    logging.info('COSMO analysis data for IC/BC')

    scratch_path = os.path.join(cfg.int2lm_input, 'meteo')

    try:
        os.makedirs(scratch_path, exist_ok=True)
        os.makedirs(cfg.meteo_dir, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating meteo input folder failed")
        raise

    for time in tools.iter_hours(starttime, hstart, hstop):
        logging.info(time)

        filename = os.path.join(cfg.meteo_dir, time.strftime('laf%Y%m%d%H'))

        if not os.path.exists(filename):
            # TODO: if meteo file not in cfg.meteo_dir, try copy file from
            # /store/s83/osm//LA%Y/%Y%m%d/coars/laf%Y%m%d%H on ela.cscs.ch
            # using scp (requires account with access to MeteoSwiss
            pass

        # copy meteo file from project folder to
        try:
            shutil.copy(filename, scratch_path)
        except FileNotFoundError:
            logging.error("Meteo input not found at %s" % filename)
            raise
        except (PermissionError, OSError):
            logging.error("Copying meteo data file failed")
            raise
        except:
            raise

    shutil.copy(logfile, logfile_finish)

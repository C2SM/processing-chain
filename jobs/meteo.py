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
    starttime : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the starttime
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    logging.info('COSMO analysis data for IC/BC')

    dest_path = os.path.join(cfg.int2lm_input, 'meteo')
    tools.create_dir(dest_path, "meteo input")

    source_nameformat = cfg.meteo_prefix + '%Y%m%d%H'
    starttime_real = starttime + timedelta(hours = hstart)
    if cfg.meteo_prefix == 'lffd':
        # nested runs use cosmoart-output as meteo data
        # have to copy the *c.nc-file
        src_file = os.path.join(cfg.meteo_dir,
                                starttime_real.strftime(source_nameformat + 'c.nc'))

        tools.copy_file(src_file, dest_path)

        logging.info("Copied constant-param file from {} to {}"
                     .format(src_file, dest_path))

        # extend nameformat with ending to match cosmo-output
        source_nameformat += '.nc'

    if cfg.meteo_prefix == 'efsf':
        source_nameformat = cfg.meteo_prefix + '%y%m%d%H'

    num_steps = 0
    meteo_dir = cfg.meteo_dir
    subdir = os.path.join(meteo_dir, starttime_real.strftime('%y%m%d%H'))
    for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
        dest_path = os.path.join(cfg.int2lm_input, 'meteo')
        src_file = os.path.join(meteo_dir, time.strftime(source_nameformat))

        if cfg.meteo_prefix == 'efsf':
            if time == starttime_real:
                src_file = os.path.join(subdir, 'eas' + time.strftime('%Y%m%d%H'))
                if not os.path.isfile(src_file) and hasattr(cfg, 'meteo_dir_alt'):
                    meteo_dir = cfg.meteo_dir_alt
                    subdir = os.path.join(meteo_dir,
                             starttime_real.strftime('%y%m%d%H'))
                    src_file = os.path.join(subdir, 'eas' + time.strftime('%Y%m%d%H'))
                dest_path = os.path.join(cfg.int2lm_input, 'meteo',
                                         cfg.meteo_prefix + '00000000')
            else:
                td = time - starttime_real - timedelta(hours=6*num_steps)
                days = str(td.days).zfill(2)
                hours = str(td.seconds//3600).zfill(2)
                td_total = time - starttime_real 
                days_total = str(td_total.days).zfill(2)
                hours_total = str(td_total.seconds//3600).zfill(2)

                src_file = os.path.join(subdir,
                                        cfg.meteo_prefix + days + hours + '0000')
                dest_path = os.path.join(cfg.int2lm_input, 'meteo',
                                         cfg.meteo_prefix +
                                         days_total + hours_total + '0000')

                # Next time, change directory
                checkdir = os.path.join(meteo_dir, time.strftime('%y%m%d%H'))
                if os.path.isdir(checkdir):
                    num_steps += 1
                    subdir = checkdir
                elif hasattr(cfg, 'meteo_dir_alt'):
                    checkdir = os.path.join(cfg.meteo_dir_alt,
                               time.strftime('%y%m%d%H'))
                    if os.path.isdir(checkdir):
                        num_steps += 1
                        subdir = checkdir
                        meteo_dir = cfg.meteo_dir_alt
                        logging.info("Switching to other input directory from {} to {}"
                                     .format(cfg.meteo_dir, cfg.meteo_dir_alt))
        elif not os.path.exists(src_file):
            # special case for MeteoSwiss COSMO-7 data
            archive = '/store/mch/msopr/owm/COSMO-7'
            yy = time.strftime("%y")
            path = '/'.join([archive, 'ANA' + yy])
            src_file = os.path.join(path, time.strftime(source_nameformat))  

        # copy meteo file from project folder to
        tools.copy_file(src_file, dest_path)

        logging.info("Copied file from {} to {}".format(src_file, dest_path))

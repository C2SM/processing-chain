#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Create anthropogenic emissions and put them into the input folder
#
# Result in case of success: all emission input-files necessary are found in 
#                            ${int2lm_input}/emissions/
#
# Dominik Brunner, July 2013
#
# 2013-07-18 Initial release, based on Christoph Knotes' emissions.bash
#            In the current version, the program only checks for the presence
#            of the constant-in-time emissions file and creates a soft link in
#            the int2lm input directory (brd)
# 2018-06-25 Translated to Python (arp)

### DEVELOPMENT VERSION ###

import os
import logging
import shutil

from . import tools

def main(starttime, hstart, hstop, cfg):
    """Copy emission files to the **int2lm** input directory.

    Necessary for both **COSMO** and **COSMOART** simulations.

    Copy emission files from project folder (``cfg.emissions_dir``) to
    **int2lm** input folder on scratch (``cfg.int2lm_input/emissions``).

    For **COSMO** simulations, converts the the netCDF-variable-names 
    from ``string`` to ``char`` (necessary for **int2lm**).

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
    target_dir = os.path.join(cfg.int2lm_input, "emissions")
    tools.create_dir(target_dir, "emissions input")

    dataset_nmbr = 0
    while True:
        # there may be multiple emission datasets, loop until no more
        # are found in cfg
        dataset_nmbr += 1
        if dataset_nmbr == 1:
            emis_dir = cfg.emissions_dir
            emis_prefix = cfg.emis_gridname
        else:
            try:
                emis_dir = getattr(cfg, "emissions_dir" + str(dataset_nmbr))
                emis_prefix = getattr(cfg, "emis_gridname" + str(dataset_nmbr))
            except AttributeError:
                # no new dataset is found, we're done
                break

        for time in tools.iter_hours(starttime, hstart, hstop):
            logging.info(time)
            filepath = os.path.join(emis_dir,
                                    time.strftime(emis_prefix+'%Y%m%d%H.nc'))
            try:
                shutil.copy(filepath, target_dir)
            except FileNotFoundError:
                logging.error("Emission input file not found at %s, or output directory doesn't exist to copy %s" % (filepath, target_dir))
                raise
            except (PermissionError, OSError):
                logging.error("Copying emission data file failed")
                raise

            # convert grid_mapping_name from string (NF90_STRING) to char
            # (NF90_CHAR) (needed for int2lm to work)
            if cfg.target.lower() == 'cosmo':
                tools.string2char.main(filepath)

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

    If there are multiple emission-datasets (cfg.emissions_dir is a list of
    paths), they are copied as follows::

        cfg.emissions_dir[0]/cfg.emis_gridname[0]YYYYMMDD.nc -> int2lm_input/emissions/emis_YYYYMMDD.nc
        cfg.emissions_dir[1]/cfg.emis_gridname[1]YYYYMMDD.nc -> int2lm_input/emissions2/emis_YYYYMMDD.nc
        cfg.emissions_dir[2]/cfg.emis_gridname[2]YYYYMMDD.nc -> int2lm_input/emissions3/emis_YYYYMMDD.nc

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
    dest_prefix = "emis_"

    if not isinstance(cfg.emissions_dir, list):
        emis_dirs = [cfg.emissions_dir]
    else:
        emis_dirs = cfg.emissions_dir
    if not isinstance(cfg.emis_gridname, list):
        emis_prefixes = [cfg.emis_gridname]
    else:
        emis_prefixes = cfg.emis_gridname

    assert len(emis_dirs) == len(emis_prefixes), (
        "Different number of cfg.emissions_dir and cfg.emis_gridname")

    for i, (emis_dir, emis_prefix) in enumerate(zip(emis_dirs, emis_prefixes)):
        # create directory
        if i == 0:
            target_dir = os.path.join(cfg.int2lm_input,
                                      "emissions")
            tools.create_dir(target_dir, "emissions input")
        else:
            target_dir = os.path.join(cfg.int2lm_input,
                                      "emissions" + str(i+1))
            tools.create_dir(target_dir, "emissions input")
        # copy data
        for time in tools.iter_hours(starttime, hstart, hstop):
            logging.info(time)
            filename_ending = time.strftime('%Y%m%d%H.nc')
            source_path = os.path.join(emis_dir, emis_prefix + filename_ending)
            dest_path = os.path.join(target_dir, dest_prefix + filename_ending)
            tools.copy_file(source_path, dest_path)

            # convert grid_mapping_name from string (NF90_STRING) to char
            # (NF90_CHAR) (needed for int2lm to work)
            if cfg.target is tools.Target.COSMO:
                tools.string2char.main(dest_path)

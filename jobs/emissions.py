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

def main(starttime,hstart,hstop,cgf):
    """
    Copy emission files from project folder (cfg.emissions_dir) to INT2LM input folder
    on scratch (cfg.int2lm_input/emissions)
    """

    try:
        os.makedirs(emissions_dir, exists_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating emissions input dir failed")
        raise

    for time in tools.iter_hours(starttime, hstart, hstop):
        logging.info(time)
        filename = os.path.join(cfg.emissions_dir, time.strftime('CO2_CO_Carbosense-CTDAS_%Y%m%d%H.nc'))
        scratch_path = os.path.join(cfg.int2lm_input, time.strftime('emissions/emis_%Y%m%d%H'))
        if not os.path.exits(filename):
            pass
        try:
            shutil.copy(filename, scratch_path)

        except FileNotFoundError:
            logging.error("Emission input file not found at %s" % filename)
            raise
        except (PermissionError, OSError):
            logging.error("Copying emission data file failed")
            raise

        # convert grid_mapping_name from string (NF90_STRING) to char (NF90_CHAR)
        tools.string2char(scratch_path)

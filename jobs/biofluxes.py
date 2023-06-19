#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Create VPRM biogenic fluxes from VPRM and put them into the input folder
#
# result in case of success: all VPRM input-files necessary are found in
#                            ${int2lm_input}/vprm/
#
# Dominik Brunner, July 2013
#
# 2013-07-18 Initial release, based on Christoph Knotes' emissions.bash
#            In the current version, the program only checks for the presence
#            of the constant-in-time emissions file and creates a soft link in the int2lm
#            input directory (brd)
# 2018-06-25 Translated to Python (arp)

import os
import logging
import shutil
from . import tools


def main(starttime, hstart, hstop, cfg):
    """Prepare the biofluxes-files for the simulation.

    Only necessary for **COSMO** simulations.

    Copy biofluxes files from project folder (``cfg.vprm_dir``) to int2lm input
    folder on scratch (``cfg.int2lm_input/vprm``).

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
    tools.check_model(cfg, 'cosmo-ghg')

    scratch_path = os.path.join(cfg.int2lm_input, 'vprm')

    tools.create_dir(scratch_path, "biofluxes input")

    for time in tools.iter_hours(starttime, hstart, hstop):
        logging.info(time)

        for prefix in cfg.vprm_prefix:
            filename = os.path.join(cfg.vprm_dir,
                                    prefix + time.strftime('%Y%m%d%H.nc'))
            filename_sc = os.path.join(scratch_path,
                                       prefix + time.strftime('%Y%m%d%H.nc'))
            if not (os.path.isfile(filename)):
                logging.error(
                    "File %s not found. Consider using the vprmsplit.py script prior",
                    filename)
                #tools.vprmsplit.main(time.strftime("%Y%m%d%H"),cfg.vprm_dir_orig,cfg.vprm_dir_proc,cfg)

            tools.copy_file(filename, scratch_path)

            if not os.path.isfile(filename_sc):
                loggig.error(
                    "Splitting or copying of GPP or/and RA files to scratch failed."
                )

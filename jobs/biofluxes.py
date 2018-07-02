#!/usr/bin/env python
# coding: utf-8
#
# Create VPRM biogenic fluxes from VPRM and put them into the input folder
#
# result in case of success: all VPRM input-files necessary are found in 
#                            ${int2lm_input}/vprm/
#
# Dominik Brunner, July 2013
#
# 18.07.2013 Initial release, based on Christoph Knotes' emissions.bash
#            In the current version, the program only checks for the presence
#            of the constant-in-time emissions file and creates a soft link in the int2lm
#            input directory
# 25.06.2018 translated into python by arp

import os
import logging
import shutil
from . import tools

def main(starttime, hstart, hstop, cfg):
    """
    Copy biofluxes files from project folder (cfg.vprm_dir) to INT2LM input folder
    on scratch (cfg.int2lm_input/vprm)
    """

    try:
        os.makedirs(vprm_dir, exists_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating biofluxes input dir failed")
        raise

    scratch_path = os.path.join(cfg.int2lm_input,'vprm')

    for time in tools.iter_hours(starttime, hstart, hstop):
        logging.info(time)
        file_gpp = os.path.join(cfg.vprm_dir, time.strftime('gpp_%Y%m%d%H.nc'))
        file_ra = os.path.join(cfg.vprm_dir, time.strftime('ra_%Y%m%d%H.nc'))
        file_gpp_sc = os.path.join(scratch_path, time.strftime('gpp_%Y%m%d%H.nc'))
        file_ra_sc = os.path.join(scratch_path, time.strftime('ra_%Y%m%d%H.nc'))
        if ((os.path.isfile(file_gpp)and(os.path.isfile(file_ra)):
            shutil.copy(file_gpp, scratch_path)
            shutil.copy(file_ra, scratch_path)
        else:
            tools.vprmsplit(time,cfg.vprm_dir,scratch_path)
        if not((os.path.isfile(file_gpp_sc))) or (not(os.path.isfile(file_ra_sc))):
            loggig.error("Splitting or copying of GPP or/and RA files to scratch failed.")

#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Prepare initial and boundary conditions
#
# In case of ICON:
# Prepare input for meteorological initial and boundary conditions
# by remapping the files onto the ICON grid (for IC) and the
# auxillary lateral-boundary grid (for BC) with the DWD ICON tools
# and saving them in the input folder.
# Currently, the input files are assumed to be ifs data.
# The files are read-in in grib2-format and the the remapped
# files are saved in netCDF-format (currently only netCDF works
# for ICON when then the simulation is driven by ifs-data).
#
# result in case of success: all meteo input-files necessary are found in
#                            ${int2lm_input}/meteo/
#
# Dominik Brunner, July 2013
#
# 2013-07-16 Initial release, based on Christoph Knote script
# 2017-01-15 Modified for hypatia and project SmartCarb
# 2018-06-21 Translated to Python (kug)
# 2021-02-28 Modified for ICON-simulations (stem)
# 2021-11-12 Modified for ICON-ART-simulations (mjaehn)

import os
import logging
import shutil
import subprocess
from datetime import timedelta
import xarray
from . import tools


def main(starttime, hstart, hstop, cfg):
    """
    **ICON** (if ``cfg.target`` is ``tools.Target.ICON``)

     Create necessary directories ``cfg.icon_input_icbc``
     and ''cfg.icon_work''

     Submitting the runscript for the DWD ICON tools to remap the meteo files.

     All runscripts specified in ``cfg.icontools_runjobs`` are submitted.

     The meteo files are read-in from the original input directory 
     (``cfg.input_root_meteo``) and the remapped meteo files are
     saved in the input folder on scratch (``cfg.icon_input/icbc``).

     The constant variable 'GEOSP' is added to the files not containing it
     using python-cdo bindings.

    **COSMO**

     Copy meteo files to **int2lm** input.

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

    #-----------------------------------------------------
    # Create directories
    #-----------------------------------------------------
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
    tools.create_dir(cfg.icon_input_grid, "icon_input_grid")
    tools.create_dir(cfg.icon_input_rad, "icon_input_rad")
    tools.create_dir(cfg.icon_input_xml, "icon_input_xml")
    tools.create_dir(cfg.icon_output, "icon_output")
    tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

    #-----------------------------------------------------
    # Copy files
    #-----------------------------------------------------
    # Copy grid files
    tools.copy_file(cfg.DYNAMICS_GRID_FILENAME,
                    cfg.dynamics_grid_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.RADIATION_GRID_FILENAME,
                    cfg.radiation_grid_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.EXTPAR_FILENAME,
                    cfg.extpar_filename_scratch,
                    output_log=True)

    # Copy radiation files
    tools.copy_file(cfg.CLDOPT_FILENAME,
                    cfg.cldopt_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.LRTM_FILENAME,
                    cfg.lrtm_filename_scratch,
                    output_log=True)

    # Copy icbc files
    tools.copy_file(cfg.INICOND_FILENAME,
                    cfg.inicond_filename_scratch,
                    output_log=True)

    # Copy XML files
    if hasattr(cfg, 'CHEMTRACER_XML_FILENAME'):
        tools.copy_file(cfg.CHEMTRACER_XML_FILENAME,
                        cfg.chemtracer_xml_filename_scratch,
                        output_log=True)

    if hasattr(cfg, 'PNTSRC_XML_FILENAME'):
        tools.copy_file(cfg.PNTSRC_XML_FILENAME,
                        cfg.pntSrc_xml_filename_scratch,
                        output_log=True)

    # -- Restart file
    if cfg.lrestart == '.TRUE.':
        print('TEST')
        print(cfg.restart_filename_scratch)
        print(os.path.join(cfg.icon_work, 'restart_atm_DOM01.nc'))
        os.symlink(cfg.restart_filename_scratch, os.path.join(cfg.icon_work, 'restart_atm_DOM01.nc'))

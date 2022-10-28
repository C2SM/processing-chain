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
import glob
import logging
import shutil
import subprocess
from datetime import timedelta
import xarray as xr

from . import tools
from .tools.interpolate_data import create_oh_for_restart, create_oh_for_inicond
from calendar import monthrange


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

    time = starttime + timedelta(hours=hstart)
    year = time.year
    month = time.month 
    day = time.day

    # -----------------------------------------------------
    # Create directories
    # -----------------------------------------------------
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
    tools.create_dir(cfg.icon_input_grid, "icon_input_grid")
    tools.create_dir(cfg.icon_input_rad, "icon_input_rad")
    tools.create_dir(cfg.icon_input_xml, "icon_input_xml")
    tools.create_dir(cfg.icon_input_oem, "icon_input_oem")
    tools.create_dir(cfg.icon_input_chemistry, "icon_input_chemistry")
    tools.create_dir(cfg.icon_input_art, "icon_input_art")
    tools.create_dir(cfg.icon_output, "icon_output")
    tools.create_dir(cfg.icon_restart_out, "icon_restart_out")

    # -----------------------------------------------------
    # Copy files
    # -----------------------------------------------------
    # Copy grid files
    tools.copy_file(cfg.DYNAMICS_GRID_FILENAME,
                    cfg.dynamics_grid_filename_scratch,
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
    if not cfg.ERA5_INICOND:
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

    # -- Copy nudging data
    if cfg.ERA5_GLOBAL_NUDGING:
        tools.copy_file(cfg.MAP_FILE_NUDGING,
                        cfg.map_file_nudging_scratch,
                        output_log=True)

    # -- Copy ART files
    if hasattr(cfg, 'INPUT_ROOT_ART'):
        list_files = glob.glob(os.path.join(cfg.INPUT_ROOT_ART, '*'))
        for file in list_files:
            tools.copy_file(file, cfg.icon_work)

    # -- Set year-specific attributes and copy OEM files

    setattr(cfg, 'oem_emis_filename_scratch',
            os.path.join(cfg.icon_input_oem,
                         os.path.basename(cfg.OEM_EMIS_FILENAME.format(year=year))))
    setattr(cfg, 'oem_vertprof_filename_scratch',
            os.path.join(cfg.icon_input_oem,
                         os.path.basename(cfg.OEM_VERTPROF_FILENAME)))
    setattr(cfg, 'oem_hourofday_filename_scratch',
            os.path.join(cfg.icon_input_oem,
                         os.path.basename(cfg.OEM_HOUROFDAY_FILENAME)))
    setattr(cfg, 'oem_dayofweek_filename_scratch',
            os.path.join(cfg.icon_input_oem,
                         os.path.basename(cfg.OEM_DAYOFWEEK_FILENAME)))
    setattr(cfg, 'oem_monthofyear_filename_scratch',
            os.path.join(cfg.icon_input_oem,
                         os.path.basename(cfg.OEM_MONTHOFYEAR_FILENAME)))

    tools.copy_file(cfg.OEM_EMIS_FILENAME.format(year=year),
                    cfg.oem_emis_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.OEM_VERTPROF_FILENAME,
                    cfg.oem_vertprof_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.OEM_HOUROFDAY_FILENAME,
                    cfg.oem_hourofday_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.OEM_DAYOFWEEK_FILENAME,
                    cfg.oem_dayofweek_filename_scratch,
                    output_log=True)
    tools.copy_file(cfg.OEM_MONTHOFYEAR_FILENAME,
                    cfg.oem_monthofyear_filename_scratch,
                    output_log=True)

    # -- Set OH attributes and copy file
    # setattr(cfg, 'oh_vmr_filename_scratch',
    #         os.path.join(cfg.icon_input_chemistry,
    #                      os.path.basename(cfg.OH_VMR_FILENAME)))

    # tools.copy_file(cfg.OH_VMR_FILENAME,
    #                 cfg.oh_vmr_filename_scratch,
    #                 output_log=True)


    # -----------------------------------------------------
    # Create meteorological initial conditions
    # -----------------------------------------------------

    # -- If not, download ERA5 data and create the inicond file
    if cfg.ERA5_INICOND:
        # -- Fetch ERA5 data
        tools.fetch_era5(starttime + timedelta(hours=hstart), cfg.icon_input_icbc)

        # -- Copy ERA5 processing script (icon_era5_inicond.job) in workdir
        with open(cfg.ICON_ERA5_INIJOB) as input_file:
            to_write = input_file.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_inicond.sh')
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))

        # -- Copy mypartab in workdir
        shutil.copy(os.path.join(os.path.dirname(cfg.ICON_ERA5_INIJOB), 'mypartab'), os.path.join(cfg.icon_input_icbc, 'mypartab'))

        # -- Run ERA5 processing script
        process = subprocess.Popen(["bash", os.path.join(cfg.icon_input_icbc, 'icon_era5_inicond.sh')], stdout=subprocess.PIPE)
        process.communicate()

    # -----------------------------------------------------
    # Create tracer initial conditions
    # -----------------------------------------------------

    # -- Download and add CAMS data to the inicond file if needed
    if cfg.SPECIES_INICOND:

        if cfg.lrestart == '.FALSE.':

            ext_restart = ''
            filename = cfg.inicond_filename_scratch

            # -- Copy the script for processing external tracer data in workdir
            with open(cfg.ICON_SPECIES_INIJOB) as input_file:
                to_write = input_file.read()
            output_file = os.path.join(cfg.icon_input_icbc, 'icon_species_inicond.sh')
            with open(output_file, "w") as outf:
                time = starttime + timedelta(hours=hstart)
                outf.write(to_write.format(cfg=cfg, filename=filename, ext_restart=ext_restart, year=year, month=month, day=day))

            # -- Run ERA5 processing script
            process = subprocess.Popen(["bash", os.path.join(cfg.icon_input_icbc, 'icon_species_inicond.sh')], stdout=subprocess.PIPE)
            process.communicate()

            # -- Create initial conditions for OH concentrations
            if 'TROH' in cfg.SPECIES2RESTART:
                create_oh_for_inicond(cfg, month)

        else:

            # -- Check the extension of tracer variables in the restart file 
            ds_restart = xr.open_dataset(cfg.restart_filename_scratch)
            tracer_name = cfg.SPECIES2RESTART[0]
            var_restart  = [var for var in ds_restart.data_vars.keys() if var.startswith(tracer_name)][0]
            ext_restart = var_restart.replace(tracer_name, '')
            filename = cfg.restart_filename_scratch

            # -- Change OH concentrations in the restart file
            if 'TROH' in cfg.SPECIES2RESTART:
                create_oh_for_restart(cfg, month, ext_restart)


    # -----------------------------------------------------
    # Create meteorological and tracer nudging conditions
    # -----------------------------------------------------

    # -- If global nudging, download and process ERA5 and CAMS data
    if cfg.ERA5_GLOBAL_NUDGING:

        for time in tools.iter_hours(starttime, hstart, hstop, step=cfg.NUDGING_STEP):

            # -- Give a name to the nudging file
            timestr = time.strftime('%Y%m%d%H')
            filename = 'era2icon_R2B03_{timestr}_nudging.nc'.format(timestr=timestr)

            # -- If initial time, copy the initial conditions to be used as boundary conditions
            if time == starttime and cfg.ERA5_INICOND:
                shutil.copy(cfg.inicond_filename_scratch, os.path.join(cfg.icon_input_icbc, filename))
                continue

            # -- Fetch ERA5 data
            tools.fetch_era5_nudging(time, cfg.icon_input_icbc)

            # -- Copy ERA5 processing script (icon_era5_nudging.job) in workdir
            with open(cfg.ICON_ERA5_NUDGINGJOB) as input_file:
                to_write = input_file.read()
            output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_nudging_{}.sh'.format(timestr))
            with open(output_file, "w") as outf:
                outf.write(to_write.format(cfg=cfg, filename=filename))

            # -- Copy mypartab in workdir
            if not os.path.exists(os.path.join(cfg.icon_input_icbc, 'mypartab')):
                shutil.copy(os.path.join(os.path.dirname(cfg.ICON_ERA5_NUDGINGJOB), 'mypartab'), os.path.join(cfg.icon_input_icbc, 'mypartab'))

            # -- Run ERA5 processing script
            process = subprocess.Popen(["bash", os.path.join(cfg.icon_input_icbc, 'icon_era5_nudging_{}.sh'.format(timestr))], stdout=subprocess.PIPE)
            process.communicate()

            if cfg.SPECIES_GLOBAL_NUDGING:

                # -- Copy CAMS processing script (icon_cams_nudging.job) in workdir
                with open(cfg.ICON_SPECIES_NUDGINGJOB) as input_file:
                    to_write = input_file.read()
                output_file = os.path.join(cfg.icon_input_icbc, 'icon_cams_nudging_{}.sh'.format(timestr))
                with open(output_file, "w") as outf:
                    outf.write(to_write.format(cfg=cfg, filename=filename))

                # -- Run ERA5 processing script
                process = subprocess.Popen(["bash", os.path.join(cfg.icon_input_icbc, 'icon_cams_nudging_{}.sh'.format(timestr))], stdout=subprocess.PIPE)
                process.communicate()

    # -----------------------------------------------------
    # Create symlink to the restart file if lrestart is True
    # -----------------------------------------------------
    if cfg.lrestart == '.TRUE.':
        os.symlink(cfg.restart_filename_scratch, os.path.join(cfg.icon_work, 'restart_atm_DOM01.nc'))


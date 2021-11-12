#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Prepare input for meteorological initial and boundary conditions
# by copying files from OSM archive to INT2LM input folder
# Currently, the input files are assumed to be COSMO-7 or
# COSMO pre-2007 analysis files.
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

import os
import logging
import shutil
import subprocess
from datetime import timedelta
from cdo import Cdo
from . import tools

cdo = Cdo()


def main(starttime, hstart, hstop, cfg):
    """
    **ICON** (if ``cfg.target`` is ``tools.Target.ICON``)

     Create necessary directories ``cfg.icon_input_icbc``,
     ''cfg.icon_input_grid'' and ''cfg.icon_work''

     Submitting the runscript for the DWD ICON tools to remap the meteo files.

     There are 4 runscripts submitted:
       - 1x for the creation of an auxillary grid of the lateral boundary
       - 2x for the remapping of the lateral boundary conditions:
         - Once for the analysis fields at time 00 (containing GEOSP)
         - Once for all other time steps (no GEOSP)
       - 1x for the remapping of the initial conditions

     The meteo files are read-in from project directory 
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

    if cfg.target is tools.Target.ICON or cfg.target is tools.Target.ICONART or \
       cfg.target is tools.Target.ICONOEM:

        logging.info('ICON analysis data for IC/BC')

        starttime_real = starttime + timedelta(hours=hstart)

        #-----------------------------------------------------
        # Create directories
        #-----------------------------------------------------
        tools.create_dir(cfg.icon_work, "icon_work")
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        tools.create_dir(cfg.icon_input_grid, "icon_input_grid")

        #-----------------------------------------------------
        # Remap initial conditions
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "ic")
        logfile_finish = os.path.join(cfg.log_finished_dir, "ic")

        # Write remap_ic namelist
        in_filename = os.path.join(
            cfg.input_root_meteo,
            starttime.strftime(cfg.source_nameformat) + '.grb')
        out_filename = os.path.join(cfg.icon_input_icbc, cfg.inidata_filename)
        in_grid_filename = in_filename
        out_grid_filename = os.path.join(cfg.input_root_grid,
                                         cfg.dynamics_grid_filename)
        with open(
                os.path.join(
                    cfg.case_dir,
                    cfg.icontools_parameter['icontools_namelist_remap'])
        ) as input_file:
            to_write = input_file.read()
        output_nml = os.path.join(cfg.icon_work, 'icontools_remap_ic.namelist')
        with open(output_nml, "w") as outf:
            to_write = to_write.format(cfg=cfg,
                                       in_filename=in_filename,
                                       out_filename=out_filename,
                                       in_grid_filename=in_grid_filename,
                                       out_grid_filename=out_grid_filename)
            outf.write(to_write)

        # Write remapfields namelist
        with open(
                os.path.join(
                    cfg.case_dir, cfg.icontools_parameter[
                        'icontools_namelist_remapfields_ic'])) as input_file:
            to_write = input_file.read()
        output_fields = os.path.join(cfg.icon_work,
                                     'icontools_remapfields_ic.namelist')
        with open(output_fields, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        # Write run script (remap_ic.job)
        with open(
                os.path.join(
                    cfg.case_dir,
                    cfg.icontools_parameter['icontools_remap_ic_runjob'])
        ) as input_file:
            to_write = input_file.read()
        output_run = os.path.join(cfg.icon_work, "remap_ic.job")
        with open(output_run, "w") as outf:
            outf.write(
                to_write.format(cfg=cfg,
                                logfile=logfile,
                                logfile_finish=logfile_finish))
        exitcode = subprocess.call(
            ["sbatch", "--wait",
             os.path.join(cfg.icon_work, 'remap_ic.job')])
        if exitcode != 0:
            raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
        logging.info("Remapped initial conditions with icontools")

        os.remove(output_nml)
        os.remove(output_fields)
        os.remove(output_run)

        #-----------------------------------------------------
        # Create auxillary boundary grid
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "auxgrid")
        logfile_finish = os.path.join(cfg.log_finished_dir, "auxgrid")

        # Write iconsub namelist
        with open(
                os.path.join(
                    cfg.case_dir,
                    cfg.icontools_parameter['icontools_namelist_iconsub'])
        ) as input_file:
            to_write = input_file.read()
        output_nml = os.path.join(cfg.icon_work, 'icontools_iconsub.namelist')
        with open(output_nml, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        # Write run script (auxgrid.job)
        with open(
                os.path.join(
                    cfg.case_dir,
                    cfg.icontools_parameter['icontools_auxgrid_runjob'])
        ) as input_file:
            to_write = input_file.read()
        output_run = os.path.join(cfg.icon_work, "auxgrid.job")
        with open(output_run, "w") as outf:
            outf.write(
                to_write.format(cfg=cfg,
                                logfile=logfile,
                                logfile_finish=logfile_finish))
        exitcode = subprocess.call(
            ["sbatch", "--wait",
             os.path.join(cfg.icon_work, 'auxgrid.job')])
        if exitcode != 0:
            raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
        logging.info("Created auxillary boundary grid with icontools")

        os.remove(output_nml)
        os.remove(output_run)

        #-----------------------------------------------------
        # Remap analysis LBC (at hours 00)
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "lbc_ana")
        logfile_finish = os.path.join(cfg.log_finished_dir, "lbc_ana")

        with open(
                os.path.join(
                    cfg.case_dir, cfg.icontools_parameter[
                        'icontools_namelist_remapfields_ana_lbc'])
        ) as input_file:
            to_write = input_file.read()
        output_nml_fields = os.path.join(
            cfg.icon_work, 'icontools_remapfields_lbc_00.namelist')
        with open(output_nml_fields, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):

            if (time.hour != 0):
                continue

            # Write remap_lbc namelist
            in_grid_filename = os.path.join(
                cfg.input_root_meteo,
                starttime.strftime(cfg.source_nameformat) + '.grb')
            in_filename = os.path.join(
                cfg.input_root_meteo,
                time.strftime(cfg.source_nameformat) + '.grb')
            out_grid_filename = os.path.join(cfg.icon_input_grid,
                                             cfg.lateral_boundary_grid)
            out_filename = os.path.join(cfg.icon_input_icbc,
                                        time.strftime(cfg.source_nameformat))
            with open(
                    os.path.join(
                        cfg.case_dir,
                        cfg.icontools_parameter['icontools_namelist_remap'])
            ) as input_file:
                to_write = input_file.read()
            output_nml_lbc = os.path.join(cfg.icon_work,
                                          'icontools_remap_lbc.namelist')
            with open(output_nml_lbc, "w") as outf:
                to_write = to_write.format(cfg=cfg,
                                           in_grid_filename=in_grid_filename,
                                           in_filename=in_filename,
                                           out_grid_filename=out_grid_filename,
                                           out_filename=out_filename)
                outf.write(to_write)

            # Write run script (remap_lbc_00.job)
            with open(
                    os.path.join(
                        cfg.case_dir, cfg.icontools_parameter[
                            'icontools_remap_ana_lbc_runjob'])) as input_file:
                to_write = input_file.read()
            output_run = os.path.join(cfg.icon_work, "remap_lbc_00.job")
            with open(output_run, "w") as outf:
                outf.write(
                    to_write.format(cfg=cfg,
                                    logfile=logfile,
                                    logfile_finish=logfile_finish))
            exitcode = subprocess.call([
                "sbatch", "--wait",
                os.path.join(cfg.icon_work, 'remap_lbc_00.job')
            ])
            if exitcode != 0:
                raise RuntimeError(
                    "sbatch returned exitcode {}".format(exitcode))
            logging.info(
                "Remapped boundary conditions at {} with icontools".format(
                    time))

            os.remove(output_nml_lbc)
            os.remove(output_run)

        os.remove(output_nml_fields)

        #-----------------------------------------------------
        # Remap fc LBC (at hours other than 00)
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "lbc_fc")
        logfile_finish = os.path.join(cfg.log_finished_dir, "lbc_fc")
        # Write remapfields namelist
        with open(
                os.path.join(
                    cfg.case_dir, cfg.icontools_parameter[
                        'icontools_namelist_remapfields_fc_lbc'])
        ) as input_file:
            to_write = input_file.read()
        output_nml_fields = os.path.join(cfg.icon_work,
                                         'icontools_remapfields_lbc.namelist')
        with open(output_nml_fields, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):

            if (time.hour == 0):
                continue

            # Write remap_lbc namelist
            in_grid_filename = os.path.join(
                cfg.input_root_meteo,
                starttime.strftime(cfg.source_nameformat) + '.grb')
            in_filename = os.path.join(
                cfg.input_root_meteo,
                time.strftime(cfg.source_nameformat) + '.grb')
            out_grid_filename = os.path.join(cfg.icon_input_grid,
                                             cfg.lateral_boundary_grid)
            out_filename = os.path.join(cfg.icon_input_icbc,
                                        time.strftime(cfg.source_nameformat))
            with open(
                    os.path.join(
                        cfg.case_dir,
                        cfg.icontools_parameter['icontools_namelist_remap'])
            ) as input_file:
                to_write = input_file.read()
            output_nml_lbc = os.path.join(cfg.icon_work,
                                          'icontools_remap_lbc.namelist')
            with open(output_nml_lbc, "w") as outf:
                to_write = to_write.format(cfg=cfg,
                                           in_grid_filename=in_grid_filename,
                                           in_filename=in_filename,
                                           out_grid_filename=out_grid_filename,
                                           out_filename=out_filename)
                outf.write(to_write)

            # Write run script (remap_lbc.job)
            with open(
                    os.path.join(
                        cfg.case_dir, cfg.icontools_parameter[
                            'icontools_remap_fc_lbc_runjob'])) as input_file:
                to_write = input_file.read()
            output_run = os.path.join(cfg.icon_work, "remap_lbc.job")
            with open(output_run, "w") as outf:
                outf.write(
                    to_write.format(cfg=cfg,
                                    logfile=logfile,
                                    logfile_finish=logfile_finish))
            exitcode = subprocess.call([
                "sbatch", "--wait",
                os.path.join(cfg.icon_work, 'remap_lbc.job')
            ])
            if exitcode != 0:
                raise RuntimeError(
                    "sbatch returned exitcode {}".format(exitcode))
            logging.info(
                "Remapped boundary conditions at {} with icontools".format(
                    time))

            os.remove(output_nml_lbc)
            os.remove(output_run)

        os.remove(output_nml_fields)

        #-----------------------------------------------------
        # Add GEOSP to all meteo files using cdo
        #-----------------------------------------------------
        # First, extract GEOSP from first file
        src_file = os.path.join(cfg.icon_input_icbc,
                                starttime.strftime(cfg.source_nameformat))
        GEOSP_file = os.path.join(cfg.icon_input_icbc, 'GEOSP.nc')
        # Select variable with CDO
        cdo.selvar("GEOSP", input=src_file, output=GEOSP_file)
        # Add GEOSP to all other files
        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
            meteo_file = os.path.join(cfg.icon_input_icbc,
                                      time.strftime(cfg.source_nameformat))
            merged_file = os.path.join(
                cfg.icon_input_icbc,
                time.strftime(cfg.source_nameformat) + '_lbc.nc')
            # Merge with CDO
            if (time.hour != 0):
                cdo.merge(input=meteo_file + ' ' + GEOSP_file,
                          output=merged_file)
                os.remove(meteo_file)
            # Only rename at hour 00:
            if (time.hour == 0):
                os.rename(meteo_file, merged_file)
            # Delete old file
            logging.info("Added GEOSP to file {}".format(merged_file))
        # Delete GEOSP_file.nc
        os.remove(GEOSP_file)

    # If COSMO (and not ICON):
    else:
        logging.info('COSMO analysis data for IC/BC')

        dest_path = os.path.join(cfg.int2lm_input, 'meteo')
        tools.create_dir(dest_path, "meteo input")

        source_nameformat = cfg.meteo_prefix + '%Y%m%d%H'
        starttime_real = starttime + timedelta(hours=hstart)
        if cfg.meteo_prefix == 'lffd':
            # nested runs use cosmoart-output as meteo data
            # have to copy the *c.nc-file
            src_file = os.path.join(
                cfg.meteo_dir,
                starttime_real.strftime(source_nameformat + 'c.nc'))

            tools.copy_file(src_file, dest_path)

            logging.info("Copied constant-param file from {} to {}".format(
                src_file, dest_path))

            # extend nameformat with ending to match cosmo-output
            source_nameformat += '.nc'

        if cfg.meteo_prefix == 'efsf':
            source_nameformat = cfg.meteo_prefix + '%y%m%d%H'

        num_steps = 0
        meteo_dir = cfg.meteo_dir
        subdir = os.path.join(meteo_dir, starttime_real.strftime('%y%m%d%H'))
        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
            dest_path = os.path.join(cfg.int2lm_input, 'meteo')
            src_file = os.path.join(meteo_dir,
                                    time.strftime(source_nameformat))

            if cfg.meteo_prefix == 'efsf':
                if time == starttime_real:
                    src_file = os.path.join(subdir,
                                            'eas' + time.strftime('%Y%m%d%H'))
                    if not os.path.isfile(src_file) and hasattr(
                            cfg, 'meteo_dir_alt'):
                        meteo_dir = cfg.meteo_dir_alt
                        subdir = os.path.join(
                            meteo_dir, starttime_real.strftime('%y%m%d%H'))
                        src_file = os.path.join(
                            subdir, 'eas' + time.strftime('%Y%m%d%H'))
                    dest_path = os.path.join(cfg.int2lm_input, 'meteo',
                                             cfg.meteo_prefix + '00000000')
                else:
                    td = time - starttime_real - timedelta(hours=6 * num_steps)
                    days = str(td.days).zfill(2)
                    hours = str(td.seconds // 3600).zfill(2)
                    td_total = time - starttime_real
                    days_total = str(td_total.days).zfill(2)
                    hours_total = str(td_total.seconds // 3600).zfill(2)

                    src_file = os.path.join(
                        subdir, cfg.meteo_prefix + days + hours + '0000')
                    dest_path = os.path.join(
                        cfg.int2lm_input, 'meteo',
                        cfg.meteo_prefix + days_total + hours_total + '0000')

                    # Next time, change directory
                    checkdir = os.path.join(meteo_dir,
                                            time.strftime('%y%m%d%H'))
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
                            logging.info(
                                "Switching to other input directory from {} to {}"
                                .format(cfg.meteo_dir, cfg.meteo_dir_alt))
            elif not os.path.exists(src_file):
                # special case for MeteoSwiss COSMO-7 data
                archive = '/store/mch/msopr/owm/COSMO-7'
                yy = time.strftime("%y")
                path = '/'.join([archive, 'ANA' + yy])
                src_file = os.path.join(path, time.strftime(source_nameformat))

            # copy meteo file from project folder to
            tools.copy_file(src_file, dest_path)

            logging.info("Copied file from {} to {}".format(
                src_file, dest_path))

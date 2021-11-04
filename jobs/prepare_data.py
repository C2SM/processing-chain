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
# 2021-09-xx Modified for ICON-ART-simulations (mjaehn)

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

    if cfg.target is tools.Target.ICON or cfg.target is tools.Target.ICONART or \
       cfg.target is tools.Target.ICONARTOEM:

        logging.info('ICON input data (IC/BC)')

        starttime_real = starttime + timedelta(hours = hstart)

        #-----------------------------------------------------
        # Create directories
        #-----------------------------------------------------
        tools.create_dir(cfg.icon_work, "icon_work")
        #tools.create_dir(cfg.icon_input_oae, "icon_input_oae") # TODO_MJ: move to oae job
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        tools.create_dir(cfg.icon_input_grid, "icon_input_grid")
        tools.create_dir(cfg.icon_input_mapping, "icon_input_mapping")
        tools.create_dir(cfg.icon_input_rad, "icon_input_rad")
        tools.create_dir(cfg.icon_output, "icon_output")
        tools.create_dir(cfg.icon_restart_out, "icon_restart_out") 


        #-----------------------------------------------------
        # Get datafile lists for LBC (each at 00 UTC and others)
        #-----------------------------------------------------
        datafile_list = []
        datafile_list_rest = []
        datafile_list_chem = []
        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
            meteo_file = os.path.join(cfg.icon_input_icbc,
                                      time.strftime(cfg.meteo_nameformat))
            if cfg.target is tools.Target.ICONARTOEM:
                chem_file = os.path.join(cfg.icon_input_chem,
                                          time.strftime(cfg.chem_nameformat))
            datafile_list_chem.append(chem_file + cfg.chem_suffix)
            if meteo_file.endswith('00'):
                datafile_list.append(meteo_file + cfg.meteo_suffix)
            else:
                datafile_list_rest.append(meteo_file + cfg.meteo_suffix)
        datafile_list = ' '.join([str(v) for v in datafile_list])
        datafile_list_rest = ' '.join([str(v) for v in datafile_list_rest])

        #-----------------------------------------------------
        # Write and submit runscripts 
        #-----------------------------------------------------
        for runscript in cfg.icontools_runjobs: 
            logfile = os.path.join(cfg.log_working_dir, 'prepare_data')
            logfile_finish = os.path.join(cfg.log_finished_dir, 'prepare_data')
            with open(os.path.join(cfg.case_dir,runscript)) as input_file:
                to_write = input_file.read()
            output_run = os.path.join(cfg.icon_work, "%s.job" % runscript)
            with open(output_run, "w") as outf:
                outf.write(to_write.format(
                    cfg=cfg,
                    logfile=logfile, logfile_finish=logfile_finish,
                    datafile_list=datafile_list,
                    datafile_list_rest=datafile_list_rest,
                    datafile_list_chem=datafile_list_chem
                    )
                )
            exitcode = subprocess.call(["sbatch", "--wait",
                                        os.path.join(cfg.icon_work, "%s.job" % runscript)])
            if exitcode != 0:
                raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
            logging.info("%s successfully executed." % runscript)

        #-----------------------------------------------------
        # Add GEOSP to all meteo files
        #-----------------------------------------------------
        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
            src_file = os.path.join(cfg.icon_input_icbc,
                                       time.strftime(cfg.meteo_nameformat) + '_lbc.nc')
            merged_file = os.path.join(cfg.icon_input_icbc,
                                       time.strftime(cfg.meteo_nameformat) + '_merged.nc')
            ds = xarray.open_dataset(src_file)
            # Load GEOSP-dataset as ds_geosp at time 00:
            if (time.hour == 0):
                da_geosp = ds['GEOSP']
            # Merge GEOSP-dataset with other timesteps
            elif (time.hour != 0):
                # Change values of time dimension to current time
                da_geosp = da_geosp.assign_coords(time=[time])
                # Merge GEOSP into temporary file
                ds_merged = xarray.merge([ds, da_geosp])
                ds_merged.attrs = ds.attrs
                ds_merged.to_netcdf(merged_file)
                # Rename file to get original file name
                tools.rename_file(merged_file, src_file)
                logging.info("Added GEOSP to file {}".format(merged_file))

        #-----------------------------------------------------
        # In cae of OEM: merge chem tracers with meteo-files
        #-----------------------------------------------------
        if cfg.target is tools.Target.ICONARTOEM:
            for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):
                if time==starttime:
                    #------------
                    # Merge IC:
                    #------------
                    meteo_file = os.path.join(cfg.icon_input_icbc,
                                               time.strftime(cfg.chem_nameformat) + '.nc')
                    chem_file = os.path.join(cfg.icon_input_icbc,
                                               time.strftime(cfg.chem_nameformat) + '.nc')
                    merged_file = os.path.join(cfg.icon_input_icbc,
                                               time.strftime(cfg.chem_nameformat) + '_merged.nc')
                    ds_meteo = xarray.open_dataset(meteo_file)
                    ds_chem = xarray.open_dataset(chem_file)
                    # Merge GEOSP into temporary file
                    ds_merged = xarray.merge([ds_meteo, ds_chem])
                    ds_merged.attrs = ds.attrs
                    ds_merged.to_netcdf(merged_file)
                    # Rename file to get original file name
                    tools.rename_file(merged_file, meteo_file)
                    logging.info("Added chemical tracer to file {}".format(merged_file))

                #------------
                # Merge LBC:
                #------------
                meteo_file = os.path.join(cfg.icon_input_icbc,
                                           time.strftime(cfg.chem_nameformat) + '_lbc.nc')
                chem_file = os.path.join(cfg.icon_input_icbc,
                                           time.strftime(cfg.chem_nameformat) + '_lbc.nc')
                merged_file = os.path.join(cfg.icon_input_icbc,
                                           time.strftime(cfg.chem_nameformat) + '_merged.nc')
                ds_meteo = xarray.open_dataset(meteo_file)
                ds_chem = xarray.open_dataset(chem_file)
                # Merge GEOSP into temporary file
                ds_merged = xarray.merge([ds_meteo, ds_chem])
                ds_merged.attrs = ds.attrs
                ds_merged.to_netcdf(merged_file)
                # Rename file to get original file name
                tools.rename_file(merged_file, meteo_file)
                logging.info("Added chemical tracer to file {}".format(merged_file))


        # Copy grid files
        tools.copy_file(cfg.radiation_grid_filename, cfg.radiation_grid_filename_scratch,
                        output_log=True)
        tools.copy_file(cfg.dynamics_grid_filename, cfg.dynamics_grid_filename_scratch,
                        output_log=True)
        tools.copy_file(cfg.map_file_latbc, cfg.map_file_latbc_scratch,
                        output_log=True)
        tools.copy_file(cfg.extpar_filename, cfg.extpar_filename_scratch,
                        output_log=True)
        tools.copy_file(cfg.lateral_boundary_grid, cfg.lateral_boundary_grid_scratch,
                        output_log=True)

        # Copy radiation files
        tools.copy_file(cfg.cldopt_filename, cfg.cldopt_filename_scratch,
                        output_log=True)
        tools.copy_file(cfg.lrtm_filename, cfg.lrtm_filename_scratch,
                        output_log=True)

        # Copy mapping file
        tools.copy_file(cfg.map_file_ana, cfg.map_file_ana_scratch,
                        output_log=True)

        # Copy tracer data in case of ART
        if cfg.target is tools.Target.ICONART  or cfg.target is tools.Target.ICONARTOEM:
            tools.create_dir(cfg.icon_input_xml, "icon_input_xml")
            tools.copy_file(cfg.chemtracer_xml_filename, cfg.chemtracer_xml_filename_scratch,
                            output_log=True)
            tools.copy_file(cfg.pntSrc_xml_filename, cfg.pntSrc_xml_filename_scratch,
                            output_log=True)

    # If COSMO (and not ICON):
    else:
        logging.info('COSMO analysis data for IC/BC')

        dest_path = os.path.join(cfg.int2lm_input, 'meteo')
        tools.create_dir(dest_path, "meteo input")

        source_nameformat = cfg.meteo_nameformat
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

        # Other IC/BC data
        inv_to_process = []
        if cfg.target is tools.Target.COSMOGHG:
            try:
                CAMS = dict(fullname = "CAMS",
                            nickname = "cams",
                            executable = "cams4int2cosmo",
                            indir = cfg.cams_dir_orig,
                            outdir = cfg.cams_dir_proc,
                            param = cfg.cams_parameters)
                inv_to_process.append(CAMS)
            except AttributeError:
                pass
            try:
                CT = dict(fullname = "CarbonTracker",
                          nickname = "ct",
                          executable = "ctnoaa4int2cosmo",
                          indir = cfg.ct_dir_orig,
                          outdir = cfg.ct_dir_proc,
                          param = cfg.ct_parameters)
                inv_to_process.append(CT)
            except AttributeError:
                pass
        elif cfg.target is tools.Target.COSMOART:
            try:
                MOZART = dict(fullname = 'MOZART',
                              nickname = 'mozart',
                              executable = 'mozart2int2lm',
                              indir = cfg.mozart_file_orig,
                              outdir = cfg.mozart_dir_proc,
                              param = [{'inc' : cfg.mozart_inc,
                                        'suffix' : cfg.mozart_prefix}])
                inv_to_process.append(MOZART)
            except AttributeError:
                pass
        
        if cfg.target is tools.Target.COSMOGHG or cfg.target is tools.Target.COSMOART:
            logging.info("Processing " + ", ".join([i["fullname"] for i in inv_to_process])+" data")

            scratch_path = os.path.join(cfg.int2lm_input,'icbc')
            tools.create_dir(scratch_path, "icbc input")

            for inv in inv_to_process:
                logging.info(inv["fullname"]+" files")
                tools.create_dir(inv["outdir"], "processed " + inv["fullname"])
            
                for p in inv["param"]:
                    inc = p["inc"]
                    for time in tools.iter_hours(starttime, hstart, hstop, inc):
                        logging.info(time)

                        filename = os.path.join(inv["outdir"],p["suffix"]+"_"+time.strftime("%Y%m%d%H")+".nc")
                        if not os.path.exists(filename):
                            logging.info(filename)
                            try:
                                to_call = getattr(tools, inv["executable"])
                                to_call.main(time,inv["indir"],inv["outdir"],p)
                            except:
                                logging.error("Preprocessing "+inv["fullname"] + " data failed")
                                raise

                        # copy to (temporary) run input directory
                        tools.copy_file(filename, scratch_path)

                        logging.info("OK")



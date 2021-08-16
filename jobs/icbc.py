#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Check presence of CarbonTracker files, preprocess, and soft link
#
# Result in case of success: all emission input-files necessary are found in 
#                            ${int2lm_input}/icbc/
#
# Dominik Brunner, Jul 2013
#
# 2013-07-17 Initial release, based on Christoph Knotes' artbd.bash
# 2018-06-21 Translated to Python (hjm)
# 2021-03-05 Modified for ICON-simulations (stem)

import os
import shutil
import logging
import subprocess
from datetime import timedelta
from cdo import Cdo
from . import tools
#from "./tools" import cams4int2cosmo
#from tools import ctnoaa4int2cosmo
#from tools import mozart2int2lm

cdo = Cdo()

def main(starttime, hstart, hstop, cfg):
    """
    **ICON** (if cfg.target is tools.Target.ICON)

    Submitting the runscript for the DWD ICON tools to remap the chemistry files.

    There are 2 runscripts submitted:
        - 1x for the remapping of the lateral boundary conditions
        - 1x for the remapping of the initial conditions

    The chem files are expected to be cams-files and are read-in from project
    directory (``cfg.input_root_chem``) and the remapped chem files
    are saved in the input folder on scratch (``cfg.icon_input/icbc``).

    The chemistry LBC files are merged with the meteo LBC files using
    python-cdo bindings. Thereby, the mixing ratios are transformed to wet
    mixing ratios. This transformation is also done for the initial conditions.

    **COSMO**

    Copy and if necessary process CAMS & CarbonTracker or Mozart
    files for **int2lm**

    Necessary for both **COSMO** and **COSMOART** simulations.

    Copy CAMS (**COSMO**) or CarbonTracker (**COSMO**) or Mozart (**COSMOART**)
    files from project folder to int2lm input folder on scratch
    (``cfg.int2lm_input/icbc``).
    If needed, launch ``cams4int2cosmo``, ``ctnoaa4int2cosmo`` or
    ``mozart2int2lm`` to adapt the files to **int2lm**.

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

    if cfg.target is tools.Target.ICONOEM or cfg.target is tools.Target.ICONART:

        logging.info('ICON chemistry data for IC/BC')

        # Wait for meteo to finish first
        tools.check_job_completion(cfg.log_finished_dir,"meteo")

        tools.create_dir(cfg.icon_input_oae, "online emissions input")
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        tools.create_dir(cfg.icon_input_icbc_processed, "icon_input_icbc_processed")

        starttime_real = starttime + timedelta(hours = hstart)

        #-----------------------------------------------------
        # Remap chemistry initial conditions
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "ic_chem")
        logfile_finish = os.path.join(cfg.log_finished_dir,"ic_chem")

        # Write remap_chem namelist
        in_filename       = os.path.join(cfg.input_root_chem,starttime.strftime(cfg.chem_nameformat)+'.grb')
        out_filename      = os.path.join(cfg.icon_input,'oae',cfg.oae_chem_init_nc+'_dry.nc')
        in_grid_filename  = in_filename
        out_grid_filename = os.path.join(cfg.input_root_grid,cfg.dynamics_grid_filename)
        with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['namelist_remap_chem'])) as input_file:
            to_write = input_file.read()
        output_nml = os.path.join(cfg.icon_work, 'icontools_remap_chem_ic.namelist')
        with open(output_nml, "w") as outf:
            to_write = to_write.format(cfg=cfg,
                                       in_filename=in_filename,
                                       out_filename=out_filename,
                                       in_grid_filename=in_grid_filename,
                                       out_grid_filename=out_grid_filename)
            outf.write(to_write)

        # Write remapfields namelist
        with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['namelist_remapfields_chem_ic'])) as input_file:
            to_write = input_file.read()
        output_fields = os.path.join(cfg.icon_work, 'icontools_remapfields_chem_ic.namelist')
        with open(output_fields, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        # Write run script (remap_ic.job)
        with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['remap_chem_ic_runjob'])) as input_file:
            to_write = input_file.read()
        output_run = os.path.join(cfg.icon_work, "remap_chem_ic.job")
        with open(output_run, "w") as outf:
            outf.write(to_write.format(
                cfg=cfg,
                logfile=logfile, logfile_finish=logfile_finish)
            )
        exitcode = subprocess.call(["sbatch", "--wait",
                                    os.path.join(cfg.icon_work, 'remap_chem_ic.job')])
        if exitcode != 0:
            raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
        logging.info("Remapped initial conditions with icontools")

        os.remove(output_nml)
        os.remove(output_fields)
        os.remove(output_run)

        # Transform initial data from dry to wet mixing ratios
        cdo.expr("'CH4w=CH4*(1-QV)'",input=out_filename,output='temp_file_01.nc')
        cdo.selvar("LNSP",input=out_filename,output='temp_file_03.nc')
        os.remove(out_filename)
        # Rename variable to match ICON internal name with CDO:
        out_filename = os.path.join(cfg.icon_input,'oae',cfg.oae_chem_init_nc)
        cdo.chname("CH4w","CH4",input='temp_file_01.nc',output='temp_file_02.nc')
        cdo.merge(input='temp_file_02.nc temp_file_03.nc',output=out_filename)

        os.remove('temp_file_01.nc')
        os.remove('temp_file_02.nc')
        os.remove('temp_file_03.nc')
        


        #-----------------------------------------------------
        # Remap chem LBC
        #-----------------------------------------------------
        logfile = os.path.join(cfg.log_working_dir, "lbc_chem")
        logfile_finish = os.path.join(cfg.log_finished_dir,"lbc_chem")

        with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['namelist_remapfields_chem_lbc'])) as input_file:
            to_write = input_file.read()
        output_nml_fields = os.path.join(cfg.icon_work, 'icontools_remapfields_chem_lbc.namelist')
        with open(output_nml_fields, "w") as outf:
            to_write = to_write.format(cfg=cfg)
            outf.write(to_write)

        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):

            # Write remap_lbc namelist
            in_grid_filename  = os.path.join(cfg.input_root_chem,starttime.strftime(cfg.chem_nameformat)+'.grb')
            in_filename       = os.path.join(cfg.input_root_chem,time.strftime(cfg.chem_nameformat)+'.grb')
            out_grid_filename = os.path.join(cfg.icon_input_grid,cfg.lateral_boundary_grid)
            out_filename      = os.path.join(cfg.icon_input_icbc,time.strftime(cfg.chem_nameformat)+'_lbc')
            with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['namelist_remap'])) as input_file:
                to_write = input_file.read()
            output_nml_lbc = os.path.join(cfg.icon_work, 'icontools_remap_chem_lbc.namelist')
            with open(output_nml_lbc, "w") as outf:
                to_write = to_write.format(cfg=cfg,
                                           in_grid_filename=in_grid_filename,
                                           in_filename=in_filename,
                                           out_grid_filename=out_grid_filename,
                                           out_filename=out_filename)
                outf.write(to_write)

            # Write run script (remap_chem_lbc.job)
            with open(os.path.join(cfg.case_dir,cfg.icontools_parameter['remap_chem_lbc_runjob'])) as input_file:
                to_write = input_file.read()
            output_run = os.path.join(cfg.icon_work, "remap_chem_lbc.job")
            with open(output_run, "w") as outf:
                outf.write(to_write.format(
                    cfg=cfg,
                    logfile=logfile, logfile_finish=logfile_finish)
                )
            exitcode = subprocess.call(["sbatch", "--wait",
                                        os.path.join(cfg.icon_work, 'remap_chem_lbc.job')])
            if exitcode != 0:
                raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
            logging.info("Remapped boundary conditions at {} with icontools".format(time))

            os.remove(output_nml_lbc)
            os.remove(output_run)

        os.remove(output_nml_fields)


        #-----------------------------------------------------
        # Merge chem files with meteo files using cdo
        #-----------------------------------------------------

        for time in tools.iter_hours(starttime, hstart, hstop, cfg.meteo_inc):

            chem_file = os.path.join(cfg.icon_input_icbc,time.strftime(cfg.chem_nameformat)+'_lbc')
            meteo_file = os.path.join(cfg.icon_input_icbc, time.strftime(cfg.source_nameformat)+'_lbc.nc')
            var_file = os.path.join(cfg.icon_input_icbc, time.strftime(cfg.source_nameformat)+'_lbc_var.nc')
            transform_file = os.path.join(cfg.icon_input_icbc, time.strftime(cfg.source_nameformat)+'_lbc_transform.nc')
            name_file = os.path.join(cfg.icon_input_icbc, time.strftime(cfg.source_nameformat)+'_lbc_name.nc')
            processed_file = os.path.join(cfg.icon_input_icbc_processed, time.strftime(cfg.source_nameformat)+'_lbc.nc')

            # Select variable with CDO
            cdo.selvar("CH4","QV",input=chem_file,output=var_file)
            # Transform to wet-mixing ratios with CDO
            cdo.expr("'CH4w=CH4*(1-QV)'",input=var_file,output=transform_file)
            # Rename variable to match ICON internal name with CDO:
            cdo.chname("CH4w","oem_tracer_1",input=transform_file,output=name_file)
            # Merge with CDO
            cdo.merge(input=name_file+' '+meteo_file,output=processed_file)

            # Delete temporary files
            os.remove(chem_file)
            os.remove(var_file)
            os.remove(transform_file)
            os.remove(name_file)

            logging.info("Merged chem variables to file {}".format(meteo_file))



    # If COSMO (and not ICON):
    else:
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
        else:
            # Unknown target
            raise RuntimeError("Unknown target: {}".format(cfg.target))

        # TO DO 
        #MOZART = dict(fullname="MOZART", nickname="mozart",executable="cams4int2cosmo")
        
        logging.info("Processing " + ", ".join([i["fullname"] for i in inv_to_process])+" data")

        scratch_path = os.path.join(cfg.int2lm_input,'icbc')
        tools.create_dir(scratch_path, "icbc input")

        for inv in inv_to_process:
            logging.info(inv["fullname"]+" files")
            tools.create_dir(inv["outdir"], "processed " + inv["fullname"])
            #process_inv(starttime,hstart,hstop,increment,inv,cfg)
        
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

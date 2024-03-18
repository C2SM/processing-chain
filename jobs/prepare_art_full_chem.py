#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
from . import tools, prepare_icon
import shutil
import subprocess
from datetime import datetime
from .tools.camchem_interpolation import vert_intpl, time_intpl, extract_timeslice
from .tools.camchem_ic_lbc_reggrid import process_lbc, process_ic

BASIC_PYTHON_JOB = True

def main(cfg):
    """
    Prepare ICON-ART simulations with full chemistry.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    logging.info("Prepare ICON-ART for full chemistry simulation.")

    ## ---------------------------
    ## -- Meteorological fields --
    ## ---------------------------
    
    # -- Copy partab_meteo in workdir
    shutil.copy(
        os.path.join(cfg.case_path, 'partab_meteo'),
        os.path.join(cfg.icon_input_icbc, 'partab_meteo'))
    
    # -- Create initial conditions on ICON grid
    if cfg.lrestart == '.FALSE.':
        
        # -- Copy ERA5 processing script in workdir
        with open(
            os.path.join(cfg.case_path, 'icon_era5_ic.sh')
        ) as inf:
            to_write = inf.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_ic.sh')
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))
    
        # -- Run ERA5 processing script
        process = subprocess.Popen([
            "bash",
            os.path.join(cfg.icon_input_icbc, 'icon_era5_ic.sh')
        ],
                                   stdout=subprocess.PIPE)
        process.communicate()
    
    # -- Create lateral boundary conditions on ICON grid

    # -- Collect file list
    datafile_list_meteo = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo_inc):
        meteo_file = os.path.join(cfg.meteo_dir, 
                                  cfg.meteo_prefix + time.strftime(cfg.meteo_nameformat))
        datafile_list_meteo.append(str(meteo_file) + cfg.meteo_suffix)
    datafile_list_meteo = ' '.join([str(v) for v in datafile_list_meteo])

    # -- Copy ERA5 processing script in workdir
    with open(
        os.path.join(cfg.case_path, 'icon_era5_lbc.sh')
    ) as inf:
        to_write = inf.read()
    output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_lbc.sh')
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg,
                                   datafile_list_meteo = datafile_list_meteo))
        
    # -- Run ERA5 processing script
    process = subprocess.Popen([
        "bash",
        os.path.join(cfg.icon_input_icbc, 'icon_era5_lbc.sh')
    ],
                            stdout=subprocess.PIPE)
    process.communicate()

   
    ## ----------------------
    ## -- Chemistry fields -- 
    ## ----------------------

    # -- Define CAM-Chem (CESM2.1) fields for interpolation

    # -- Define aerosol tracers and split of aerosols into modes
    MW_NH4HSO4 = 115.11
    MW_SO4 = 96.06
    MW_CL = 35.453
    MW_NA = 23.0
    MW_NACL = 58.453

    aero_mode_dict = {
        'pom_a4':{
            'poa_mixed_ait':0.9,  # 90%-->mixed, 10%-->sol/insol 
            'poa_insol_ait':0.1
            },
        'pom_a1':{
            'poa_mixed_acc':0.9, 
            'poa_insol_acc':0.1
            },
        'bc_a4':{
            'soot_mixed_ait':0.9, 
            'soot_insol_ait':0.1
            },
        'bc_a1':{
            'soot_mixed_acc':0.9, 
            'soot_insol_acc':0.1
            },
        'so4_a2':{
            'so4_mixed_ait':0.9*MW_SO4/MW_NH4HSO4,  # Sulfat aerosol in CAM-Chem has composition NH4HSO4
            'so4_sol_ait':0.1*MW_SO4/MW_NH4HSO4
            },
        'so4_a1':{
            'so4_mixed_acc':0.9*MW_SO4/MW_NH4HSO4, 
            'so4_sol_acc':0.1*MW_SO4/MW_NH4HSO4
            },
        'so4_a3':{
            'so4_mixed_coa':0.9*MW_SO4/MW_NH4HSO4, 
            'so4_sol_coa':0.1*MW_SO4/MW_NH4HSO4
            },
        'ncl_a2':{
            'na_mixed_ait':0.9*MW_NA/MW_NACL, 
            'na_sol_ait':0.1*MW_NA/MW_NACL, 
            'cl_mixed_ait':0.9*MW_CL/MW_NACL, 
            'cl_sol_ait':0.1*MW_CL/MW_NACL
            },
        'ncl_a1':{
            'na_mixed_acc':0.9*MW_NA/MW_NACL, 
            'na_sol_acc':0.1*MW_NA/MW_NACL, 
            'cl_mixed_acc':0.9*MW_CL/MW_NACL, 
            'cl_sol_acc':0.1*MW_CL/MW_NACL},
        'ncl_a3':{
            'na_mixed_coa':0.9*MW_NA/MW_NACL, 
            'na_sol_coa':0.1*MW_NA/MW_NACL, 
            'cl_mixed_coa':0.9*MW_CL/MW_NACL, 
            'cl_sol_coa':0.1*MW_CL/MW_NACL},
        'dst_a2':{
            'dust_mixed_ait':0.9, 
            'dust_insol_ait':0.1
            },
        'dst_a1':{
            'dust_mixed_acc':0.9, 
            'dust_insol_acc':0.1
            },
        'dst_a3':{
            'dust_mixed_coa':0.9, 
            'dust_insol_coa':0.1
            },
        'NH4':{
            'nh4_mixed_acc':0.9*0.9*0.9,    # Ammonium aerosol 90%-->Aitken/Accumulation, 
            'nh4_sol_acc':0.9*0.9*0.1,      # 10%-->Coarse, 90%-->Accumulation, 10%-->Aitken
            'nh4_mixed_ait':0.9*0.1*0.9, 
            'nh4_sol_ait':0.9*0.1*0.1, 
            'nh4_mixed_coa':0.1*0.9,
            'nh4_sol_coa':0.1*0.1
            }
        }

    # -- Define chemical tracers and molar weights for VMR --> MMR
    chem_mw_dict = {
        'ALKNIT':133.1460,
        'BENZENE':78.1121,
        'BIGALD':98.1001,
        'BIGALD1':84.0735,
        'BIGALD2':98.1001,
        'BIGALD3':98.1001,
        'BIGALD4':112.1268,
        'BIGALK':72.1490,
        'BIGENE':56.1065,
        'C2H2':26.0374,
        'C2H4':28.0532,
        'C2H5OH':46.0685,
        'C2H6':30.0691,
        'C3H6':42.0799,
        'C3H8':44.0957,
        'CH2O':30.0260,
        'CH3CHO':44.0526,
        'CH3COCH3':58.0793,
        'CH3COCHO':72.0628,
        'CH3COOH':60.0520,
        'CH3OH':32.0419,
        'CH3OOH':48.0413,
        'CH4':16.0425,
        'CO':28.0101,
        'CRESOL':108.1381,
        'DMS':62.1340,
        'GLYOXAL':26.0374,
        'H2O':18.0153,
        'H2O2':34.0147,
        'HCOOH':46.0254,
        'HNO3':63.0128,
        'HO2':33.0068,
        'HO2NO2':79.0123,
        'HONITR':135.1188,
        'HYAC':74.0787,
        'ISOP':68.1172,
        'ISOPNITA':147.1295,
        'ISOPNITB':147.1295,
        'MACR':70.0900,
        'MEK':72.1059,
        'MPAN':147.0864,
        'MVK':70.0900,
        'N2O':44.0129,
        'N2O5':108.0105,
        'NH3':17.0306,
        'NO':30.0061,
        'NO2':46.0056,
        'NO3':62.0050,
        'NOA':119.0762,
        'O3':47.9982,
        'OH':17.0073,
        'ONITR':133.1029,
        'PAN':121.0491,
        'PBZNIT':183.1186,
        'PHENOL':94.1115,
        'SO2':64.0648,
        'TERPNIT':215.2467,
        'TOLUENE':92.1387,
        'XYLENES':106.1653
    }
    
    camchem_spec = list(aero_mode_dict.keys()) + list(chem_mw_dict.keys())

    chem_filename = os.path.join(cfg.chem_dir, 
                                 cfg.chem_icbc_filename)
    meteo_filename = os.path.join(cfg.icon_input_icbc, 
                                  cfg.meteo_prefix +
                                  cfg.startdate_sim.strftime(cfg.meteo_nameformat) + 
                                  '_lbc' +
                                  cfg.inidata_filename_suffix)
    tmp_filename = os.path.join(cfg.icon_input_icbc,
                                'camchem_temp.nc')

    # -- Vertically interpolate CAM-Chem fields
    vert_intpl(chem_filename, 
               meteo_filename, 
               tmp_filename, 
               camchem_spec,
               cfg.startdate_sim, 
               cfg.enddate_sim,
               cfg.chem_ref_date)
    
    # -- Create temporary folder for time slices of chem data
    tmp_dir = os.path.join(cfg.icon_input_icbc,'chem_data_tmp')
    if os.path.exists(tmp_dir):
        os.rmdir(tmp_dir)
    os.makedirs(tmp_dir)

    # -- Extract one NetCDF file per time step
    out_template = os.path.join(tmp_dir, 
                                cfg.chem_prefix + 
                                cfg.chem_nameformat + 
                                cfg.chem_suffix)
    
    extract_timeslice(tmp_filename, 
                      out_template, 
                      camchem_spec,
                      cfg.chem_ref_date)

    # -- Remove tmp file with vertically interpolated fields
    os.remove(tmp_filename)
    
    # -- Linear interpolation with respect to time 
    time_intpl(tmp_dir,
               cfg.chem_prefix)
                                   
    # -- Prepare data for initial conditions
    process_ic(tmp_dir, 
               cfg.icon_input_icbc, 
               cfg.startdate_sim,
               aero_mode_dict,
               cfg.chem_prefix)

    # -- Prepare data for lateral boundary conditions
    process_lbc(tmp_dir,
                cfg.icon_input_icbc,
                cfg.startdate_sim,
                cfg.enddate_sim,
                cfg.chem_inc,
                cfg.chem_prefix,
                cfg.chem_suffix,
                cfg.chem_nameformat,
                chem_mw_dict,
                aero_mode_dict)
    
    # -- Remove tmp folder with chem data
    shutil.rmtree(tmp_dir)
                             
    # -- Copy partab_chem in workdir
    shutil.copy(
        os.path.join(cfg.case_path, 'partab_chem'),
        os.path.join(cfg.icon_input_icbc, 'partab_chem'))

    # -- Create initial conditions on ICON grid
    if cfg.lrestart == '.FALSE.':
        # -- Copy CAM-Chem processing script in workdir
        with open(
            os.path.join(cfg.case_path, 'icon_camchem_ic.sh')
        ) as inf:
            to_write = inf.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_camchem_ic.sh')
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))

        # -- Run CAM-Chem processing script
        process = subprocess.Popen([
            "bash",
            os.path.join(cfg.icon_input_icbc, 'icon_camchem_ic.sh')
        ],
                                stdout=subprocess.PIPE)
        process.communicate()

    # -- Create lateral boundary conditions on ICON grid

    # -- Collect file list 
    datafile_list_chem = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo_inc):
        chem_file = os.path.join(cfg.icon_input_icbc, 
                                 cfg.chem_prefix + time.strftime(cfg.chem_nameformat))
        datafile_list_chem.append(str(chem_file) + '_lbc' + cfg.chem_suffix)
    datafile_list_chem = ' '.join([str(v) for v in datafile_list_chem])

    # -- Copy CAM-Chem processing script in workdir
    with open(
        os.path.join(cfg.case_path, 'icon_camchem_lbc.sh')
    ) as inf:
        to_write = inf.read()
    output_file = os.path.join(cfg.icon_input_icbc, 'icon_camchem_lbc.sh')
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg,
                                   datafile_list_chem = datafile_list_chem))
    
    # -- Run CAM-Chem processing script
    process = subprocess.Popen([
        "bash",
        os.path.join(cfg.icon_input_icbc, 'icon_camchem_lbc.sh')
    ],
                               stdout=subprocess.PIPE)
    process.communicate()

    # -- Create symbolic link to LBC file at experiment start
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.ini_LBC_file, cfg.ini_LBC_file_scratch)
        
    ## ----------------------------------------------
    ## -- Merging IC/LBC for meteo and chem fields --
    ## ----------------------------------------------

    logging.info('Merging IC and LBC')

    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                     cfg.meteo_inc):
        if time == cfg.startdate_sim:
            # -- Merge IC
            meteo_file = os.path.join(
                cfg.icon_input_icbc,
                time.strftime(cfg.meteo_prefix + cfg.meteo_nameformat) + 
                '_ic.nc')
            if os.path.isfile(meteo_file):
                chem_file = os.path.join(
                    cfg.icon_input_icbc, 
                    time.strftime(cfg.chem_prefix + cfg.chem_nameformat) + 
                    '_ic_icon.nc')
                merged_file = os.path.join(
                    cfg.icon_input_icbc,
                    time.strftime(cfg.inidata_prefix + cfg.inidata_nameformat) + 
                    cfg.inidata_filename_suffix)
                ds_meteo = xr.open_dataset(meteo_file)
                ds_chem = xr.open_dataset(chem_file)
                ds_merged = xr.merge([ds_meteo, ds_chem],
                                     compat="override")
                ds_merged.to_netcdf(merged_file)
                # Rename file to get original file name
                tools.remove_file(meteo_file)
                tools.remove_file(chem_file)
                logging.info(
                    "Added MECCA tracers to file {}".format(merged_file))

        # -- Merge LBC
        meteo_file = os.path.join(
        cfg.icon_input_icbc,
            time.strftime(cfg.meteo_prefix + cfg.meteo_nameformat) +
            '_lbc.nc')
        chem_file = os.path.join(
            cfg.icon_input_icbc, 
            time.strftime(cfg.chem_prefix + cfg.chem_nameformat) + 
            '_lbc_icon.nc')
        merged_file = os.path.join(
                    cfg.icon_input_icbc,
                    time.strftime(cfg.lbcdata_prefix + cfg.lbcdata_nameformat) + 
                    cfg.lbcdata_filename_suffix)
        ds_meteo = xr.open_dataset(meteo_file)
        ds_chem = xr.open_dataset(chem_file)
        ds_merged = xr.merge([ds_meteo, ds_chem], 
                             compat="override")
        ds_merged.to_netcdf(merged_file)
        tools.remove_file(meteo_file)
        tools.remove_file(chem_file)
        logging.info(
            "Added MECCA tracers to file {}".format(merged_file))
        
    ## -----------------------------------
    ## -- Add Q (copy of QV) to IC file --
    ## -----------------------------------
        
    logging.info('Add Q (copy of QV) to initial file')
    ic_file = os.path.join(
    cfg.icon_input_icbc,
    cfg.startdate_sim.strftime(cfg.inidata_prefix +
                           cfg.inidata_nameformat +
                           cfg.inidata_filename_suffix)
    )
    if os.path.isfile(ic_file):
        merged_file = os.path.join(
        cfg.icon_input_icbc,
        cfg.startdate_sim.strftime(cfg.inidata_prefix +
                               cfg.inidata_nameformat + 
                                '_merged' +
                                   cfg.inidata_filename_suffix)
        )
        ds = xr.open_dataset(ic_file)
        merging = False
        if 'Q' not in ds:
            merging = True
            ds['Q'] = ds['QV']
            logging.info(f"Added Q to file {ic_file}")
        if merging:
            ds.to_netcdf(merged_file)
            tools.rename_file(merged_file, ic_file)
#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
from . import tools, prepare_icon
import shutil
import subprocess

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
    logging.info("Prepare ICON-ART for full chemistry LAM simulation.")

    ## ----------------------------------
    ## -- Remapping from EU to CH grid --
    ## ----------------------------------

    # -- Copy partab_LAM in workdir
    shutil.copy(os.path.join(cfg.case_path, 'partab_LAM'),
                os.path.join(cfg.icon_input_icbc, 'partab_LAM'))

    # -- Create initial conditions on ICON grid
    if cfg.lrestart == '.FALSE.':

        # -- startdate in ICON output format
        date_icon_out_format = cfg.startdate_sim.strftime(
            cfg.LAM_icon_output_nameformat)

        # -- Copy IC remapping script in workdir
        with open(os.path.join(cfg.case_path, 'icon_LAM_ic.sh')) as inf:
            to_write = inf.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_LAM_ic.sh')
        with open(output_file, "w") as outf:
            outf.write(
                to_write.format(cfg=cfg,
                                date_icon_out_format=date_icon_out_format))

        # -- Run IC remapping script
        process = subprocess.Popen(
            ["bash",
             os.path.join(cfg.icon_input_icbc, 'icon_LAM_ic.sh')],
            stdout=subprocess.PIPE)
        process.communicate()

    # -- Create lateral boundary conditions on ICON grid

    # -- Collect file list
    datafile_list = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.LAM_inc):
        icon_output_file = os.path.join(
            cfg.LAM_icon_output_dir, cfg.LAM_prefix + "LBC_" +
            time.strftime(cfg.LAM_icon_output_nameformat))
        datafile_list.append(str(icon_output_file) + cfg.LAM_suffix)
    datafile_list = ' '.join([str(v) for v in datafile_list])

    # -- Copy LBC remapping script in workdir
    with open(os.path.join(cfg.case_path, 'icon_LAM_lbc.sh')) as inf:
        to_write = inf.read()
    output_file = os.path.join(cfg.icon_input_icbc, 'icon_LAM_lbc.sh')
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg, datafile_list=datafile_list))

    # -- Run LBC remapping script
    process = subprocess.Popen(
        ["bash", os.path.join(cfg.icon_input_icbc, 'icon_LAM_lbc.sh')],
        stdout=subprocess.PIPE)
    process.communicate()

    # -- Rename LBC files
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.LAM_inc):
        old_name = os.path.join(
            cfg.icon_input_icbc, cfg.LAM_prefix + "LBC_" +
            time.strftime(cfg.LAM_icon_output_nameformat) + '_tmp' +
            cfg.lbcdata_filename_suffix)
        new_name = os.path.join(
            cfg.icon_input_icbc,
            cfg.lbcdata_prefix + time.strftime(cfg.lbcdata_nameformat) +
            '_tmp' + cfg.lbcdata_filename_suffix)
        tools.rename_file(old_name, new_name)

    ## ------------------------------
    ## -- Chemistry and Aerosol IC --
    ## ------------------------------

    substr_chem = ["_full"]
    substr_aero = ["_insol", "_sol", "_mixed", "_giant"]
    var_list = [
        'clon', 'clon_bnds', 'clat', 'clat_bnds', 'height', 'height_bnds',
        'time'
    ]

    if cfg.lrestart == '.FALSE.':
        in_file = os.path.join(
            cfg.icon_input_icbc,
            cfg.inidata_prefix + cfg.startdate_sim_yyyymmddhh + '_tmp' +
            cfg.inidata_filename_suffix)
        out_meteo = os.path.join(
            cfg.icon_input_icbc, cfg.inidata_prefix +
            cfg.startdate_sim_yyyymmddhh + cfg.inidata_filename_suffix)
        out_chem = os.path.join(cfg.art_input_folder, cfg.chem_ini_filename)
        out_aero = os.path.join(cfg.art_input_folder, cfg.aero_ini_filename)

        # -- Create datasets and copy global attributes
        ds_in = xr.open_dataset(in_file)
        ds_meteo = xr.Dataset(attrs=ds_in.attrs)
        ds_chem = xr.Dataset(attrs=ds_in.attrs)
        ds_aero = xr.Dataset(attrs=ds_in.attrs)

        # -- Write variables to IC files
        for var_name, var in ds_in.variables.items():
            var_to_copy = var.copy(deep=True)
            if var_name in var_list:
                ds_chem[var_name] = var_to_copy
                ds_aero[var_name] = var_to_copy
                ds_meteo[var_name] = var_to_copy

            else:
                # -- Copy gas phase variables
                if any(substring in var_name for substring in substr_chem):
                    ds_chem[var_name] = var_to_copy

                # -- Copy aerosol variables
                elif any(substring in var_name for substring in substr_aero):
                    ds_aero[var_name] = var_to_copy

                # -- Copy meteo variables
                else:
                    ds_meteo[var_name] = var_to_copy

        # -- Write datasets to file
        ds_meteo.to_netcdf(out_meteo)
        ds_chem.to_netcdf(out_chem)
        ds_aero.to_netcdf(out_aero)

        # -- Close the datasets
        ds_in.close()
        ds_meteo.close()
        ds_chem.close()
        ds_aero.close()

        # -- Remove temp file
        tools.remove_file(in_file)

        logging.info("Created IC files {} and {}".format(out_chem, out_aero))

    # -- CK: The following is because of memory issues, try to resolve in a different way!
    ## ----------------------------------------
    ## -- Reduce number of variables for LBC --
    ## ----------------------------------------

    lbc_list = [
        'ALKNO3_full', 'BENZENE_full', 'C5H6O2_full', 'BIGALD1_full',
        'BIGALD2_full', 'BIGALD3_full', 'BIGALD4_full', 'C5H12_full',
        'C4H8_full', 'C2H2_full', 'C2H4_full', 'C2H5OH_full', 'C2H6_full',
        'C3H6_full', 'C3H8_full', 'HCHO_full', 'CH3CHO_full', 'CH3COCH3_full',
        'MGLYOX_full', 'CH3CO2H_full', 'CH3OH_full', 'CH3OOH_full', 'CH4_full',
        'CO_full', 'CRESOL_full', 'DMS_full', 'GLYOX_full', 'H2O_full',
        'H2O2_full', 'HCOOH_full', 'HNO3_full', 'HO2_full', 'HNO4_full',
        'LHONITR_full', 'ACETOL_full', 'C5H8_full', 'LISOPBDNO3_full',
        'LISOPACNO3_full', 'MACR_full', 'MEK_full', 'MPAN_full', 'MVK_full',
        'N2O_full', 'N2O5_full', 'NH3_full', 'NO_full', 'NO2_full', 'NO3_full',
        'NOA_full', 'O3_full', 'OH_full', 'LONITR_full', 'PAN_full',
        'PBZNIT_full', 'PHENOL_full', 'SO2_full', 'BPINANO3_full',
        'TOLUENE_full', 'LXYL_full', 'APINENE_full', 'BPINENE_full',
        'MYRC_full', 'LIMON_full', 'BCARY_full', 'MBO_full', 'nmb_sol_ait',
        'nmb_sol_acc', 'nmb_sol_coa', 'nmb_insol_ait', 'nmb_insol_acc',
        'nmb_insol_coa', 'nmb_mixed_ait', 'nmb_mixed_acc', 'nmb_mixed_coa',
        'nmb_giant', 'dust_insol_acc', 'dust_insol_coa', 'dust_mixed_acc',
        'dust_mixed_coa', 'dust_giant', 'seas_giant', 'na_sol_ait',
        'na_sol_acc', 'na_sol_coa', 'na_mixed_ait', 'na_mixed_acc',
        'na_mixed_coa', 'cl_sol_ait', 'cl_sol_acc', 'cl_sol_coa',
        'cl_mixed_ait', 'cl_mixed_acc', 'cl_mixed_coa', 'soot_insol_ait',
        'soot_mixed_ait', 'soot_insol_acc', 'soot_mixed_acc', 'soot_insol_coa',
        'soot_mixed_coa', 'poa_insol_ait', 'poa_mixed_ait', 'poa_insol_acc',
        'poa_mixed_acc', 'poa_insol_coa', 'poa_mixed_coa', 'pm_insol_ait',
        'pm_mixed_ait', 'pm_insol_acc', 'pm_mixed_acc', 'pm_insol_coa',
        'pm_mixed_coa', 'so4_sol_ait', 'so4_sol_acc', 'so4_sol_coa',
        'so4_mixed_ait', 'so4_mixed_acc', 'so4_mixed_coa', 'nh4_sol_ait',
        'nh4_sol_acc', 'nh4_sol_coa', 'nh4_mixed_ait', 'nh4_mixed_acc',
        'nh4_mixed_coa', 'no3_sol_ait', 'no3_sol_acc', 'no3_sol_coa',
        'no3_mixed_ait', 'no3_mixed_acc', 'no3_mixed_coa', 'u', 'v', 'w', 'P',
        'T', 'qv', 'qc', 'qi', 'qr', 'qs', 'hhl', 'clat', 'clat_bnds', 'clon',
        'clon_bnds', 'depth', 'depth_2', 'depth_2_bnds', 'height',
        'height_bnds', 'height_2', 'height_3', 'height_3_bnds', 'height_4',
        'height_5', 'height_6', 'height_6_bnds', 'time'
    ]

    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.LAM_inc):
        in_file = os.path.join(
            cfg.icon_input_icbc,
            cfg.lbcdata_prefix + time.strftime(cfg.lbcdata_nameformat) +
            '_tmp' + cfg.lbcdata_filename_suffix)
        out_file = os.path.join(
            cfg.icon_input_icbc,
            cfg.lbcdata_prefix + time.strftime(cfg.lbcdata_nameformat) +
            cfg.lbcdata_filename_suffix)

        ds_in = xr.open_dataset(in_file)

        all_vars = list(ds_in.variables)

        # -- Select LBC variables
        lbc_vars = [var for var in all_vars if var in lbc_list]

        # -- Create dataset and copy global attributes
        ds_out = xr.Dataset(attrs=ds_in.attrs)

        # -- Copy variables
        for var_name in lbc_vars:
            lbc_var = ds_in[var_name]
            lbc_var_to_copy = lbc_var.copy(deep=True)

            ds_out[var_name] = lbc_var_to_copy

        # -- Write dataset to NetCDF file
        ds_out.to_netcdf(out_file)

        # -- Close datasets
        ds_in.close()
        ds_out.close()

        # -- Remove temp file
        tools.remove_file(in_file)

    # -- Create symbolic link to LBC file at experiment start
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.ini_LBC_file, cfg.ini_LBC_file_scratch)

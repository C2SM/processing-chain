#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import contextlib
import logging
import xarray as xr
import numpy as np
import shutil
import subprocess
from .tools.camchem_interpolation import vert_intpl, time_intpl, extract_timeslice
from .tools.camchem_ic_lbc_reggrid import process_lbc, process_ic
from . import tools, prepare_icon

BASIC_PYTHON_JOB = True


def chem_sources(cfg):
    """Chemistry input sources, each remapped and interpolated separately.

    - 'chem'      : CAMS v25r1 CH4 (always)
    - 'chem_c2h6' : EAC4 C2H6 (only if the section exists in config.yaml)

    Each source has its own vertical grid and surface pressure, so they can
    not be put into one file before the vertical interpolation.
    """
    sources = [
        dict(dir=cfg.chem_dir,
             prefix=cfg.chem_prefix,
             suffix=cfg.chem_suffix,
             mw={'CH4': 16.0425})
    ]
    if getattr(cfg, 'chem_c2h6_prefix', None):
        sources.append(
            dict(dir=cfg.chem_c2h6_dir,
                 prefix=cfg.chem_c2h6_prefix,
                 suffix=cfg.chem_c2h6_suffix,
                 mw={'C2H6': 30.069}))
    for src in sources:
        # -- Monthly input file (lat/lon)
        src['monthly'] = os.path.join(
            src['dir'],
            src['prefix'] + cfg.startdate_sim_yyyymm + src['suffix'])
    return sources


def run_chem_remap_script(cfg, script_name, chem_in, chem_filename):
    """Copy a chemistry remapping script into the workdir and run it.

    The script remaps the monthly chemistry input `chem_in` from lat/lon onto
    an unstructured grid and has to create `chem_filename`.
    """
    with open(os.path.join(cfg.case_path, script_name)) as inf:
        to_write = inf.read()
    output_file = os.path.join(cfg.icon_input_icbc, script_name)
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg, chem_in=chem_in, chem_out=chem_filename))

    process = subprocess.Popen(["bash", output_file], stdout=subprocess.PIPE)
    process.communicate()
    if process.returncode != 0 or not os.path.isfile(chem_filename):
        raise RuntimeError(
            f"{script_name} failed (exit code {process.returncode}), "
            f"{chem_filename} not created")


def chem_to_timeslices(cfg, src, chem_filename, meteo_filename, tmp_dir, start,
                       end, nameformat):
    """Vertically interpolate remapped chemistry fields onto the meteo levels
    and write one file per time step (incl. time interpolation) to tmp_dir.

    The remapped monthly chemistry file is removed afterwards.
    """
    tmp_filename = os.path.join(cfg.icon_input_icbc, 'chem_temp.nc')
    spec = list(src['mw'])

    # -- Vertically interpolate CAM-Chem fields
    vert_intpl(chem_filename, meteo_filename, tmp_filename, spec, start, end,
               cfg.chem_ref_date)

    # -- Remove remapped monthly chemistry file (large)
    tools.remove_file(chem_filename)

    # -- Create temporary folder for time slices of chem data
    if os.path.exists(tmp_dir):
        shutil.rmtree(tmp_dir)
    os.makedirs(tmp_dir)

    # -- Extract one NetCDF file per time step
    out_template = os.path.join(tmp_dir,
                                src['prefix'] + nameformat + src['suffix'])
    extract_timeslice(tmp_filename, out_template, spec, cfg.chem_ref_date)

    # -- Remove tmp file with vertically interpolated fields
    os.remove(tmp_filename)

    # -- Linear interpolation with respect to time
    time_intpl(tmp_dir, src['prefix'])


@contextlib.contextmanager
def open_chem(chem_files):
    """Open the chemistry files of all sources (same time step) as one dataset.

    All sources are on the same grid and meteo levels (same clat/clon, lev_m,
    hyam, ...). Variables present in several files, e.g. 'PS', are taken
    from the first source (CAMS CH4).
    """
    with contextlib.ExitStack() as stack:
        datasets = [
            stack.enter_context(xr.open_dataset(f)) for f in chem_files
        ]
        yield xr.merge(datasets, compat='override')


def latbc_filename_format(cfg):
    """strftime format of the LBC files ICON reads (namelist latbc_filename).

    Converts the ICON keywords in e.g. 'ERA5_<y><m><d><h>_lbc.nc' to
    'ERA5_%Y%m%d%H_lbc.nc'. Falls back to lbcdata_* if latbc_filename is not
    set or contains unsupported keywords.
    """
    fmt = getattr(cfg, 'latbc_filename', None)
    if fmt:
        for key, val in [('<y>', '%Y'), ('<m>', '%m'), ('<d>', '%d'),
                         ('<h>', '%H'), ('<min>', '%M'), ('<sec>', '%S')]:
            fmt = fmt.replace(key, val)
        if '<' not in fmt:
            return fmt
    return (cfg.lbcdata_prefix + cfg.lbcdata_nameformat +
            cfg.lbcdata_filename_suffix)


def main(cfg):
    """
    Prepare ICON-ART simulations.

    - Add GEOSP to all meteo files that don't contain it
    - Add Q (copy of QV) and/or PS to initial file
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)

    ## ---------------------------
    ## -- Meteorological fields --
    ## ---------------------------

    # -- Copy partab_meteo in workdir
    shutil.copy(os.path.join(cfg.case_path, 'partab_meteo'),
                os.path.join(cfg.icon_input_icbc, 'partab_meteo'))

    # -- Create initial conditions on ICON grid
    if cfg.lrestart == '.FALSE.':

        # -- Copy ERA5 processing script in workdir
        with open(os.path.join(cfg.case_path, 'icon_era5_ic.sh')) as inf:
            to_write = inf.read()
        output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_ic.sh')
        print(output_file, cfg)
        with open(output_file, "w") as outf:
            outf.write(to_write.format(cfg=cfg))

        # -- Run ERA5 processing script
        # process = subprocess.Popen(
        #     ["bash",
        #      os.path.join(cfg.icon_input_icbc, 'icon_era5_ic.sh')],
        #     stdout=subprocess.PIPE)
        process = subprocess.Popen(
            # ["uenv", "run", "netcdf-tools/2025:v1", "--view=default", "--",
            ["bash",
             os.path.join(cfg.icon_input_icbc, 'icon_era5_ic.sh')],
            stdout=subprocess.PIPE)
        process.communicate()

    # -- Create lateral boundary conditions on ICON grid

    # -- Collect file list
    datafile_list_meteo = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo_inc):
        meteo_file = os.path.join(
            cfg.meteo_dir,
            cfg.meteo_prefix + time.strftime(cfg.meteo_nameformat))
        datafile_list_meteo.append(str(meteo_file) + cfg.meteo_suffix)
    datafile_list_meteo = ' '.join([str(v) for v in datafile_list_meteo])

    # -- Copy ERA5 processing script in workdir
    with open(os.path.join(cfg.case_path, 'icon_era5_lbc.sh')) as inf:
        to_write = inf.read()
    output_file = os.path.join(cfg.icon_input_icbc, 'icon_era5_lbc.sh')
    with open(output_file, "w") as outf:
        outf.write(
            to_write.format(cfg=cfg, datafile_list_meteo=datafile_list_meteo))

    # -- Run ERA5 processing script
    process = subprocess.Popen(
        # ["uenv", "run", "netcdf-tools/2025:v1", "--view=default", "--",
        ["bash", os.path.join(cfg.icon_input_icbc, 'icon_era5_lbc.sh')],
        stdout=subprocess.PIPE)
    process.communicate()

    aero_mode_dict = {}
    sources = chem_sources(cfg)

    # -- The chemistry input is remapped horizontally BEFORE the vertical
    # -- interpolation:
    # --   IC : icon_camchem_ic.sh  (lat/lon --> ICON grid)
    # --   LBC: icon_camchem_lbc.sh (lat/lon --> lateral boundary grid)
    # -- All following steps work on the unstructured grid ('ncells').
    # -- This is done once per chemistry source (see chem_sources); the
    # -- resulting files are merged with the meteo files below.

    # -- Name format of the hourly chemistry files
    # -- (cfg.chem_nameformat is the format of the monthly input file, '%Y%m')
    chem_nameformat_h = '%Y%m%d%H'

    # -- Meteo file, only used for its hybrid coefficients (hyam, hybm, ...)
    meteo_filename = os.path.join(
        cfg.icon_input_icbc,
        cfg.meteo_prefix + cfg.startdate_sim.strftime(cfg.meteo_nameformat) +
        '_lbc' + cfg.inidata_filename_suffix)
    tmp_dir = os.path.join(cfg.icon_input_icbc, 'chem_data_tmp')

    for src in sources:
        logging.info(f"Chemistry {list(src['mw'])} from {src['monthly']}")

        # -- Initial conditions (ICON grid)
        if cfg.lrestart == '.FALSE.':
            chem_filename = os.path.join(
                cfg.icon_input_icbc, src['prefix'] + cfg.startdate_sim_yyyymm +
                '_icon' + cfg.inidata_filename_suffix)
            run_chem_remap_script(cfg, 'icon_camchem_ic.sh', src['monthly'],
                                  chem_filename)
            chem_to_timeslices(cfg, src, chem_filename, meteo_filename,
                               tmp_dir, cfg.startdate_sim, cfg.startdate_sim,
                               chem_nameformat_h)

            # -- (converted from mole fraction to [kg/kg], as for the LBC)
            process_ic(tmp_dir, cfg.icon_input_icbc, cfg.startdate_sim,
                       aero_mode_dict, src['prefix'], src['mw'])
            shutil.rmtree(tmp_dir)

            ic_name = cfg.startdate_sim.strftime(src['prefix'] +
                                                 chem_nameformat_h)
            os.replace(
                os.path.join(cfg.icon_input_icbc, ic_name + '_ic.nc'),
                os.path.join(cfg.icon_input_icbc, ic_name + '_ic_icon.nc'))

        # -- Lateral boundary conditions (lateral boundary grid)
        chem_filename = os.path.join(
            cfg.icon_input_icbc, src['prefix'] + cfg.startdate_sim_yyyymm +
            '_icon_lbc' + cfg.inidata_filename_suffix)
        run_chem_remap_script(cfg, 'icon_camchem_lbc.sh', src['monthly'],
                              chem_filename)
        chem_to_timeslices(cfg, src, chem_filename, meteo_filename, tmp_dir,
                           cfg.startdate_sim, cfg.enddate_sim,
                           chem_nameformat_h)

        process_lbc(tmp_dir, cfg.icon_input_icbc, cfg.startdate_sim,
                    cfg.enddate_sim, cfg.chem_inc, src['prefix'],
                    src['suffix'], chem_nameformat_h, src['mw'],
                    aero_mode_dict)
        shutil.rmtree(tmp_dir)

        for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                     cfg.chem_inc):
            lbc_name = time.strftime(src['prefix'] + chem_nameformat_h)
            os.replace(
                os.path.join(cfg.icon_input_icbc,
                             lbc_name + '_lbc' + src['suffix']),
                os.path.join(cfg.icon_input_icbc, lbc_name + '_lbc_icon.nc'))

    # -- Create symbolic link to LBC file at experiment start
    if cfg.lrestart == '.TRUE.':
        tools.symlink_file(cfg.ini_LBC_file, cfg.ini_LBC_file_scratch)

    ## ----------------------------------------------
    ## -- Merging IC/LBC for meteo and chem fields --
    ## ----------------------------------------------

    logging.info('Merging IC and LBC')

    # -- File names ICON reads: IC from inidata_* (see jobs/icon.py),
    # -- LBC from latbc_filename (e.g. 'ERA5_<y><m><d><h>_lbc.nc').
    # -- Merged files are written to a temporary name first, because the
    # -- target name can be identical to the meteo input file name.
    ic_target = os.path.join(
        cfg.icon_input_icbc,
        cfg.startdate_sim.strftime(cfg.inidata_prefix +
                                   cfg.inidata_nameformat +
                                   cfg.inidata_filename_suffix))
    lbc_target_format = latbc_filename_format(cfg)

    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo_inc):
        if time == cfg.startdate_sim:
            # -- Merge IC
            # -- (icon_era5_ic.sh writes either <meteo>_ic.nc or directly
            # -- the inidata file name)
            meteo_file = os.path.join(
                cfg.icon_input_icbc,
                time.strftime(cfg.meteo_prefix + cfg.meteo_nameformat) +
                '_ic.nc')
            if not os.path.isfile(meteo_file):
                meteo_file = ic_target
            chem_files = [
                os.path.join(
                    cfg.icon_input_icbc,
                    time.strftime(src['prefix'] + chem_nameformat_h) +
                    '_ic_icon.nc') for src in sources
            ]
            if os.path.isfile(meteo_file) and all(
                    os.path.isfile(f) for f in chem_files):
                tmp_file = ic_target + '.merge_tmp'
                with xr.open_dataset(meteo_file) as ds_meteo, \
                        open_chem(chem_files) as ds_chem:
                    # Replace "PS" from CAM-Chem by ERA5 value
                    ds_chem["PS"] = np.exp(ds_meteo["LNPS"])
                    ds_chem['Q'] = ds_meteo['QV']
                    logging.info(f"Added PS and Q to file {ic_target}")
                    ds_merged = xr.merge([ds_meteo, ds_chem],
                                         compat="override")
                    ds_merged.to_netcdf(tmp_file)
                tools.remove_file(meteo_file)
                for f in chem_files:
                    tools.remove_file(f)
                os.replace(tmp_file, ic_target)
                logging.info(
                    "Added MECCA tracers to file {}".format(ic_target))
            elif cfg.lrestart == '.FALSE.':
                raise FileNotFoundError(
                    f"IC merge: {meteo_file} or {chem_files} not found")

        # -- Merge LBC
        meteo_file = os.path.join(
            cfg.icon_input_icbc,
            time.strftime(cfg.meteo_prefix + cfg.meteo_nameformat) + '_lbc.nc')
        chem_files = [
            os.path.join(
                cfg.icon_input_icbc,
                time.strftime(src['prefix'] + chem_nameformat_h) +
                '_lbc_icon.nc') for src in sources
        ]
        merged_file = os.path.join(cfg.icon_input_icbc,
                                   time.strftime(lbc_target_format))
        if merged_file == ic_target:
            raise ValueError(
                f"LBC file name {merged_file} is identical to the IC file "
                "name; check latbc_filename / inidata_* in config.yaml")
        tmp_file = merged_file + '.merge_tmp'

        with xr.open_dataset(meteo_file) as ds_meteo, \
                open_chem(chem_files) as ds_chem:

            if 'GEOP_ML' not in ds_meteo.variables:
                logging.warning(
                    f"'GEOP_ML' missing in {meteo_file}. Attempting to copy from 00:00 UTC file."
                )
                zero_hour = time.replace(hour=0, minute=0, second=0)
                zero_hour_file = os.path.join(
                    cfg.icon_input_icbc, zero_hour.strftime(lbc_target_format))

                try:
                    with xr.open_dataset(zero_hour_file) as ds_zero_hour:
                        if 'GEOP_ML' in ds_zero_hour.variables:
                            ds_meteo['GEOP_ML'] = ds_zero_hour['GEOP_ML'].load(
                            )
                            logging.info(
                                f"Copied 'GEOP_ML' from {zero_hour_file} to {meteo_file}."
                            )
                        else:
                            logging.error(
                                f"'GEOP_ML' not found in {zero_hour_file}.")
                except FileNotFoundError:
                    logging.error(
                        f"00:00 UTC file {zero_hour_file} not found.")
                except Exception as e:
                    logging.error(
                        f"Failed to process 00:00 UTC file {zero_hour_file}: {e}"
                    )

            # Replace "PS" from CAM-Chem by ERA5 value (as for the IC)
            if 'LNPS' in ds_meteo:
                ds_chem["PS"] = np.exp(ds_meteo["LNPS"])

            ds_merged = xr.merge([ds_meteo, ds_chem], compat="override")
            ds_merged.to_netcdf(tmp_file)
        tools.remove_file(meteo_file)
        for f in chem_files:
            tools.remove_file(f)
        os.replace(tmp_file, merged_file)
        logging.info("Added MECCA tracers to file {}".format(merged_file))

    ## ------------------------------------------------------
    ## -- Add Q (copy of QV) and PS=(exp(LNPS)) to IC file --
    ## ------------------------------------------------------

    logging.info('Add Q (copy of QV) and PS (exp(LNPS)) to initial file')
    ic_file = os.path.join(
        cfg.icon_input_icbc,
        cfg.startdate_sim.strftime(cfg.inidata_prefix +
                                   cfg.inidata_nameformat +
                                   cfg.inidata_filename_suffix))
    if os.path.isfile(ic_file):
        merged_file = os.path.join(
            cfg.icon_input_icbc,
            cfg.startdate_sim.strftime(cfg.inidata_prefix +
                                       cfg.inidata_nameformat + '_merged' +
                                       cfg.inidata_filename_suffix))
        ds = xr.open_dataset(ic_file)
        merging = False
        if "PS" not in ds:
            merging = True
            ds["PS"] = np.exp(ds["LNPS"])
            for var in ds.data_vars:
                ds[var].encoding = {}
                logging.info(f"Added PS to file {ic_file}")
            logging.info(f"Added PS to file {ic_file}")
        if 'Q' not in ds:
            merging = True
            ds['Q'] = ds['QV']
            # -- Bug fix for:
            # "Variable has conflicting _FillValue (nan) and missing_value (-9e+33)"
            for var in ds.data_vars:
                ds[var].encoding = {}
            logging.info(f"Added Q to file {ic_file}")
        # add surface pressure compatible with ERA5 data
        if merging:
            ds.to_netcdf(merged_file)
            tools.rename_file(merged_file, ic_file)

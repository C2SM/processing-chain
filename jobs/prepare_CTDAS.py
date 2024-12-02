#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import shutil
import subprocess
from . import tools, prepare_icon
from .tools.fetch_external_data import fetch_era5,  fetch_CAMS_CO2, fetch_ICOS_data, fetch_OCO2_data, process_ICOS_data, process_OCO2_data
from concurrent.futures import ThreadPoolExecutor, as_completed

BASIC_PYTHON_JOB = False


def main(cfg):
    """
    Prepare CTDAS inversion

    This does the following steps:
    1. Download ERA-5 data for the chosen dates
    2. Download CAMS data for the chosen dates
    3. Interpolate CAMS data to the ERA-5 location (horizontally and vertically)
    4. Download ICOS station data for the chosen dates
    5. Download OCO-2 data for the chosen dates
    6. Prepare the folder output structure

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    print(cfg.print_config())
    tools.change_logfile(cfg.logfile)
    logging.info("Prepare ICON-ART for CTDAS")

    # -- 1. Download CAMS CO2 data (for a whole year)
    if cfg.chem_fetch_CAMS:
        fetch_CAMS_CO2(
            cfg.startdate_sim, cfg.icon_input_icbc
        )  # This should be turned into a more central location I think.

    # -- 2. Fetch *all* ERA5 data (not just for initial conditions)
    if cfg.meteo_fetch_era5:
        times = list(tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim, cfg.meteo_nudging_step))
        logging.info(f"Time range considered here: {times}")

        # Split downloads in 3-day chunks, but run simultaneously
        N = 3
        chunks = list(tools.split_into_chunks(times, N, cfg.meteo_nudging_step))
        logging.info(f"Time range split up into chunks of {N} days, giving the following chunks: {chunks}")

        # Run fetch_era5 in parallel over chunks
        output_filenames = [None] * len(chunks)  # Create a list to store filenames in order
        with ThreadPoolExecutor(max_workers=4) as executor:
            futures = {executor.submit(fetch_era5, chunk, cfg.icon_input_icbc, resolution=0.25, area=[60, -15, 35, 20]): i for i, chunk in enumerate(chunks)}
            for future in futures:
                index = futures[future]  # Get the index of the future
                try:
                    result = future.result()  # Get the result from the future
                    output_filenames[index] = result  # Store the returned filename(s) in the correct order
                    logging.info(f"Fetched data and saved to: {result}")
                except Exception as exc:
                    logging.error(f"Generated an exception: {exc}")
        logging.info(f"All fetched files: {output_filenames}")

        # Split files (with multiple days/times) into individual files using bash script
        era5_split_template = cfg.case_path / cfg.meteo_era5_splitjob
        era5_split_job = cfg.icon_input_icbc / cfg.meteo_era5_splitjob
        logging.info(f"Preparing ERA5 splitting script for ICON from {era5_split_template}")
        ml_files = " ".join([f"{filenames[0]}" for filenames in output_filenames])
        surf_files = " ".join([f"{filenames[1]}" for filenames in output_filenames])
        with open(era5_split_template, 'r') as infile, open(era5_split_job, 'w') as outfile:
            outfile.write(infile.read().format(
                cfg=cfg,
                ml_files=ml_files,
                surf_files=surf_files
            ))
        logging.info(f"Running ERA5 splitting script {era5_split_job}")
        subprocess.run(["bash", era5_split_job], check=True, stdout=subprocess.PIPE)

    # -- 3. Process initial conditions data using bash script
    datestr = cfg.startdate_sim.strftime("%Y-%m-%dT%H:%M:%S")
    era5_ml_file = cfg.icon_input_icbc / f"era5_ml_{datestr}.nc"
    era5_surf_file = cfg.icon_input_icbc / f"era5_surf_{datestr}.nc"
    era5_ini_file = cfg.icon_input_icbc / f"era5_ini_{datestr}.nc"
    era5_ini_template = cfg.case_path / cfg.meteo_era5_inijob
    era5_ini_job = cfg.icon_input_icbc / cfg.meteo_era5_inijob
    with open(era5_ini_template, 'r') as infile, open(era5_ini_job,
                                                      'w') as outfile:
        outfile.write(infile.read().format(cfg=cfg,
                                           era5_ml_file=era5_ml_file,
                                           era5_surf_file=era5_surf_file,
                                           inicond_filename=era5_ini_file))
    shutil.copy(cfg.case_path / 'mypartab', cfg.icon_input_icbc / 'mypartab')
    logging.info(f"Running ERA5 initial conditions script {era5_ini_job}")
    subprocess.run(["bash", era5_ini_job], check=True, stdout=subprocess.PIPE)
    # --- CAMS inicond
    logging.info("Preparing CAMS preprocessing script for ICON")
    cams_ini_template = cfg.case_path / cfg.chem_cams_inijob
    cams_ini_job = cfg.icon_input_icbc / cfg.chem_cams_inijob
    with open(cams_ini_template, 'r') as infile, open(cams_ini_job,
                                                      'w') as outfile:
        outfile.write(infile.read().format(cfg=cfg,
                                           inicond_filename=era5_ini_file))
    logging.info("Running CAMS preprocessing initial conditions script")
    subprocess.run(["bash", cams_ini_job], check=True, stdout=subprocess.PIPE)

    # -- 3. Interpolate CAMS to ERA5 3D grid
    if cfg.meteo_interpolate_CAMS_to_ERA5:
        for time in tools.iter_hours(cfg.startdate_sim,
                                     cfg.enddate_sim,
                                     step=cfg.meteo_nudging_step):

            # -- Give a name to the nudging file
            datestr = time.strftime("%Y-%m-%dT%H:%M:%S")
            era5_ml_file = cfg.icon_input_icbc / f"era5_ml_{datestr}.nc"
            era5_surf_file = cfg.icon_input_icbc / f"era5_surf_{datestr}.nc"
            era5_nudge_file = cfg.icon_input_icbc / f"era5_nudge_{datestr}.nc"

            # -- Copy ERA5 processing script (icon_era5_nudging.job) in workdir
            nudging_template = cfg.case_path / cfg.meteo_era5_nudgingjob
            nudging_job = cfg.icon_input_icbc / f'icon_era5_nudging_{datestr}.sh'
            with open(nudging_template, 'r') as infile, open(nudging_job,
                                                             'w') as outfile:
                outfile.write(infile.read().format(cfg=cfg,
                                                   era5_ml_file=era5_ml_file,
                                                   era5_surf_file=era5_surf_file,
                                                   filename=era5_nudge_file))

            # -- Copy mypartab in workdir
            if not os.path.exists(cfg.case_path / 'mypartab'):
                shutil.copy(cfg.case_path / 'mypartab',
                            cfg.icon_input_icbc / 'mypartab')

            # -- Run ERA5 processing script
            subprocess.run(["bash", nudging_job],
                           check=True,
                           stdout=subprocess.PIPE)

            # -- Copy CAMS processing script (icon_cams_nudging.job) in workdir
            logging.info("Preparing CAMS preprocessing nudging script for ICON")
            cams_nudge_template = cfg.case_path / cfg.chem_cams_nudgingjob
            cams_nudge_job = cfg.icon_input_icbc / cfg.chem_cams_nudgingjob
            with open(cams_nudge_template, 'r') as infile, open(cams_nudge_job,
                                                            'w') as outfile:
                outfile.write(infile.read().format(cfg=cfg,
                                                   filename=era5_nudge_file))
            subprocess.run(["bash", cams_nudge_job], check=True, stdout=subprocess.PIPE)

    # -- 4. Download ICOS CO2 data
    if cfg.obs_fetch_ICOS:
        fetch_ICOS_data(cookie_token=cfg.obs_ICOS_cookie_token,
                        start_date=cfg.startdate_sim.strftime("%d-%m-%Y"),
                        end_date=cfg.enddate_sim.strftime("%d-%m-%Y"),
                        save_path=cfg.obs_ICOS_path,
                        species=[
                            'co2',
                        ])
        tools.create_dir(cfg.case_root / "global_inputs" / "ICOS", "ICOS input files")
        process_ICOS_data(ICOS_obs_folder=cfg.obs_ICOS_path,
                          start_date=cfg.startdate_sim,
                          end_date=cfg.enddate_sim,
                          output_folder=cfg.case_root / "global_inputs" / "ICOS"
        )

    if cfg.obs_fetch_OCO2:
        # A user must do the following steps to obtain access to OCO2 data
        # from getpass import getpass
        # import os
        # from subprocess import Popen
        # urs = 'urs.earthdata.nasa.gov'    # Earthdata URL to call for authentication
        # prompts = ['Enter NASA Earthdata Login Username \n(or create an account at urs.earthdata.nasa.gov): ',
        #         'Enter NASA Earthdata Login Password: ']
        # homeDir = os.path.expanduser("~") + os.sep
        # with open(homeDir + '.netrc', 'w') as file:
        #     file.write('machine {} login {} password {}'.format(urs, getpass(prompt=prompts[0]), getpass(prompt=prompts[1])))
        #     file.close()
        # with open(homeDir + '.urs_cookies', 'w') as file:
        #     file.write('')
        #     file.close()
        # with open(homeDir + '.dodsrc', 'w') as file:
        #     file.write('HTTP.COOKIEJAR={}.urs_cookies\n'.format(homeDir))
        #     file.write('HTTP.NETRC={}.netrc'.format(homeDir))
        #     file.close()
        # Popen('chmod og-rw ~/.netrc', shell=True)
        fetch_OCO2_data(cfg.startdate_sim,
                   cfg.enddate_sim,
                   -8,
                   30,
                   35,
                   65,
                   cfg.obs_OCO2_path,
                   product="OCO2_L2_Lite_FP_11.1r")
        tools.create_dir(cfg.case_root / "global_inputs" / "OCO2", "OCO-2 output")
        process_OCO2_data(OCO2_obs_folder=cfg.obs_OCO2_path,
                          start_date=cfg.startdate_sim,
                          end_date=cfg.enddate_sim,
                          output_folder=cfg.case_root / "global_inputs" / "OCO2")  # post-process all the OCO2 data
    logging.info("OK")

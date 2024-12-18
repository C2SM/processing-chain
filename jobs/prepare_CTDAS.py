#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import shutil
import subprocess
from . import tools, prepare_icon
from .tools.fetch_external_data import fetch_era5,  fetch_CAMS_CO2, fetch_ICOS_data, fetch_OCO2_data, process_ICOS_data, process_OCO2_data
from .tools.ctdas_utilities import create_lambda_regions, create_prior_all_ones, create_boundary_regions, create_boundary_prior_all_ones
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import timedelta

BASIC_PYTHON_JOB = False


def main(cfg):
    """
    Prepare CTDAS inversion

    This does the following steps:
    1. Download CAMS data for the full year (only granularity possible)
    2. Download ERA-5 for this date range
    3. Run initial condition script (for CAMS and ERA-5)
    4. Run boundary condition script (for CAMS and ERA-5)
    5. Download ICOS station data for the chosen dates
    6. Download OCO-2 data for the chosen dates
    7. Prepare the folder output structure
    8. Run the first one-day simulation
    9. Patch the CTDAS directory with files of our own

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
        CAMS_folder = cfg.case_root / "global_inputs" / "CAMS"
        tools.create_dir(CAMS_folder, "CAMS input files")
        fetch_CAMS_CO2(
            cfg.startdate_sim, (cfg.enddate_sim+timedelta(days=1)), CAMS_folder
        )

    # -- 2. Fetch *all* ERA5 data (not just for initial conditions)
    if cfg.meteo_fetch_era5:
        ERA5_folder = cfg.case_root / "global_inputs" / "ERA5"
        tools.create_dir(ERA5_folder, "CAMS input files")

        times = list(tools.iter_hours(cfg.startdate_sim, (cfg.enddate_sim+timedelta(days=1)), cfg.meteo_nudging_step))
        logging.info(f"Time range considered here: {times}")

        file_list = [f"era5_ml_{(cfg.startdate_sim + timedelta(hours=i)).replace(tzinfo=None).isoformat()}.nc"
             for i in range(0, int((cfg.enddate_sim - cfg.startdate_sim).total_seconds() // 3600) + 1, cfg.meteo_nudging_step)]
        file_list+= [f"era5_surf_{(cfg.startdate_sim + timedelta(hours=i)).replace(tzinfo=None).isoformat()}.nc"
             for i in range(0, int((cfg.enddate_sim - cfg.startdate_sim).total_seconds() // 3600) + 1, cfg.meteo_nudging_step)]
        missing_files = [file for file in file_list if not (ERA5_folder / file).exists()]
        if not missing_files:
            logging.info("All model level files already present")
        else:
            logging.info(f"Missing files: {missing_files}")
            # Split downloads in 3-day chunks, but run simultaneously
            N = 3
            chunks = list(tools.split_into_chunks(times, N, cfg.meteo_nudging_step))
            logging.info(f"Time range split up into chunks of {N} days, giving the following chunks: {chunks}")

            # Run fetch_era5 in parallel over chunks
            output_filenames = [None] * len(chunks)  # Create a list to store filenames in order
            with ThreadPoolExecutor(max_workers=4) as executor:
                futures = {executor.submit(fetch_era5, chunk, ERA5_folder, resolution=0.25, area=[60, -15, 35, 20]): i for i, chunk in enumerate(chunks)}
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
            era5_split_job = ERA5_folder / (cfg.meteo_era5_splitjob.stem + f'{cfg.startdate_sim.strftime("%Y%m%d")}' + cfg.meteo_era5_splitjob.suffix)
            logging.info(f"Preparing ERA5 splitting script for ICON from {era5_split_template}")
            ml_files = " ".join([f"{filenames[0]}" for filenames in output_filenames])
            surf_files = " ".join([f"{filenames[1]}" for filenames in output_filenames])
            with open(era5_split_template, 'r') as infile, open(era5_split_job, 'w') as outfile:
                outfile.write(infile.read().format(
                    cfg=cfg,
                    ml_files=ml_files,
                    surf_files=surf_files,
                    ERA5_folder=ERA5_folder
                ))
            logging.info(f"Running ERA5 splitting script {era5_split_job}")
            subprocess.run(["bash", era5_split_job], check=True, stdout=subprocess.PIPE)

    # -- 3. Process initial conditions data using bash script
    datestr = cfg.startdate_sim.strftime("%Y-%m-%dT%H:%M:%S")
    era5_ini_file = cfg.icon_input_icbc / f"era5_ini_{datestr}.nc"
    if not era5_ini_file.is_file():
        logging.info("Preparing ERA5 initial conditions script for ICON")
        era5_ml_file = ERA5_folder / f"era5_ml_{datestr}.nc"
        era5_surf_file = ERA5_folder / f"era5_surf_{datestr}.nc"
        era5_ini_template = cfg.case_path / cfg.meteo_era5_inijob
        era5_ini_job = ERA5_folder / (era5_ini_template.stem + f'{cfg.startdate_sim.strftime("%Y%m%d")}' + era5_ini_template.suffix)
        with open(era5_ini_template, 'r') as infile, open(era5_ini_job,
                                                        'w') as outfile:
            outfile.write(infile.read().format(cfg=cfg,
                                            era5_ml_file=era5_ml_file,
                                            era5_surf_file=era5_surf_file,
                                            inicond_filename=era5_ini_file,
                                            ERA5_folder=ERA5_folder))
        shutil.copy(cfg.case_path / cfg.meteo_partab, ERA5_folder / 'mypartab')
        logging.info(f"Running ERA5 initial conditions script {era5_ini_job}")
        subprocess.run(["bash", era5_ini_job], check=True, stdout=subprocess.PIPE)
        # --- CAMS inicond
        logging.info("Preparing CAMS initial conditions script for ICON")
        cams_ini_template = cfg.case_path / cfg.chem_cams_inijob
        cams_ini_job = ERA5_folder / (cams_ini_template.stem + f'{cfg.startdate_sim.strftime("%Y%m%d")}' + cams_ini_template.suffix)
        with open(cams_ini_template, 'r') as infile, open(cams_ini_job,
                                                        'w') as outfile:
            outfile.write(infile.read().format(cfg=cfg,
                                            inicond_filename=era5_ini_file,
                                            ERA5_folder=ERA5_folder,
                                            CAMS_file=CAMS_folder / f'cams_egg4_{cfg.startdate_sim.strftime("%Y%m%d%H")}.nc',
                                            era5_cams_ini_file=era5_ini_file))
        logging.info(f"Running CAMS initial conditions script {cams_ini_job}")
        subprocess.run(["bash", cams_ini_job], check=True, stdout=subprocess.PIPE)

    # -- 4. Interpolate CAMS to ERA5 3D grid
    if cfg.meteo_interpolate_CAMS_to_ERA5:
        for time in tools.iter_hours(cfg.startdate_sim,
                                     (cfg.enddate_sim+timedelta(days=1)),
                                     step=cfg.meteo_nudging_step):

            # -- Give a name to the nudging file
            datestr = time.strftime("%Y-%m-%dT%H:%M:%S")
            datestr2= time.strftime("%Y%m%d%H")
            era5_nudge_file_final = cfg.icon_input_icbc / f"era5_nudge_{datestr2}.nc"
            if not era5_nudge_file_final.exists():
                era5_ml_file = ERA5_folder / f"era5_ml_{datestr}.nc"
                era5_surf_file = ERA5_folder / f"era5_surf_{datestr}.nc"
                era5_nudge_file = ERA5_folder / f"era5_nudge_{datestr}.nc"

                # -- Copy ERA5 processing script (icon_era5_nudging.job) in workdir
                nudging_template = cfg.case_path / cfg.meteo_era5_nudgingjob
                nudging_job = ERA5_folder / f'icon_era5_nudging_{datestr}.sh'
                with open(nudging_template, 'r') as infile, open(nudging_job,
                                                                'w') as outfile:
                    outfile.write(infile.read().format(cfg=cfg,
                                                    era5_ml_file=era5_ml_file,
                                                    era5_surf_file=era5_surf_file,
                                                    filename=era5_nudge_file,
                                                    ERA5_folder=ERA5_folder))

                # -- Copy mypartab into workdir
                if not os.path.exists(ERA5_folder / 'mypartab'):
                    shutil.copy(cfg.case_path / cfg.meteo_partab,
                                ERA5_folder / 'mypartab')

                # -- Run ERA5 processing script
                subprocess.run(["bash", nudging_job],
                            check=True,
                            stdout=subprocess.PIPE)

                # -- Copy CAMS processing script (icon_cams_nudging.job) into workdir
                logging.info("Preparing CAMS preprocessing nudging script for ICON")
                cams_nudge_template = cfg.case_path / cfg.chem_cams_nudgingjob
                cams_nudge_job = ERA5_folder / (cams_nudge_template.stem + f'{cfg.startdate_sim.strftime("%Y%m%d")}' + cams_nudge_template.suffix)
                with open(cams_nudge_template, 'r') as infile, open(cams_nudge_job,
                                                                'w') as outfile:
                    outfile.write(infile.read().format(cfg=cfg,
                                                    filename=era5_nudge_file,
                                                    ERA5_folder=ERA5_folder,
                                                    CAMS_file=CAMS_folder / f'cams_egg4_{time.strftime("%Y%m%d%H")}.nc',
                                                    era5_cams_nudge_file=era5_nudge_file_final,
                                                    ))
                subprocess.run(["bash", cams_nudge_job], check=True, stdout=subprocess.PIPE)

    # -- 5. Download ICOS CO2 data
        # Lots of potential for 'dehardcoding' things here, but that has to be done with
        # a lot of care.
    if cfg.CTDAS_obs_fetch_ICOS:
        fetch_ICOS_data(cookie_token=cfg.CTDAS_obs_ICOS_cookie_token,
                        start_date=cfg.startdate_sim.strftime("%d-%m-%Y"),
                        end_date=(cfg.enddate_sim+timedelta(days=1)).strftime("%d-%m-%Y"),
                        save_path=cfg.CTDAS_obs_ICOS_path,
                        species=[
                            'co2',
                        ])
        tools.create_dir(cfg.case_root / "global_inputs" / "ICOS", "ICOS input files")
        process_ICOS_data(ICOS_obs_folder=cfg.CTDAS_obs_ICOS_path,
                          start_date=cfg.startdate_sim,
                          end_date=(cfg.enddate_sim+timedelta(days=1)),
                          output_folder=cfg.case_root / "global_inputs" / "ICOS"
        )

    # -- 6. Download OCO2 data
    if cfg.CTDAS_obs_fetch_OCO2:
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
                   (cfg.enddate_sim+timedelta(days=1)),
                   -8,
                   30,
                   35,
                   65,
                   cfg.CTDAS_obs_OCO2_path,
                   product="OCO2_L2_Lite_FP_11.1r")
        tools.create_dir(cfg.case_root / "global_inputs" / "OCO2", "OCO-2 output")
        process_OCO2_data(OCO2_obs_folder=cfg.CTDAS_obs_OCO2_path,
                          start_date=cfg.startdate_sim,
                          end_date=(cfg.enddate_sim+timedelta(days=1)),
                          output_folder=cfg.case_root / "global_inputs" / "OCO2")  # post-process all the OCO2 data

    # -- 7. Create the required folder structure
    # For the ICON runs
    tools.create_dir(cfg.icon_base / "output_prior", "Prior")
    tools.create_dir(cfg.icon_base / "output_opt_once", "1 time optimized")
    tools.create_dir(cfg.icon_base / "output_opt_twice", "2 times optimized")

    # For the sampling
    tools.create_dir(cfg.case_root / "global_output" / "extracted_ICOS", "Output of the extraction script")

    # -- 8. Initialize the first one-day run, only for the first lag
    if cfg.startdate_sim == cfg.startdate:
        # -- 8.1 Get the global_inputs folder filled out
        logging.info('Copy global inputs to working directory')
        if cfg.machine == 'daint':
            script_lines = [
                '#!/usr/bin/env bash',
                f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
                f'#SBATCH --account={cfg.compute_account}',
                '#SBATCH --time=00:10:00',
                f'#SBATCH --partition={cfg.compute_queue}',
                f'#SBATCH --constraint={cfg.constraint}', '#SBATCH --nodes=1',
                f'#SBATCH --output={cfg.logfile}', '#SBATCH --open-mode=append',
                f'#SBATCH --chdir={cfg.icon_work}', ''
            ]
        elif cfg.machine == 'euler':
            script_lines = [
                '#!/usr/bin/env bash',
                f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
                '#SBATCH --time=00:10:00',
                f'#SBATCH --partition={cfg.compute_queue}',
                f'#SBATCH --constraint={cfg.constraint}', '#SBATCH --ntasks=1',
                f'#SBATCH --output={cfg.logfile}', '#SBATCH --open-mode=append',
                f'#SBATCH --chdir={cfg.icon_work}', ''
            ]
        for category in cfg.CTDAS_global_inputs:
            tools.create_dir(cat_folder := cfg.case_root / "global_inputs" / category, category)
            for file in category:
                source = (p := Path(file))
                destination = cat_folder / p.name
                script_lines.append(f'rsync -av {source} {destination}')
        with (script := cfg.icon_work / 'copy_global_inputs.job').open('w') as f:
            f.write('\n'.join(script_lines))    
            cfg.submit('global_inputs', script)

        # -- 8.2 Create the ensemble data for the first day
        tools.create_dir(OEM_folder := cfg.case_root / "global_inputs" / "OEM", "OEM")
        lambdas = [int(item) for line in cfg.CTDAS_lambdas for item in line.split(',')]
        if cfg.CTDAS_regions == 'basegrid':
            nregs, ncats = create_lambda_regions(cfg.input_files_dynamics_grid_filename, OEM_folder / "lambdaregions.nc", lambdas)
            create_prior_all_ones(OEM_folder / "prior_all_ones.nc", nensembles=cfg.CTDAS_nensembles, ncats=lambdas.max(), nregs=nregs)
        else:
            raise NotImplementedError('Only basegrid is implemented for now')
        create_boundary_regions('/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc', '/scratch/snx3000/ekoene/boundary_mask_bg.nc')
        create_boundary_prior_all_ones('/scratch/snx3000/ekoene/boundary_lambdas_bg.nc', nensembles=cfg.CTDAS_nensembles)

        # Create a folder an `nlag` period earlier / icon / output_opt_twice

        # then initialize the runscript file

        # era5_split_template = cfg.case_path / cfg.firstrunscript
        # era5_split_job =  / cfg.meteo_era5_splitjob
        # era5_split_job = era5_split_job.parent / (era5_split_job.stem + f'{cfg.startdate_sim.strftime("%Y%m%d")}' + era5_split_job.suffix)
        # logging.info(f"Preparing ERA5 splitting script for ICON from {era5_split_template}")
        # ml_files = " ".join([f"{filenames[0]}" for filenames in output_filenames])
        # surf_files = " ".join([f"{filenames[1]}" for filenames in output_filenames])
        # with open(era5_split_template, 'r') as infile, open(era5_split_job, 'w') as outfile:
        #     outfile.write(infile.read().format(
        #         cfg=cfg,
        #         ml_files=ml_files,
        #         surf_files=surf_files,
        #         ERA5_folder=ERA5_folder
        #     ))
        # logging.info(f"Running ERA5 splitting script {era5_split_job}")
        # subprocess.run(["bash", era5_split_job], check=True, stdout=subprocess.PIPE)


    logging.info("OK")
    shutil.copy(cfg.logfile, cfg.logfile_finish)

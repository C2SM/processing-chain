#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import xarray as xr
import shutil
import subprocess
from . import tools, prepare_icon
from .tools.generate_tracers_xml import generate_tracers_xml
from .tools.fetch_external_data import fetch_era5, fetch_CAMS_CO2, fetch_ICOS_data, fetch_OCO2_data, process_ICOS_data, process_OCO2_data
from .tools.ctdas_utilities import create_lambda_regions, create_prior_all_ones, create_prior_all_zeros, create_boundary_regions, create_boundary_prior_all_ones, create_boundary_prior_separate
from concurrent.futures import ThreadPoolExecutor
from datetime import timedelta
from pathlib import Path
from subprocess import run
import re

BASIC_PYTHON_JOB = False


def run_bash_script(template, job, **kwargs):
    with job.open('w') as outfile:
        outfile.write(template.read_text().format(**kwargs))
    subprocess.run(["bash", job], check=True, stdout=subprocess.PIPE)


def era5_splitting_script(cfg, ERA5_folder, output_filenames):
    era5_split_template = cfg.case_path / cfg.meteo_era5_splitjob
    era5_split_job = ERA5_folder / (era5_split_template.stem +
                                    f'{cfg.startdate_sim.strftime("%Y%m%d")}' +
                                    era5_split_template.suffix)
    logging.info(
        f"Preparing ERA5 splitting script for ICON from {era5_split_template}")
    ml_files = " ".join([f"{filenames[0]}" for filenames in output_filenames])
    surf_files = " ".join(
        [f"{filenames[1]}" for filenames in output_filenames])
    run_bash_script(era5_split_template,
                    era5_split_job,
                    cfg=cfg,
                    ml_files=ml_files,
                    surf_files=surf_files,
                    ERA5_folder=ERA5_folder)


def initial_conditions_script(cfg, ERA5_folder, CAMS_folder, era5_ini_file):
    datestr = cfg.startdate_sim.strftime("%Y-%m-%dT%H:%M:%S")
    era5_ml_file = ERA5_folder / f"era5_ml_{datestr}.nc"
    era5_surf_file = ERA5_folder / f"era5_surf_{datestr}.nc"
    era5_ini_template = cfg.case_path / cfg.meteo_era5_inijob
    era5_ini_job = ERA5_folder / (era5_ini_template.stem +
                                  f'{cfg.startdate_sim.strftime("%Y%m%d")}' +
                                  era5_ini_template.suffix)
    run_bash_script(era5_ini_template,
                    era5_ini_job,
                    cfg=cfg,
                    era5_ml_file=era5_ml_file,
                    era5_surf_file=era5_surf_file,
                    inicond_filename=era5_ini_file,
                    ERA5_folder=ERA5_folder)
    shutil.copy(cfg.case_path / cfg.meteo_partab, ERA5_folder / 'mypartab')
    logging.info(f"Running ERA5 initial conditions script {era5_ini_job}")

    cams_ini_template = cfg.case_path / cfg.chem_cams_inijob
    cams_ini_job = ERA5_folder / (cams_ini_template.stem +
                                  f'{cfg.startdate_sim.strftime("%Y%m%d")}' +
                                  cams_ini_template.suffix)
    run_bash_script(cams_ini_template,
                    cams_ini_job,
                    cfg=cfg,
                    inicond_filename=era5_ini_file,
                    ERA5_folder=ERA5_folder,
                    CAMS_file=CAMS_folder /
                    f'cams_egg4_{cfg.startdate_sim.strftime("%Y%m%dT%H")}.nc',
                    era5_cams_ini_file=era5_ini_file)
    logging.info(f"Running CAMS initial conditions script {cams_ini_job}")


def boundary_conditions_script(cfg, ERA5_folder, CAMS_folder, time):
    datestr = time.strftime("%Y-%m-%dT%H:%M:%S")
    datestr2 = time.strftime("%Y%m%d%H")
    era5_nudge_file_final = cfg.icon_input_icbc / f"era5_nudge_{datestr2}.nc"
    if not era5_nudge_file_final.exists():
        era5_ml_file = ERA5_folder / f"era5_ml_{datestr}.nc"
        era5_surf_file = ERA5_folder / f"era5_surf_{datestr}.nc"
        era5_nudge_file = ERA5_folder / f"era5_nudge_{datestr}.nc"

        nudging_template = cfg.case_path / cfg.meteo_era5_nudgingjob
        nudging_job = ERA5_folder / f'icon_era5_nudging_{datestr}.sh'
        run_bash_script(nudging_template,
                        nudging_job,
                        cfg=cfg,
                        era5_ml_file=era5_ml_file,
                        era5_surf_file=era5_surf_file,
                        filename=era5_nudge_file,
                        ERA5_folder=ERA5_folder)

        if not os.path.exists(ERA5_folder / 'mypartab'):
            shutil.copy(cfg.case_path / cfg.meteo_partab,
                        ERA5_folder / 'mypartab')

        cams_nudge_template = cfg.case_path / cfg.chem_cams_nudgingjob
        cams_nudge_job = ERA5_folder / (
            cams_nudge_template.stem +
            f'{cfg.startdate_sim.strftime("%Y%m%d")}' +
            cams_nudge_template.suffix)
        run_bash_script(cams_nudge_template,
                        cams_nudge_job,
                        cfg=cfg,
                        filename=era5_nudge_file,
                        ERA5_folder=ERA5_folder,
                        CAMS_file=CAMS_folder /
                        f'cams_egg4_{time.strftime("%Y%m%dT%H")}.nc',
                        era5_cams_nudge_file=era5_nudge_file_final)
        logging.info(f"Running CAMS nudging script {cams_nudge_job}")


def create_icon_job(cfg, run_type, firstrun=False, runthrough=False):
    """Generate ICON script dynamically."""
    OEM_folder = cfg.case_root / "global_inputs" / "OEM"

    if firstrun:
        tracers_xml = cfg.case_root / "global_inputs" / "XML" / "tracers_firstrun.xml"
        ini_restart_end_string = f"{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%SZ')}"
        output_directory = cfg.case_root / "global_outputs" / f"{run_type}_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}"
        lambda_nc = OEM_folder / f"prior_all_ones.nc"
        bg_lambda_nc = OEM_folder / f"boundary_lambdas_bg.nc"
        output_init = cfg.CTDAS_restart_init_time
        restart_file = cfg.case_root / "global_outputs" / f"opt2_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"
    else:
        tracers_xml = cfg.case_root / "global_inputs" / "XML" / "tracers_restart.xml"
        ini_restart_end_string = f"{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time) + timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y-%m-%dT%H:%M:%SZ')}"
        output_directory = cfg.case_root / "global_outputs" / f"{run_type}_{cfg.startdate_sim.strftime('%Y%m%d')}"
        lambda_nc = OEM_folder / f"lambda_{cfg.startdate_sim.strftime('%Y%m%d')}_{run_type}.nc"
        bg_lambda_nc = OEM_folder / f"bg_lambda_{cfg.startdate_sim.strftime('%Y%m%d')}_{run_type}.nc"
        output_init = 24 * 60 * 60 * cfg.CTDAS_ctdas_cycle + cfg.CTDAS_restart_init_time
        restart_job = "opt1" if run_type == "prior" else "opt2"
        restart_file = cfg.case_root / "global_outputs" / f"{restart_job}_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"

    if runthrough:
        if firstrun:
            tracers_xml = cfg.case_root / "global_inputs" / "XML" / "tracers_runthrough_firstrun.xml"
            ini_restart_end_string = f"{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%SZ')}"
            output_directory = cfg.case_root / "global_outputs" / f"runthrough_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}"
            lambda_nc = OEM_folder / f"prior_all_zeros.nc"
            bg_lambda_nc = OEM_folder / f"boundary_lambdas_separate.nc"
            output_init = cfg.CTDAS_restart_init_time
            restart_file = cfg.case_root / "global_outputs" / f"runthrough_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"
        else:
            tracers_xml = cfg.case_root / "global_inputs" / "XML" / "tracers_runthrough_restart.xml"
            ini_restart_end_string = f"{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time) + timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y-%m-%dT%H:%M:%SZ')}"
            output_directory = cfg.case_root / "global_outputs" / f"runthrough_{cfg.startdate_sim.strftime('%Y%m%d')}"
            lambda_nc = OEM_folder / f"prior_all_zeros.nc"
            bg_lambda_nc = OEM_folder / f"boundary_lambdas_separate.nc"
            output_init = 24 * 60 * 60 * cfg.CTDAS_ctdas_cycle + cfg.CTDAS_restart_init_time
            restart_file = cfg.case_root / "global_outputs" / f"runthrough_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"

    tools.create_dir(output_directory, f"Create {run_type} output")

    script_content = (
        fn := cfg.case_path / cfg.icon_runjob_filename
    ).read_text().format(
        cfg=cfg,
        ini_restart_string=cfg.startdate_sim.strftime('%Y-%m-%dT%H:%M:%SZ'),
        ini_restart_end_string=ini_restart_end_string,
        inifile_nc=cfg.icon_input_icbc /
        f"era5_ini_{cfg.startdate_sim.strftime('%Y-%m-%dT%H:%M:%S')}.nc",
        tracers_xml=tracers_xml,
        emissionsgrid_nc=cfg.case_root / "global_inputs" / "inventories" /
        f"INV_{(cfg.startdate_sim + timedelta(days=1)).strftime('%Y%m%d')}.nc",
        vertical_profile_nc=OEM_folder / "vertical_profiles.nc",
        hour_of_year_nc=OEM_folder / "hourofyear.nc",
        lambda_nc=lambda_nc,
        lambda_regions_nc=OEM_folder / "lambdaregions.nc",
        bg_lambda_nc=bg_lambda_nc,
        bg_lambda_regions_nc=OEM_folder / "boundary_mask_bg.nc",
        vprm_coeffs_nc=cfg.case_root / "global_inputs" / "VPRM" /
        cfg.CTDAS_global_inputs_VPRM[0].split('/')[-1],
        latbc_boundary_grid_nc=cfg.case_root / "global_inputs" / "grid" /
        "lateral_boundary.grid.nc",
        output_directory=output_directory,
        restart_file=restart_file,
        restart_init_time=cfg.CTDAS_restart_init_time,
        output_init=output_init)

    script_path = cfg.icon_work / f"{fn.stem}_{cfg.startdate_sim.strftime('%Y%m%d')}{'_' + run_type if not firstrun else ''}{'_firstrun_runthrough' if firstrun and runthrough else ''}{fn.suffix}"
    with script_path.open('w') as outfile:
        outfile.write(script_content)
    logging.info(f"Preparing ICON script for {run_type} run at {script_path}")


def create_slurm_script(cfg):
    """Generate SLURM script based on machine type."""
    base_lines = [
        '#!/usr/bin/env bash',
        f'#SBATCH --job-name="copy_input_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}"',
        '#SBATCH --time=00:10:00', f'#SBATCH --partition={cfg.compute_queue}',
        f'#SBATCH --constraint={cfg.constraint}',
        f'#SBATCH --output={cfg.logfile}', '#SBATCH --open-mode=append',
        f'#SBATCH --chdir={cfg.case_root / "global_inputs"}', ''
    ]

    machine_specific = {
        'daint':
        [f'#SBATCH --account={cfg.compute_account}', '#SBATCH --nodes=1'],
        'euler': ['#SBATCH --ntasks=1'],
        'santis':
        ['#SBATCH --nodes=1', f'#SBATCH --account={cfg.compute_account}']
    }

    return base_lines + machine_specific.get(cfg.machine, [])


def copy_global_inputs(cfg):
    """Handle copying of global input files."""
    script_lines = create_slurm_script(cfg)

    for attr in dir(cfg):
        if attr.startswith('CTDAS_global_inputs_'):
            category = attr[len('CTDAS_global_inputs_'):]
            cat_folder = cfg.case_root / "global_inputs" / category
            tools.create_dir(cat_folder, category)

            for file in getattr(cfg, attr):
                source = Path(file)
                destination = cat_folder / source.name
                script_lines.append(f'rsync -av {source} {destination}')

    script_path = cfg.case_root / "global_inputs" / 'copy_global_inputs.job'
    with script_path.open('w') as f:
        f.write('\n'.join(script_lines))
    cfg.submit('global_inputs', script_path)


def generate_tracers(cfg):
    """Generate tracers XML files."""
    tools.create_dir(xml_folder := cfg.case_root / "global_inputs" / "XML",
                     "XML")
    TR_prior = generate_tracers_xml(cfg.tracers,
                                    cfg.CTDAS_nensembles,
                                    restart=False,
                                    propagate_bg=cfg.CTDAS_propagate_bg)
    TR_restart = generate_tracers_xml(cfg.tracers,
                                      cfg.CTDAS_nensembles,
                                      restart=True,
                                      propagate_bg=cfg.CTDAS_propagate_bg)
    with open(xml_folder / "tracers_firstrun.xml", "w",
              encoding="utf-8") as file:
        file.write(TR_prior)
    with open(xml_folder / "tracers_restart.xml", "w",
              encoding="utf-8") as file:
        file.write(TR_restart)
    if cfg.CTDAS_runthrough:
        TR_runthrough_prior = generate_tracers_xml(cfg.tracers,
                                                   cfg.CTDAS_nensembles,
                                                   cfg.CTDAS_nboundaries,
                                                   restart=False,
                                                   runthrough=True)
        TR_runthrough_restart = generate_tracers_xml(cfg.tracers,
                                                     cfg.CTDAS_nensembles,
                                                     cfg.CTDAS_nboundaries,
                                                     restart=True,
                                                     runthrough=True)
        with open(xml_folder / "tracers_runthrough_firstrun.xml",
                  "w",
                  encoding="utf-8") as file:
            file.write(TR_runthrough_prior)
        with open(xml_folder / "tracers_runthrough_restart.xml",
                  "w",
                  encoding="utf-8") as file:
            file.write(TR_runthrough_restart)


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
    8. Prepare the first one-day simulation
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

    # -- 1. Download CAMS CO2 data (for simulation period)
    if cfg.chem_fetch_CAMS:
        tools.create_dir(
            CAMS_folder := cfg.case_root / "global_inputs" / "CAMS",
            "CAMS input files")
        fetch_CAMS_CO2(cfg.startdate_sim,
                       (cfg.enddate_sim + timedelta(days=1)), CAMS_folder)

    # -- 2. Fetch ERA5 data (for simulation period)
    if cfg.meteo_fetch_era5:
        tools.create_dir(
            ERA5_folder := cfg.case_root / "global_inputs" / "ERA5",
            "ERA5 input files")
        times = list(
            tools.iter_hours(cfg.startdate_sim,
                             (cfg.enddate_sim + timedelta(days=1)),
                             cfg.meteo_nudging_step))
        logging.info(f"Time range considered here: {times}")

        file_list = [
            f"era5_ml_{(cfg.startdate_sim + timedelta(hours=i)).replace(tzinfo=None).isoformat()}.nc"
            for i in range(
                0,
                int((cfg.enddate_sim - cfg.startdate_sim).total_seconds() //
                    3600) + 1, cfg.meteo_nudging_step)
        ]
        file_list += [
            f"era5_surf_{(cfg.startdate_sim + timedelta(hours=i)).replace(tzinfo=None).isoformat()}.nc"
            for i in range(
                0,
                int((cfg.enddate_sim - cfg.startdate_sim).total_seconds() //
                    3600) + 1, cfg.meteo_nudging_step)
        ]
        missing_files = [
            file for file in file_list if not (ERA5_folder / file).exists()
        ]
        if not missing_files:
            logging.info("All model level files already present")
        else:
            logging.info(
                f"Missing files: {missing_files}. All data will be re-fetched."
            )
            # Split downloads in 3-day chunks, but run simultaneously
            N = 3
            chunks = list(
                tools.split_into_chunks(times, N, cfg.meteo_nudging_step))
            logging.info(
                f"Time range split up into chunks of {N} days, giving the following chunks: {chunks}"
            )

            # Run fetch_era5 in parallel over chunks
            output_filenames = [None] * len(
                chunks)  # Create a list to store filenames in order
            with ThreadPoolExecutor(max_workers=4) as executor:
                futures = {
                    executor.submit(fetch_era5,
                                    chunk,
                                    ERA5_folder,
                                    resolution=0.25,
                                    area=[60, -15, 35, 20]):
                    i
                    for i, chunk in enumerate(chunks)
                }
                for future in futures:
                    index = futures[future]  # Get the index of the future
                    try:
                        result = future.result(
                        )  # Get the result from the future
                        output_filenames[
                            index] = result  # Store the returned filename(s) in the correct order
                        logging.info(f"Fetched data and saved to: {result}")
                    except Exception as exc:
                        logging.error(f"Generated an exception: {exc}")
            logging.info(f"All fetched files: {output_filenames}")

            era5_splitting_script(cfg, ERA5_folder, output_filenames)

    # -- 3. Create initial conditions for ICON
    datestr = cfg.startdate_sim.strftime("%Y-%m-%dT%H:%M:%S")
    era5_ini_file = cfg.icon_input_icbc / f"era5_ini_{datestr}.nc"
    if not era5_ini_file.is_file():
        logging.info("Preparing ERA5 initial conditions script for ICON")
        initial_conditions_script(cfg, ERA5_folder, CAMS_folder, era5_ini_file)

    # -- 4. Create boundary conditions for ICON
    for time in tools.iter_hours(cfg.startdate_sim,
                                 (cfg.enddate_sim + timedelta(days=1)),
                                 step=cfg.meteo_nudging_step):
        boundary_conditions_script(cfg, ERA5_folder, CAMS_folder, time)

    # -- 5. Download ICOS CO2 data
    # Lots of potential for 'dehardcoding' things here, but that has to be done with
    # a lot of care.
    if cfg.CTDAS_obs_ICOS_fetch:
        fetch_ICOS_data(start_date=cfg.startdate_sim.strftime("%d-%m-%Y"),
                        end_date=(cfg.enddate_sim +
                                  timedelta(days=1)).strftime("%d-%m-%Y"),
                        save_path=cfg.CTDAS_obs_ICOS_path,
                        species=[
                            'co2',
                        ])
        tools.create_dir(ICOS_path := cfg.case_root / "global_inputs" / "ICOS",
                         "ICOS input files")
        process_ICOS_data(ICOS_obs_folder=cfg.CTDAS_obs_ICOS_path,
                          start_date=cfg.startdate_sim,
                          end_date=(cfg.enddate_sim + timedelta(days=1)),
                          output_folder=ICOS_path)

    # -- 6. Download OCO2 data
    if cfg.CTDAS_obs_OCO2_fetch:
        # fetch_OCO2_data(cfg.startdate_sim,
        #                 (cfg.enddate_sim + timedelta(days=1)),
        #                 -8, 30, 35, 65,
        #                 cfg.CTDAS_obs_OCO2_path,
        #                 product="OCO2_L2_Lite_FP_11.1r")
        tools.create_dir(OCO2_path := cfg.case_root / "global_inputs" / "OCO2",
                         "OCO-2 output")
        process_OCO2_data(
            OCO2_obs_folder=cfg.CTDAS_obs_OCO2_path,
            ICON_grid_file=cfg.input_files_dynamics_grid_filename,
            start_date=cfg.startdate_sim,
            end_date=(cfg.enddate_sim + timedelta(days=1)),
            output_folder=OCO2_path)

    # -- 7. Create the required run data
    # Create sampling output folder
    tools.create_dir(cfg.case_root / "global_outputs" / "extracted_ICOS",
                     "Output of the extraction script")

    # Create ICON jobs
    create_icon_job(cfg, "prior")
    create_icon_job(cfg, "opt1")
    create_icon_job(cfg, "opt2")
    if cfg.startdate_sim == cfg.startdate:
        create_icon_job(cfg, "opt2", firstrun=True)
    if cfg.CTDAS_runthrough:
        create_icon_job(cfg, "runthrough", runthrough=True)
    if (cfg.startdate_sim == cfg.startdate) and cfg.CTDAS_runthrough:
        create_icon_job(cfg, "runthrough", firstrun=True, runthrough=True)

    # Copy global input data
    if cfg.startdate_sim == cfg.startdate: copy_global_inputs(cfg)

    # Generate tracers
    if cfg.startdate_sim == cfg.startdate: generate_tracers(cfg)

    # Generate initial ensemble lambdas (equal to 1)
    if cfg.startdate_sim == cfg.startdate:
        # Set up OEM Folder
        tools.create_dir(OEM_folder := cfg.case_root / "global_inputs" / "OEM",
                         "OEM")
        # Interpret lambdas from the YAML file
        lambdas = [
            int(item) for line in cfg.CTDAS_lambdas for item in line.split(',')
        ]
        # Create lambda regions for basegrid
        if cfg.CTDAS_regions == 'basegrid':
            nregs, ncats = create_lambda_regions(
                cfg.input_files_dynamics_grid_filename,
                OEM_folder / "lambdaregions.nc", lambdas)
            create_prior_all_ones(OEM_folder / "prior_all_ones.nc",
                                  nensembles=cfg.CTDAS_nensembles,
                                  ncats=max(lambdas),
                                  nregs=nregs,
                                  propagate_bg=cfg.CTDAS_propagate_bg)
            if cfg.CTDAS_runthrough:
                create_prior_all_zeros(OEM_folder / "prior_all_zeros.nc",
                                       nensembles=cfg.CTDAS_nboundaries,
                                       ncats=max(lambdas),
                                       nregs=nregs)
        else:
            raise NotImplementedError('Only basegrid is implemented for now')
        create_boundary_regions(cfg.input_files_dynamics_grid_filename,
                                OEM_folder / 'boundary_mask_bg.nc',
                                cfg.CTDAS_nboundaries, cfg.cdo_nco_cmd,
                                cfg.cdo_nco_cmd_post)
        create_boundary_prior_all_ones(OEM_folder / 'boundary_lambdas_bg.nc',
                                       n_bg_ens=cfg.CTDAS_nboundaries,
                                       nensembles=cfg.CTDAS_nensembles,
                                       propagate_bg=cfg.CTDAS_propagate_bg)
        if cfg.CTDAS_runthrough:
            create_boundary_prior_separate(OEM_folder /
                                           'boundary_lambdas_separate.nc',
                                           n_bg_ens=cfg.CTDAS_nboundaries)
    # Patch CTDAS files
    if cfg.startdate_sim == cfg.startdate:
        logging.info("Patching CTDAS files")

        def evaluate_dict(d, replace, using):
            return {
                key: eval(value.replace(replace, str(using)))
                for key, value in d.items()
            }

        meta_dict = {
            d.replace("XXX", "ENS"): {
                "ensemble": cfg.CTDAS_nensembles + (1 if cfg.CTDAS_propagate_bg else 0)
            } if "XXX" in d else {}
            for d in cfg.tracers if not d.startswith("EM")
        }
        for key, source_paths in cfg.CTDAS["ctdas_patch"].items():
            destination_dir = Path(cfg.CTDAS_ctdas_path) / key
            os.makedirs(destination_dir, exist_ok=True)
            if isinstance(source_paths, str):
                source_paths = [source_paths]
            for source_path in source_paths:
                in_path = cfg.case_path / source_path
                destination_path = destination_dir / in_path.name
                with in_path.open('r') as infile, destination_path.open(
                        'w') as outfile:
                    outfile.write(eval(f"f'''{infile.read()}'''"))
                logging.info(f"Copied {in_path} -> {destination_path}")

    logging.info("OK")
    shutil.copy(cfg.logfile, cfg.logfile_finish)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta
import pytz

import logging
import os
import sys
import time
import shutil
import argparse

import jobs
from jobs import tools
from config import Config


def parse_arguments():
    """Parse command line arguments for the processing chain script.

    Parses and retrieves command line arguments, allowing users to specify
    run identifiers, jobs to execute, and various options to control the
    execution of the processing chain.

    Returns
    -------
    argparse.Namespace
        A namespace object containing parsed command line arguments.
    """
    parser = argparse.ArgumentParser(description="Run the processing chain.")

    parser.add_argument("casenames",
                        nargs='+',
                        help="List of identifiers for the runs. "
                        "The config-files for each run is assumed "
                        "to be in cases/<casename>/. The runs are executed "
                        "sequentially in the order they're given here.")

    jobs_help = ("List of job-names to be executed. A job is a .py-"
                 "file in jobs/ with a main()-function which "
                 "handles one aspect of the processing chain, for "
                 "example copying meteo-input data or launching a "
                 "job for int2lm. "
                 "Jobs are executed in the order in which they are "
                 "given here. "
                 "If no jobs are given, default jobs will be executed"
                 "as defined in config/workflows.yaml.")
    parser.add_argument("-j",
                        "--jobs",
                        nargs='*',
                        dest="job_list",
                        help=jobs_help,
                        default=None)

    chunks_help = ("List of chunks to be executed. A chunk is time"
                   "frame within the total simulation period."
                   "It has the format `YYYYMMDDHH_YYYYMMDDHH`."
                   "If no chunks are given, all chunks within the"
                   "simulation period will be executed.")
    parser.add_argument("-c",
                        "--chunks",
                        nargs='*',
                        dest="chunk_list",
                        help=chunks_help,
                        default=None)

    sync_help = ("Force synchronous execution.")
    parser.add_argument("-s",
                        "--force-sync",
                        action='store_true',
                        help=sync_help)

    no_logging_help = ("Disable logging for chain_status.log.")
    parser.add_argument("--no-logging",
                        action='store_false',
                        dest="enable_logging",
                        default=True,
                        help=no_logging_help)

    force_help = ("Force the processing chain to redo all specified jobs,"
                  " even if they have been started already or were finished"
                  " previously. WARNING: Only logfiles get deleted,"
                  " other effects of a given job (copied files etc.)"
                  " are simply overwritten. This may cause errors.")
    parser.add_argument("-f", "--force", action='store_true', help=force_help)

    tries_help = ("Amount of time the cosmo job is re-tried before crashing."
                  " Default is 1.")
    parser.add_argument("-t",
                        "--try",
                        help=tries_help,
                        dest="ntry",
                        type=int,
                        default=1)

    resume_help = (
        "Resume the processing chain by restarting the last unfinished job."
        " WARNING: Only the logfile gets deleted,"
        " other effects of a given job (copied files etc.)"
        " are simply overwritten. This may cause errors.")
    parser.add_argument("-r",
                        "--resume",
                        help=resume_help,
                        dest="resume",
                        action='store_true')

    args = parser.parse_args()

    return args


def run_chunk(cfg, force, resume):
    """Run a chunk of the processing chain, managing job execution and logging.

    This function sets up and manages the execution of a processing chain, handling
    job execution, logging, and various configuration settings.

    Parameters
    ----------
    cfg : Config
        Object holding user-defined configuration parameters as attributes.
    force : bool
        If True, it will force the execution of jobs regardless of their completion status.
    resume : bool
        If True, it will resume the last unfinished job.

    Raises
    ------
    RuntimeError
        If an error or timeout occurs during job execution.

    Notes
    -----
    - This function sets various configuration values based on the provided parameters.
    - It checks for job completion status and resumes or forces execution accordingly.
    - Job log files are managed, and errors or timeouts are handled with notifications.
    """
    # Set forecast time
    cfg.forecasttime = (cfg.enddate_sim -
                        cfg.startdate_sim).total_seconds() / 3600

    # String variables for startdate_sim
    cfg.startdate_sim_yyyymmddhh = cfg.startdate_sim.strftime('%Y%m%d%H')
    cfg.enddate_sim_yyyymmddhh = cfg.enddate_sim.strftime('%Y%m%d%H')

    # Folder naming and structure
    cfg.chunk_id = f'{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}'
    cfg.chain_root = cfg.work_root / cfg.casename / cfg.chunk_id

    # Config variables for spinup runs (datetimes, job-id, etc.)
    if hasattr(cfg, 'spinup'):
        if cfg.first_one:  # first run in spinup
            cfg.chain_root_prev = None
        else:  # consecutive runs in spinup
            cfg.startdate_sim_yyyymmddhh = cfg.startdate_sim.strftime(
                '%Y%m%d%H')
            enddate_sim_yyyymmddhh_prev = (
                cfg.enddate_sim -
                timedelta(hours=cfg.restart_step_hours)).strftime('%Y%m%d%H')

            if cfg.second_one:
                startdate_sim_yyyymmddhh_prev = (cfg.enddate_sim - timedelta(
                    hours=2 * cfg.restart_step_hours)).strftime('%Y%m%d%H')
            else:  # all other runs (i.e., get chunk_id from previous run)
                startdate_sim_yyyymmddhh_prev = (
                    cfg.enddate_sim -
                    timedelta(hours=2 * cfg.restart_step_hours +
                              cfg.spinup)).strftime('%Y%m%d%H')

            cfg.chunk_id_prev = f'{startdate_sim_yyyymmddhh_prev}_{enddate_sim_yyyymmddhh_prev}'
            cfg.chain_root_prev = cfg.work_root / cfg.casename / cfg.chunk_id_prev
            cfg.last_cosmo_output = cfg.chain_root_prev / 'cosmo' / 'output'

        # No restart for spinup simulations (= default values for no restart)
        cfg.cosmo_restart_out = ''
        cfg.cosmo_restart_in = ''
    elif 'restart' in cfg.workflow['features']:
        cfg.startdate_sim_prev = cfg.startdate_sim - timedelta(
            hours=cfg.restart_step_hours)
        cfg.enddate_sim_prev = cfg.enddate_sim - timedelta(
            hours=cfg.restart_step_hours)
        cfg.startdate_sim_prev_yyyymmddhh = cfg.startdate_sim_prev.strftime(
            '%Y%m%d%H')
        cfg.enddate_sim_prev_yyyymmddhh = cfg.enddate_sim_prev.strftime(
            '%Y%m%d%H')

        cfg.chunk_id_prev = f'{cfg.startdate_sim_prev_yyyymmddhh}_{cfg.enddate_sim_prev_yyyymmddhh}'
        cfg.chain_root_prev = cfg.work_root / cfg.casename / cfg.chunk_id_prev

        # Set restart directories
        cfg.cosmo_restart_out = cfg.chain_root / 'cosmo' / 'restart'
        cfg.cosmo_restart_in = cfg.chain_root_prev / 'cosmo' / 'restart'

    # Check constraint
    if hasattr(cfg, 'constraint'):
        assert cfg.constraint in ['gpu', 'mc'], ("Unknown constraint, use"
                                                 "gpu or mc")

    # If nested run: use output of mother-simulation
    if 'nesting' in cfg.workflow['features'] and not os.path.isdir(
            cfg.meteo.dir):
        # if ifs_hres_dir doesn't point to a directory,
        # it is the name of the mother run
        mother_name = cfg.meteo.dir
        cfg.meteo.dir = cfg.work_root / mother_name / cfg.chunk_id / 'cosmo' / 'output'
        cfg.meteo.inc = 1
        cfg.meteo.prefix = 'lffd'

    # Logging
    cfg.log_working_dir = cfg.chain_root / 'checkpoints' / 'working'
    cfg.log_finished_dir = cfg.chain_root / 'checkpoints' / 'finished'

    # Create working directories
    tools.create_dir(cfg.chain_root, "chain_root")
    tools.create_dir(cfg.log_working_dir, "log_working")
    tools.create_dir(cfg.log_finished_dir, "log_finished")

    # Number of levels and switch for unit conversion for 'reduce_output' job
    if not hasattr(cfg, 'output_levels'):
        cfg.output_levels = -1
    if not hasattr(cfg, 'convert_gas'):
        cfg.convert_gas = True

    if cfg.is_async:
        # Empty curent job ids
        cfg.job_ids['current'] = {}

        # Submit current chunk
        for job_name in cfg.jobs:
            if (cfg.log_finished_dir / job_name).exists() and not force:
                # Skip job if already finished
                print(f'    └── Skipping "{job_name}" job')
                skip = True
            else:
                print(f'    └── Starting "{job_name}" job')

                # Logfile settings
                cfg.logfile = cfg.log_working_dir / job_name
                cfg.logfile_finish = cfg.log_finished_dir / job_name

                # Submit the job
                job = getattr(jobs, job_name)
                if hasattr(job, 'BASIC_PYTHON_JOB') and job.BASIC_PYTHON_JOB:
                    script = cfg.create_sbatch_script(job_name)
                    cfg.submit(job_name, script)
                else:
                    job.main(cfg)

        # Wait for previous chunk jobs, monitor them and cycle info
        cfg.cycle()

    else:
        # run jobs (if required)
        for job in cfg.jobs:
            skip = False

            # if exists job is currently worked on or has been finished
            if (cfg.log_working_dir / job).exists():
                if not force:
                    while True:
                        if (cfg.log_finished_dir / job).exists():
                            print(
                                f'    └── Skip "{job}" for chunk "{cfg.chunk_id}"'
                            )
                            skip = True
                            break
                        elif resume:
                            resume = False
                            break
                        else:
                            print(
                                f"    └── Wait for {job} of chunk {cfg.chunk_id}"
                            )
                            sys.stdout.flush()
                            for _ in range(3000):
                                time.sleep(0.1)
                else:
                    (cfg.log_working_dir / job).unlink()
                    (cfg.log_finished_dir / job).unlink(missing_ok=True)

            if not skip:
                print(f'    └── Process "{job}" for chunk "{cfg.chunk_id}"')
                sys.stdout.flush()

                try_count = 1 + (cfg.ntry - 1) * (job == 'cosmo')
                while try_count > 0:
                    try_count -= 1
                    try:
                        # Change the log file
                        cfg.logfile = cfg.log_working_dir / job
                        cfg.logfile_finish = cfg.log_finished_dir / job

                        # Launch the job
                        to_call = getattr(jobs, job)
                        to_call.main(cfg)

                        shutil.copy(cfg.logfile, cfg.logfile_finish)

                        exitcode = 0
                        try_count = 0
                    except Exception:
                        subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                            job, cfg.chunk_id)
                        logging.exception(subject)
                        if cfg.user_mail:
                            message = tools.prepare_message(
                                cfg.log_working_dir / job)
                            logging.info('Sending log file to %s' %
                                         cfg.user_mail)
                            tools.send_mail(cfg.user_mail, subject, message)
                        if try_count == 0:
                            raise RuntimeError(subject)

                if exitcode != 0 or not (cfg.log_finished_dir / job).exists():
                    subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                        job, cfg.chunk_id)
                    if cfg.user_mail:
                        message = tools.prepare_message(cfg.log_working_dir /
                                                        job)
                        logging.info('Sending log file to %s' % cfg.user_mail)
                        tools.send_mail(cfg.user_mail, subject, message)
                    raise RuntimeError(subject)


def restart_runs(cfg, force, resume):
    """Start subchains in specified intervals and manage restarts.

    This function slices the total runtime of the processing chain according to the
    `cfg.restart_step_hours` configuration. It calls `run_chunk()` for each
    specified interval.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    force : bool
        If True, it will force the execution of jobs regardless of their completion status.
    resume : bool
        If True, it will resume the last unfinished job.

    Notes
    -----
    - The function iterates over specified intervals, calling `run_chunk()` for each.
    - It manages restart settings and logging for each subchain.
    """
    if not cfg.chunks:
        for startdate_sim in tools.iter_hours(cfg.startdate, cfg.enddate,
                                              cfg.restart_step_hours):
            if 'spinup' in cfg.workflow['features'] and hasattr(cfg, 'spinup'):
                if startdate_sim > cfg.startdate:
                    startdate_sim = startdate_sim - timedelta(hours=cfg.spinup)

            enddate_sim = startdate_sim + timedelta(
                hours=cfg.restart_step_hours)
            startdate_sim_yyyymmddhh = startdate_sim.strftime("%Y%m%d%H")
            enddate_sim_yyyymmddhh = enddate_sim.strftime("%Y%m%d%H")
            chunk_id = f"{startdate_sim_yyyymmddhh}_{enddate_sim_yyyymmddhh}"

            if enddate_sim > cfg.enddate:
                continue

            cfg.chunks.append(chunk_id)

    for chunk_id in cfg.chunks:
        cfg.chunk_id = chunk_id
        cfg.startdate_sim_yyyymmddhh = chunk_id[0:10]
        cfg.enddate_sim_yyyymmddhh = chunk_id[-10:]
        cfg.startdate_sim = datetime.strptime(
            cfg.startdate_sim_yyyymmddhh, "%Y%m%d%H").replace(tzinfo=pytz.UTC)
        cfg.enddate_sim = datetime.strptime(
            cfg.enddate_sim_yyyymmddhh, "%Y%m%d%H").replace(tzinfo=pytz.UTC)

        if 'spinup' in cfg.workflow['features'] and hasattr(cfg, 'spinup'):
            print("Using spin-up restarts.")
            if cfg.startdate_sim == cfg.startdate:
                cfg.first_one = True
                cfg.second_one = False
                cfg.lrestart = '.FALSE.'
            elif cfg.startdate_sim == cfg.startdate + timedelta(
                    hours=cfg.restart_step_hours):
                cfg.first_one = False
                cfg.second_one = True
                cfg.lrestart = '.TRUE.'
            else:
                cfg.first_one = False
                cfg.second_one = False
                cfg.lrestart = '.TRUE.'
        else:
            print("Using built-in model restarts.")
            # Set restart variable (only takes effect for ICON)
            cfg.lrestart = ".FALSE." if cfg.startdate_sim == cfg.startdate else ".TRUE."

        print(f'└── Starting chunk "{cfg.chunk_id}"')

        run_chunk(cfg=cfg, force=force, resume=resume)


def main():
    """Main script for running a processing chain.

    This script handles the execution of a processing chain for one or more
    specified cases. It loads model configurations, prepares the environment,
    and starts the chain based on the provided settings.

    Parameters
    ----------
    None (Command-line arguments are parsed internally)

    Notes
    -----
    - This script uses command-line arguments to specify cases and job lists.
    - It loads model configurations, converts paths to absolute, sets restart 
      settings, and starts the chain.
    - Depending on the model's features, it may run with or without restarts
      or utilize spin-up restarts.
    """
    args = parse_arguments()

    for casename in args.casenames:
        # Load configs
        cfg = Config(casename)

        # Make ntry a Config variable
        cfg.ntry = args.ntry

        # Convert relative to absolute paths
        cfg.convert_paths_to_absolute()

        # Set restart step in hours
        cfg.set_restart_step_hours()

        # Duplicate variables in the form of <dict>_<value> for better
        # access within namelist template.
        # E.g.: cfg.meteo['dir'] -> cfg.meteo_dir
        cfg.create_vars_from_dicts()

        # Check if jobs are set or if default ones are used
        if args.job_list is None:
            cfg.jobs = cfg.workflow['jobs']
        else:
            cfg.jobs = args.job_list

        # Check if chunks are set or if all are used
        if args.chunk_list is None:
            cfg.chunks = []
        else:
            cfg.chunks = args.chunk_list

        # Check sync is forced
        if args.force_sync:
            cfg.is_async = None

        # Print config before chain starts
        cfg.print_config()

        tools.create_dir(cfg.case_root, "case_root")

        print("╔════════════════════════════════════════╗")
        print("║       Starting Processing Chain        ║")
        print("║════════════════════════════════════════║")
        print(f"║  Case: {casename: <31} ║")
        print(f"║  Workflow: {cfg.workflow_name: <27} ║")
        print("╚════════════════════════════════════════╝")

        # Check for restart compatibility and spinup
        if 'restart' in cfg.workflow['features']:
            restart_runs(cfg=cfg, force=args.force, resume=args.resume)
        else:
            print("No restarts are used.")
            cfg.startdate_sim = cfg.startdate
            cfg.enddate_sim = cfg.enddate
            run_chunk(cfg=cfg, force=args.force, resume=args.resume)

    print("╔════════════════════════════════════════╗")
    print("║       Processing Chain Completed       ║")
    print("╚════════════════════════════════════════╝")


if __name__ == '__main__':
    main()

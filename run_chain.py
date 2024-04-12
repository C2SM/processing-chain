#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta
import pytz
import logging
import shutil
import argparse

import jobs
from jobs import tools
from config import Config


def parse_arguments():
    """Parse command line arguments for the Processing Chain script.

    Parses and retrieves command line arguments, allowing users to specify
    run identifiers, jobs to execute, and various options to control the
    execution of the Processing Chain.

    Returns
    -------
    argparse.Namespace
        A namespace object containing parsed command line arguments.
    """
    parser = argparse.ArgumentParser(description="Run the Processing Chain.")

    parser.add_argument("casenames",
                        nargs='+',
                        help="List of identifiers for the runs. "
                        "The config-files for each run is assumed "
                        "to be in cases/<casename>/. The runs are executed "
                        "sequentially in the order they're given here.")

    jobs_help = ("List of job names to be executed. A job is a .py "
                 "file in jobs/ with a main()-function which "
                 "handles one aspect of the Processing Chain, for "
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
                  " are simply overwritten. This may cause errors"
                  " or unexpected behavior.")
    parser.add_argument("-f", "--force", action='store_true', help=force_help)

    resume_help = (
        "Resume the Processing Chain by restarting the last unfinished job."
        " WARNING: Only the logfile gets deleted,"
        " other effects of a given job (copied files etc.)"
        " are simply overwritten. This may cause errors."
        " or unexpected behavior.")
    parser.add_argument("-r",
                        "--resume",
                        help=resume_help,
                        dest="resume",
                        action='store_true')

    args = parser.parse_args()

    return args


def run_chunk(cfg, force, resume):
    """Run a chunk of the processing chain, managing job execution and logging.

    This function sets up and manages the execution of a Processing Chain, handling
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

    # Logging
    cfg.chain_root = cfg.work_root / cfg.casename / cfg.chunk_id
    cfg.log_working_dir = cfg.chain_root / 'checkpoints' / 'working'
    cfg.log_finished_dir = cfg.chain_root / 'checkpoints' / 'finished'

    # Create working directories
    tools.create_dir(cfg.chain_root, "chain_root")
    tools.create_dir(cfg.log_working_dir, "log_working")
    tools.create_dir(cfg.log_finished_dir, "log_finished")

    # Config variables for spinup and restart runs
    cfg.cosmo_restart_in = ''
    cfg.cosmo_restart_out = ''
    if hasattr(cfg, 'spinup'):
        if cfg.chunk_id_prev:
            cfg.chain_root_prev = cfg.work_root / cfg.casename / cfg.chunk_id_prev
            cfg.last_cosmo_output = cfg.chain_root_prev / 'cosmo' / 'output'
    elif 'restart' in cfg.workflow['features']:
        if cfg.chunk_id_prev:
            cfg.chain_root_prev = cfg.work_root / cfg.casename / cfg.chunk_id_prev
            cfg.cosmo_restart_in = cfg.chain_root_prev / 'cosmo' / 'restart'
        cfg.cosmo_restart_out = cfg.chain_root / 'cosmo' / 'restart'

    if not cfg.force_sync:
        # Empty curent job ids
        cfg.job_ids['current'] = {}

        # Submit current chunk
        for job_name in cfg.jobs:
            if (cfg.log_finished_dir / job_name).exists() and not force:
                # Skip job if already finished
                print(f'    └── Skipping "{job_name}" job')
                skip = True
            else:
                print(f'    └── Submitting "{job_name}" job')

                # Logfile settings
                cfg.logfile = cfg.log_working_dir / job_name
                cfg.logfile_finish = cfg.log_finished_dir / job_name

                # Submit the job
                job = getattr(jobs, job_name)
                if hasattr(job, 'BASIC_PYTHON_JOB') and job.BASIC_PYTHON_JOB:
                    cfg.submit_basic_python(job_name)
                else:
                    job.main(cfg)

        # Wait for previous chunk jobs, monitor them and cycle info
        cfg.cycle()

    else:  # For nested run_chain.py
        for job_name in cfg.jobs:
            print(f'    └── Process "{job_name}" for chunk "{cfg.chunk_id}"')
            try:
                # Change the log file
                cfg.logfile = cfg.log_working_dir / job_name
                cfg.logfile_finish = cfg.log_finished_dir / job_name

                # Launch the job
                to_call = getattr(jobs, job_name)
                to_call.main(cfg)

                shutil.copy(cfg.logfile, cfg.logfile_finish)

                exitcode = 0
            except Exception:
                exitcode = 1
                subject = "ERROR or TIMEOUT in job '%s' for chunk '%s'" % (
                    job_name, cfg.chunk_id)
                logging.exception(subject)
                if cfg.user_mail:
                    message = tools.prepare_message(cfg.log_working_dir /
                                                    job_name)
                    logging.info('Sending log file to %s' % cfg.user_mail)
                    tools.send_mail(cfg.user_mail, subject, message)

            if exitcode != 0 or not (cfg.log_finished_dir / job_name).exists():
                subject = "ERROR or TIMEOUT in job '%s' for chunk '%s'" % (
                    job_name, cfg.chunk_id)
                if cfg.user_mail:
                    message = tools.prepare_message(cfg.log_working_dir /
                                                    job_name)
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

    for chunk_id in cfg.chunks:
        cfg.chunk_id = chunk_id
        cfg.get_previous_chunk_id(cfg.chunk_id)
        cfg.startdate_sim_yyyymmddhh = cfg.chunk_id[0:10]
        cfg.enddate_sim_yyyymmddhh = cfg.chunk_id[-10:]
        cfg.startdate_sim = datetime.strptime(
            cfg.startdate_sim_yyyymmddhh, "%Y%m%d%H").replace(tzinfo=pytz.UTC)
        cfg.enddate_sim = datetime.strptime(
            cfg.enddate_sim_yyyymmddhh, "%Y%m%d%H").replace(tzinfo=pytz.UTC)

        if 'spinup' in cfg.workflow['features'] and hasattr(cfg, 'spinup'):
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

        # Check sync is forced
        if args.force_sync:
            cfg.force_sync = True
        else:
            cfg.force_sync = False

        # Check constraint
        if cfg.constraint and cfg.machine == 'daint':
            assert cfg.constraint in ['gpu', 'mc'], ("Unknown constraint, use"
                                                     "gpu or mc")

        # Get complete chunk list
        cfg.get_chunk_list()

        # Print config before chain starts
        cfg.print_config()

        # Get custom chunks if specified
        cfg.chunks = args.chunk_list if args.chunk_list else cfg.chunk_list

        tools.create_dir(cfg.case_root, "case_root")

        print("╔════════════════════════════════════════╗")
        print("║       Starting Processing Chain        ║")
        print("╠════════════════════════════════════════╣")
        print(f"║      Case: {casename: <27} ║")
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

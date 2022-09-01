#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta
from genericpath import isdir

import importlib
from itertools import chain
import logging
import glob
import os
import sys
import time
import shutil
import argparse
import xarray as xr
import jobs
from jobs import tools

default_jobs = {
    tools.Target.ICON: ["prepare_data", "icon"],
    tools.Target.ICONART: ["prepare_data_global", "icon_global"],
    tools.Target.ICONARTOEM: ["prepare_data", "oae", "icon"]
}

def parse_arguments():
    """Parse the command line arguments given to this script

    Returns
    -------
    Namespace-object
    """
    parser = argparse.ArgumentParser(description="Run the processing chain.")

    parser.add_argument("casenames",
                        nargs='+',
                        help="List of identifiers for the runs. "
                        "The config-files for each run is assumed "
                        "to be in cases/<casename>/. The runs are executed "
                        "sequentially in the order they're given here.")

    times_help = ("Triplet of {date hstart hstop} | "
                  "date: Startdate of the run in the format "
                  "yyyy-mm-dd | "
                  "hstart: Time on the startdate when the "
                  "simulation starts. If this is zero, the "
                  "simulation starts at midnight of the +startdate. | "
                  "hstop: Length of the simulation in hours. The "
                  "simulation runs until startdate + hstart + "
                  "hstop. Depending on your config.py settings, "
                  "processing-chain will split up the simulation "
                  "and perform several restarts before reaching the "
                  "stopping-time.")
    parser.add_argument("times", nargs=3, help=times_help)

    jobs_help = ("List of job-names to be executed. A job is a .py-"
                 "file in jobs/ with a main()-function which "
                 "handles one aspect of the processing chain, for "
                 "example copying meteo-input data or launching a "
                 "job for int2lm. "
                 "Jobs are executed in the order in which they are "
                 "given here. "
                 "If no jobs are given, the default that will be "
                 "executed is: COSMO: {} | COSMOART : {}".format(
                     default_jobs[tools.Target.ICON],
                     default_jobs[tools.Target.ICONART]))
    parser.add_argument("-j",
                        "--jobs",
                        nargs='*',
                        dest="job_list",
                        help=jobs_help,
                        default=None)

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

    args = parser.parse_args()
    args.startdate = args.times[0]
    args.hstart = int(args.times[1])
    args.hstop = int(args.times[2])

    return args


def load_config_file(casename, cfg):
    """Load the config file.

    Looks for the config file in ``cases/casename/config.py`` and then imports
    it as a module. This lets the config file contain python statements which
    are evaluated on import.

    If this is not the first config-file to be imported by run_chain.py, the
    module has to be reloaded to overwrite the values of the old case.

    Access variables declared in the config-file (``myval = 9``) with
    ``cfg.myval``.

    Add new variables with::

        setattr(cfg, 'myval', 9)

    Parameters
    ----------
    casename : str
        Name of the folder in cases/ where the configuration files are stored
    cfg : module or None
        If cfg is None, the module is freshly imported. If it is a module
        object, that module is reloaded.

    Returns
    -------
    config-object
        Object with all variables as attributes
    """
    cfg_path = os.path.join('cases', casename, 'config')

    if not os.path.exists(os.path.dirname(cfg_path)):
        all_cases = [path.name for path in os.scandir('cases') if path.is_dir]
        closest_name = min([(tools.levenshtein(casename, name), name)
                            for name in all_cases],
                           key=lambda x: x[0])[1]
        raise FileNotFoundError("Case-directory '{}' not found, did you "
                                "mean '{}'?".format(casename, closest_name))

    sys.path.append(os.path.dirname(cfg_path))

    try:
        if cfg is None:
            cfg = importlib.import_module(os.path.basename(cfg_path))
        else:
            cfg = importlib.reload(cfg)
    except ModuleNotFoundError:
        raise FileNotFoundError("No file 'config.py' in " +
                                os.path.dirname(cfg_path))

    # so that a different cfg-file can be imported later
    sys.path.pop()

    return cfg


def set_simulation_type(cfg):
    """Detect the chain target and if there is a subtarget.

    Check if a target was provided in the config-object. If no target is
    provided, set the target to cosmo in the config-object.

    Check if a subtarget was provided in the config-object. Subtargets
    provide a way to customize the behaviour of the processing chain
    for different types of simulations.

    Raise a RuntimeError if an unsupported target or subtarget is given in cfg.
    You can add targets and subtargets in the jobs/tools/__init__.py file.

    Translates the target and subtarget from string to enum.

    Parameters
    ----------
    cfg : config-object
    """
    default = 'icon-art'

    # -- Set Target
    target_str = getattr(cfg, 'TARGET', default)
    try:
        target_enum = tools.str_to_enum[target_str.lower()]
    except KeyError:
        raise ValueError("The target of the chain must be one of {}".format(
            list(tools.str_to_enum.keys())))
    setattr(cfg, 'TARGET', target_enum)

    # -- Set Subtarget
    subtarget_str = getattr(cfg, 'SUBTARGET', 'none')
    try:
        subtarget_enum = tools.str_to_enum[subtarget_str.lower()]
    except KeyError:
        raise ValueError("The target of the chain must be one of {}".format(
            list(tools.str_to_enum.keys())))
    setattr(cfg.TARGET, 'SUBTARGET', subtarget_enum)


def run_chain(work_root, cfg, start_time, hstart, hstop, job_names, spinup, force):
    """Run chain ignoring already finished jobs.

    Sets configuration values derived from user-provided ones, for example the
    folder-structure inside the working directory.

    Sets up the logging module used by the jobs.

    Creates directories for each job.

    Decides which jobs to run and then runs them; first it checks wether the
    job was already executed or is currently running (depending on the logging
    file of the job). Then if the job has to be run, it calls the main()-
    function of the job. If force is True, the logging-file of the job will
    be deleted (if it exists) and the job will be executed regardless.

    Parameters
    ----------
    work_root : str
        The path to the directory in which the chain writes files during
        execution (typically scratch)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    start_time : datetime-object
        The startdate of the simulation
    hstart : int
        Offset (in hours) of the actual start from the startdate (start param)
    hstop : int
        Length of simulation (in hours)
    job_names : list of str
        List of the names of jobs to execute on every timeslice.
        Jobs are ``.py`` files in the ``jobs/`` directory with a ``main()``
        function that will be called from ``run_chain()``.
    force : bool
        If True will do job regardless of completion status
    """
    # Read mail address
    # if os.environ['USER'] == 'jenkins':
    #     mail_address = None
    # elif os.path.exists(os.environ['HOME'] + '/.forward'):
    #     with open(os.environ['HOME'] + '/.forward', 'r') as file:
    #         mail_address = file.read().rstrip()
    # else:
    mail_address = None

    # -----------------------------------------------
    # -- Set simulation parameters and working dir
    # -----------------------------------------------


    # ini date and forecast time (ignore meteo times)
    inidate = int((start_time - datetime(1970, 1, 1)).total_seconds())
    inidate_yyyymmddhh = start_time.strftime('%Y%m%d%H')
    inidate_yyyymmdd_hh = start_time.strftime('%Y%m%d_%H')
    inidate_yyyymmddhhmmss = start_time.strftime('%Y%m%d%H%M%S')
    forecasttime = '%d' % (hstop - hstart)
    setattr(cfg, 'inidate', inidate)
    setattr(cfg, 'inidate_yyyymmddhh', inidate_yyyymmddhh)
    setattr(cfg, 'inidate_yyyymmdd_hh', inidate_yyyymmdd_hh)
    setattr(cfg, 'inidate_yyyymmddhhmmss', inidate_yyyymmddhhmmss)
    setattr(cfg, 'hstart', hstart)
    setattr(cfg, 'hstop', hstop)
    setattr(cfg, 'forecasttime', forecasttime)

    # -- Initial and end date of the simulation 
    ini_datetime_string = (start_time + timedelta(hours=hstart)).strftime('%Y-%m-%dT%H:00:00Z')
    end_datetime_string = (start_time + timedelta(hours=hstop)).strftime('%Y-%m-%dT%H:00:00Z')
    setattr(cfg, 'ini_datetime_string', ini_datetime_string)
    setattr(cfg, 'end_datetime_string', end_datetime_string)

    # -- Job/simulation ID and directory (spinups have different names from regular simulations)
    if spinup:
        job_id = 'spinup_%s_%d_%d' % (inidate_yyyymmddhh, hstart, hstop)
    else:
        job_id = 'run_%s_%d_%d' % (inidate_yyyymmddhh, hstart, hstop)

    chain_root = os.path.join(work_root, cfg.CASENAME, job_id)
    setattr(cfg, 'chain_root', chain_root)

    # -----------------------------------------------
    # -- ICON input data in workdir
    # -----------------------------------------------

    # -- Set attributes for base directory for input data
    setattr(cfg, 'icon_base', os.path.join(chain_root, 'icon'))
    setattr(cfg, 'icon_input', os.path.join(chain_root, 'icon', 'input'))
    setattr(cfg, 'icon_input_grid',
            os.path.join(chain_root, 'icon', 'input', 'grid'))
    setattr(cfg, 'icon_input_icbc',
            os.path.join(chain_root, 'icon', 'input', 'icbc'))
    setattr(cfg, 'icon_input_rad',
            os.path.join(chain_root, 'icon', 'input', 'rad'))
    setattr(cfg, 'icon_input_xml',
            os.path.join(chain_root, 'icon', 'input', 'xml'))
    setattr(cfg, 'icon_work', os.path.join(chain_root, 'icon', 'run'))
    setattr(cfg, 'icon_output', os.path.join(chain_root, 'icon', 'output'))

    # -- Set attributes for input filenames
    setattr(
        cfg, 'dynamics_grid_filename_scratch',
        os.path.join(cfg.icon_input_grid,
                        os.path.basename(cfg.DYNAMICS_GRID_FILENAME)))
    setattr(
        cfg, 'radiation_grid_filename_scratch',
        os.path.join(cfg.icon_input_grid,
                        os.path.basename(cfg.RADIATION_GRID_FILENAME)))
    setattr(
        cfg, 'extpar_filename_scratch',
        os.path.join(cfg.icon_input_grid,
                        os.path.basename(cfg.EXTPAR_FILENAME)))
    setattr(
        cfg, 'cldopt_filename_scratch',
        os.path.join(cfg.icon_input_rad,
                        os.path.basename(cfg.CLDOPT_FILENAME)))
    setattr(
        cfg, 'lrtm_filename_scratch',
        os.path.join(cfg.icon_input_rad,
                        os.path.basename(cfg.LRTM_FILENAME)))
    setattr(
        cfg, 'inicond_filename_scratch',
        os.path.join(cfg.icon_input_icbc,
                        os.path.basename(cfg.INICOND_FILENAME)))
    if hasattr(cfg, 'CHEMTRACER_XML_FILENAME'):
        setattr(
            cfg, 'chemtracer_xml_filename_scratch',
            os.path.join(cfg.icon_input_xml, os.path.basename(cfg.CHEMTRACER_XML_FILENAME))
            )
    if hasattr(cfg, 'PNTSRC_XML_FILENAME'):
        setattr(
            cfg, 'pntSrc_xml_filename_scratch',
            os.path.join(cfg.icon_input_xml, os.path.basename(cfg.PNTSRC_XML_FILENAME))
            )


    # -----------------------------------------------
    # -- Restart directories and files
    # -----------------------------------------------

    # -- Directory to dump a potential restart at the end of the current simulation
    setattr(cfg, 'icon_restart_out', os.path.join(chain_root, 'icon', 'restart'))

    # -- If this simulation is a spinup, must change the initial conditions
    if spinup:
        setattr(cfg, 'lrestart', '.FALSE.')
        setattr(cfg, 'restart_time_interval', 'PT%dH' % (hstop - hstart))   
        setattr(cfg, 'inicond_filename_scratch',
            os.path.join(cfg.icon_input_icbc,
                        (start_time + timedelta(hours=hstart)).strftime('era52icon_R2B04_DOM01_%Y%m%d%H')))


    # -- If this simulation is not a spinup, maybe there is a restart file somewhere
    else:
        job_id_spinup_run = 'spinup_%s_%d_%d' % (inidate_yyyymmddhh, hstart - cfg.SPINUP_TIME, hstart)
        job_id_last_run = 'run_%s_%d_%d' % (inidate_yyyymmddhh, hstart - cfg.RESTART_STEP, hstart)

        chain_root_spinup_run = os.path.join(work_root, cfg.CASENAME, job_id_spinup_run)
        chain_root_last_run = os.path.join(work_root, cfg.CASENAME, job_id_last_run)

        # -- If a spinup exists, then use the associated restart
        if os.path.isdir(chain_root_spinup_run):
            setattr(cfg, 'icon_restart_type', 'spinup')
            setattr(cfg, 'icon_restart_in', os.path.join(chain_root_spinup_run, 'icon', 'restart'))
            setattr(cfg, 'ini_datetime_string',(start_time + 
                    timedelta(hours=hstart) - 
                    timedelta(hours=cfg.SPINUP_TIME)).strftime('%Y-%m-%dT%H:00:00Z'))


            # -- If both directories exist, we must merge the data
            if os.path.isdir(chain_root_last_run):
                restart_name = (start_time + timedelta(hours=hstart)).strftime('restart_%Y%m%dT%H%M%SZ.nc')
                spinup_filepath = os.path.join(cfg.icon_restart_in, restart_name)
                run_filepath = os.path.join(chain_root_last_run, 'icon', 'restart', restart_name)

                ds_spinup = xr.open_dataset(spinup_filepath)
                ds_run = xr.open_dataset(run_filepath)

                # -- Find the right names for the variables
                for var in cfg.VARS_SPINUP_MERGE :
                    var_spinup = [i for i in ds_spinup.data_vars.keys() if i.startswith(var)][0]
                    var_run = [i for i in ds_run.data_vars.keys() if i.startswith(var)][0]
                    ds_spinup[var_spinup] = ds_run[var_run]
                
                # -- Replace the restart file with the new dataset
                os.remove(spinup_filepath)
                ds_spinup.to_netcdf(spinup_filepath)


        # -- If there is no spinup restart, then we have to use the restart from the last run
        elif not os.path.isdir(chain_root_spinup_run) and os.path.isdir(chain_root_last_run):
                setattr(cfg, 'icon_restart_type', 'previous_run')
                setattr(cfg, 'icon_restart_in', os.path.join(chain_root_last_run, 'icon', 'restart')) 
                setattr(cfg, 'ini_datetime_string', start_time.strftime('%Y-%m-%dT%H:00:00Z')) # -- TODO : retrieve the start_time from the config run ?
                job_id_last_run = 'run_%s_%d_%d' % (inidate_yyyymmddhh,
                                                hstart - cfg.RESTART_STEP, hstart)
                chain_root_last_run = os.path.join(work_root, cfg.CASENAME,
                                                job_id_last_run)


        # -- If none exists, there is no restart at all
        else:
            setattr(cfg, 'lrestart', '.FALSE.')
            setattr(cfg, 'restart_time_interval', 'PT%dH' % (hstop - hstart))   
            setattr(cfg, 'icon_restart_type', None)


        # Also ...
        # -- If one of the directories exist, then we can use at least one restart
        # -- (with a restart, inidatetime must be the very beginning of the simulation and not from the restart)
        if os.path.isdir(chain_root_spinup_run) or os.path.isdir(chain_root_last_run):
            setattr(cfg, 'lrestart', '.TRUE.')
            setattr(cfg, 'restart_time_interval', 'PT%dH' % (hstop - hstart + cfg.SPINUP_TIME))   
            setattr(cfg, 'restart_filename_scratch', 
                    os.path.join(cfg.icon_restart_in, (start_time + 
                    timedelta(hours=hstart)).strftime('restart_%Y%m%dT%H%M%SZ.nc')))


    # -----------------------------------------------
    # -- Create running and logging directories
    # -----------------------------------------------

    # -- logging
    log_working_dir = os.path.join(chain_root, 'checkpoints', 'working')
    log_finished_dir = os.path.join(chain_root, 'checkpoints', 'finished')
    setattr(cfg, 'log_working_dir', log_working_dir)
    setattr(cfg, 'log_finished_dir', log_finished_dir)

    # -- Create working dirs
    tools.create_dir(chain_root, "chain_root")
    tools.create_dir(log_working_dir, "log_working")
    tools.create_dir(log_finished_dir, "log_finished")

    # number of levels and switch for unit conversion for 'reduce_output' job
    if not hasattr(cfg, 'OUTPUT_LEVELS'):
        setattr(cfg, 'OUTPUT_LEVELS', -1)
    if not hasattr(cfg, 'CONVERT_GAS'):
        setattr(cfg, 'CONVERT_GAS', True)

    # ------------------------
    # Run jobs (if required)
    # ------------------------
    
    for job in job_names:

        # mapping of scripts in jobs with their arguments
        skip = False

        # if exists job is currently worked on or has been finished
        if os.path.exists(os.path.join(log_working_dir, job)):
            if not force:
                while True:
                    if os.path.exists(os.path.join(log_finished_dir, job)):
                        print('Skip "%s" for chain "%s"' % (job, job_id))
                        skip = True
                        break
                    else:
                        print('Wait for "%s" of chain "%s"' % (job, job_id))
                        sys.stdout.flush()
                        for _ in range(3000):
                            time.sleep(0.1)
            else:
                os.remove(os.path.join(log_working_dir, job))
                try:
                    os.remove(os.path.join(log_finished_dir, job))
                except FileNotFoundError:
                    pass

        if not skip:
            print('Process "%s" for chain "%s"' % (job, job_id))
            sys.stdout.flush()

            try_count = 1 + (args.ntry - 1) * (job == 'cosmo')
            while try_count > 0:
                try_count -= 1
                try:
                    # Change the log file
                    logfile = os.path.join(cfg.log_working_dir, job)
                    logfile_finish = os.path.join(cfg.log_finished_dir, job)
                    tools.change_logfile(logfile)

                    # Launch the job
                    to_call = getattr(jobs, job)
                    to_call.main(start_time, hstart, hstop, cfg)

                    shutil.copy(logfile, logfile_finish)

                    exitcode = 0
                    try_count = 0
                except:
                    subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                        job, job_id)
                    logging.exception(subject)
                    if mail_address:
                        message = tools.prepare_message(
                            os.path.join(log_working_dir, job))
                        logging.info('Sending log file to %s' % mail_address)
                        tools.send_mail(mail_address, subject, message)
                    if try_count == 0:
                        raise RuntimeError(subject)

            if exitcode != 0 or not os.path.exists(
                    os.path.join(log_finished_dir, job)):
                subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                    job, job_id)
                if mail_address:
                    message = tools.prepare_message(
                        os.path.join(log_working_dir, job))
                    logging.info('Sending log file to %s' % mail_address)
                    tools.send_mail(mail_address, subject, message)
                raise RuntimeError(subject)


def restart_run(work_root, cfg, start, hstart, hstop, job_names, force):
    """Starts the subchains in the specified intervals.
    
    Slices the total runtime of the chain according to ``cfg.RESTART_STEP``.
    Calls ``run_chain()`` for each step.
    
    Parameters
    ----------
    work_root : str
        The path to the directory in which the chain writes files during
        execution (typically scratch)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    start : datetime-object
        The startdate
    hstart : int
        Offset (in hours) of the actual start from the startdate (start param)
    hstop : int
        Length of simulation (in hours)
    job_names : list of str
        List of the names of jobs to execute on every timeslice.
        Jobs are .py files in the jobs/ directory with a main() function
        that will be called from run_chain().
    force : bool
        If True will do job regardless of completion status
    """

    # -- Maximum seconds of simulation for output 
    setattr(cfg, 'output_writing_max', hstop - hstart + cfg.SPINUP_TIME)

    # -- Loop over the time steps
    for time in tools.iter_hours(start, hstart, hstop, step=cfg.RESTART_STEP):

        sub_hstart = (time - start).total_seconds() / 3600.
        runtime = min(cfg.RESTART_STEP, hstop - sub_hstart)
        sub_hstop = sub_hstart + runtime

        # -- Don't start the simulation with runtime equal to 0
        if runtime == 0:
            continue

        print("Starting run with starttime {}".format(time))

        # -- Spinup run
        if cfg.SPINUP_TIME > 0:
            run_chain(work_root=work_root,
                    cfg=cfg,
                    start_time=start,
                    hstart=sub_hstart - cfg.SPINUP_TIME,
                    hstop=sub_hstart,
                    job_names=job_names,
                    spinup=True,
                    force=force)

        # -- Real simulation
        run_chain(work_root=work_root,
                  cfg=cfg,
                  start_time=start,
                  hstart=sub_hstart,
                  hstop=sub_hstop,
                  job_names=job_names,
                  spinup=False,
                  force=force)
        

if __name__ == '__main__':

    timer_init = time.time()

    args = parse_arguments()

    # 'empty' config object to be overwritten by load_config_file
    cfg = None
    for casename in args.casenames:
        cfg = load_config_file(casename=casename, cfg=cfg)
        start_time = datetime.strptime(args.startdate, '%Y-%m-%d')
        set_simulation_type(cfg)
        if args.job_list is None:
            args.job_list = default_jobs[cfg.TARGET]

        print("Starting chain for case {}, using {}".format(
            casename, cfg.TARGET.name))

        # -- Create the directory to gather all outputs
        tools.create_dir(os.path.join(cfg.WORK_DIR, cfg.CASENAME, 'chain'), 'chain')

        # -- Run the main code
        restart_run(work_root=cfg.WORK_DIR,
                        cfg=cfg,
                        start=start_time,
                        hstart=args.hstart,
                        hstop=args.hstop,
                        job_names=args.job_list,
                        force=args.force)

        # -- Move all outputs from different restart steps into the same directory 'chain'
        list_dirs = glob.glob(os.path.join(cfg.WORK_DIR, cfg.CASENAME, 'run_*'))
        for direc in list_dirs:
            if os.path.isdir(direc):
                list_files = glob.glob(os.path.join(direc, 'icon', 'output', '*'))
                for file in list_files:
                    shutil.move(file, os.path.join(cfg.WORK_DIR, cfg.CASENAME, 'chain'))

    print('>>> finished chain for good or bad! <<<')
    print("--- %s seconds ---" % (time.time() - timer_init))


#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta

import importlib
import logging
import os
import sys
import time
import shutil
import argparse
import csv
import yaml

import jobs
from jobs import tools


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

    jobs_help = ("List of job-names to be executed. A job is a .py-"
                 "file in jobs/ with a main()-function which "
                 "handles one aspect of the processing chain, for "
                 "example copying meteo-input data or launching a "
                 "job for int2lm. "
                 "Jobs are executed in the order in which they are "
                 "given here. "
                 "If no jobs are given, default jobs will be executed"
                 "as defined in config/models.yaml.")
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

    return args


class Config():

    def __init__(self, casename):
        # Global attributes (initialized with default values)
        self.user_name = os.environ['USER']
        self.set_email()
        self.casename = casename
        self.set_account()

        self.chain_src_dir = os.getcwd()
        self.case_path = os.path.join(self.chain_src_dir, 'cases',
                                      self.casename)
        self.work_root = os.path.join(self.chain_src_dir, 'work')

        # User-defined attributes from config file
        self.load_config_file(casename)

        # Specific settings based on the node type ('gpu' or 'mc')
        self.set_node_info()

    def load_config_file(self, casename):
        """
        Load the configuration settings from a YAML file.

        This method reads the configuration settings from a YAML file located in
        the 'cases/casename' directory and sets them as attributes of the instance.

        Parameters:
        - casename (str): Name of the folder in 'cases/' where the configuration
          files are stored.

        Returns:
        - self (Config): The same `Config` instance with configuration settings as
          attributes.
        """

        cfg_file = os.path.join('cases', casename, 'config.yaml')

        if not os.path.isfile(cfg_file):
            all_cases = [
                path.name for path in os.scandir('cases') if path.is_dir()
            ]
            closest_name = min([(tools.levenshtein(casename, name), name)
                                for name in all_cases],
                               key=lambda x: x[0])[1]
            raise FileNotFoundError(
                f"Case-directory '{casename}' not found, did you mean '{closest_name}'?"
            )

        try:
            with open(cfg_file, 'r') as yaml_file:
                cfg_data = yaml.load(yaml_file, Loader=yaml.FullLoader)
        except FileNotFoundError:
            raise FileNotFoundError(
                f"No file 'config.yaml' in {os.path.dirname(cfg_file)}")

        # Directly assign values to instance attributes
        for key, value in cfg_data.items():
            setattr(self, key, value)

        return self

    def set_account(self):
        if self.user_name == 'jenkins':
            # g110 account for Jenkins testing
            self.compute_account = 'g110'
        elif os.path.exists(os.environ['HOME'] + '/.acct'):
            # Use account specified in ~/.acct file
            with open(os.environ['HOME'] + '/.acct', 'r') as file:
                self.compute_account = file.read().rstrip()
        else:
            # Use standard account
            self.compute_account = os.popen("id -gn").read().splitlines()[0]

        return self

    def set_node_info(self):
        if self.constraint == 'gpu':
            self.ntasks_per_node = 12
            self.mpich_cuda = ('export MPICH_RDMA_ENABLED_CUDA=1\n'
                               'export MPICH_G2G_PIPELINE=256\n'
                               'export CRAY_CUDA_MPS=1\n')
        elif self.constraint == 'mc':
            self.ntasks_per_node = 36
            self.mpich_cuda = ''
        else:
            raise ValueError(
                "Invalid value for 'constraint' in the configuration."
                "It should be either 'gpu' or 'mc'.")

        return self

    def set_email(self):
        if self.user_name == 'jenkins':
            self.user_mail = None
        elif os.path.exists(os.environ['HOME'] + '/.forward'):
            with open(os.environ['HOME'] + '/.forward', 'r') as file:
                self.user_mail = file.read().rstrip()
        else:
            self.user_mail = None

        return self

    def print_config(self):
        # Print the configuration
        # max_col_width = max(len(key) for key in vars(self)) + 1
        max_col_width = 27

        print("\nConfiguration:")
        print(f"{'Attribute':<{max_col_width}} Type Value")
        print("-" * 80)
        for key, value in vars(self).items():
            if isinstance(value, list):
                # If the value is a list, format it with indentation
                print(f"{key:<{max_col_width}} list")
                for item in value:
                    item_type = type(item).__name__
                    print(f"  - {item:<{max_col_width-4}} {item_type}")
            elif isinstance(value, dict):
                # If the value is a dictionary, format it as before
                print(f"{key:<{max_col_width}} dict")
                for sub_key, sub_value in value.items():
                    sub_value_type = type(sub_value).__name__
                    print(
                        f"  - {sub_key:<{max_col_width-4}} {sub_value_type:<4} {sub_value}"
                    )
            else:
                # Standard output
                key_type = type(key).__name__
                print(f"{key:<{max_col_width}} {key_type:<4} {value}")


def run_chain(work_root, model_cfg, cfg, start_time, hstart, hstop, job_names,
              force):
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

    # ini date and forecast time (ignore meteo times)
    inidate_yyyymmddhh = start_time.strftime('%Y%m%d%H')
    inidate_yyyymmdd_hh = start_time.strftime('%Y%m%d_%H')
    setattr(cfg, 'inidate_yyyymmddhh', inidate_yyyymmddhh)
    setattr(cfg, 'inidate_yyyymmdd_hh', inidate_yyyymmdd_hh) # only for icon-art-oem
    setattr(cfg, 'hstart', hstart)
    setattr(cfg, 'hstop', hstop)
    forecasttime = '%d' % (hstop - hstart)

    # Folder naming and structure
    job_id = '%s_%d_%d' % (cfg.inidate_yyyymmddhh, cfg.hstart, cfg.hstop)
    chain_root = os.path.join(work_root, cfg.casename, job_id)
    setattr(cfg, 'job_id', job_id)
    setattr(cfg, 'chain_root', chain_root)

    if hasattr(cfg, 'spinup'):
        if cfg.first_one:  # first run in spinup
            chain_root_last_run = ''
        else:  # consecutive runs in spinup
            inidate_yyyymmddhh_spinup = (
                start_time - timedelta(hours=cfg.spinup)).strftime('%Y%m%d%H')
            setattr(cfg, 'inidate_yyyymmddhh', inidate_yyyymmddhh_spinup)
            setattr(cfg, 'hstart', 0)
            setattr(cfg, 'hstop', hstop + cfg.spinup)
            forecasttime = '%d' % (hstop + cfg.spinup)
            inidate_yyyymmddhh_last_run = (
                start_time -
                timedelta(hours=cfg.restart_step)).strftime('%Y%m%d%H')
            if cfg.second_one:  # second run (i.e., get job_id from first run)
                job_id_last_run = '%s_%d_%d' % (inidate_yyyymmddhh_last_run, 0,
                                                hstop)
            else:  # all other runs
                job_id_last_run = '%s_%d_%d' % (inidate_yyyymmddhh_last_run,
                                                0 - cfg.spinup, hstop)
            chain_root_last_run = os.path.join(work_root, cfg.casename,
                                               job_id_last_run)

    setattr(cfg, 'forecasttime', forecasttime)


    if hasattr(cfg, 'constraint'):
        assert cfg.constraint in ['gpu', 'mc'], ("Unknown constraint, use"
                                                 "gpu or mc")

    # Spinup
    if hasattr(cfg, 'spinup'):
        setattr(cfg, 'last_cosmo_output',
                os.path.join(chain_root_last_run, 'cosmo', 'output'))
        # No restart for spinup simulations (= default values for no restart)
        setattr(cfg, 'cosmo_restart_out', '')
        setattr(cfg, 'cosmo_restart_in', '')
    elif 'restart' in model_cfg['models'][cfg.model]['features']:
        job_id_last_run = '%s_%d_%d' % (cfg.inidate_yyyymmddhh,
                                        hstart - cfg.restart_step, hstart)
        chain_root_last_run = os.path.join(work_root, cfg.casename,
                                           job_id_last_run)
        # Set restart directories
        setattr(cfg, 'cosmo_restart_out',
                os.path.join(chain_root, 'cosmo', 'restart'))
        setattr(cfg, 'cosmo_restart_in',
                os.path.join(chain_root_last_run, 'cosmo', 'restart'))

    # Restart step
    if 'restart' in model_cfg['models'][cfg.model]['features']:
        setattr(cfg, 'restart_step', hstop - hstart)

    # if nested run: use output of mother-simulation
    if 'nesting' in model_cfg['models'][
            cfg.model]['features'] and not os.path.isdir(cfg.meteo.dir):
        # if ifs_hres_dir doesn't point to a directory,
        # it is the name of the mother run
        mother_name = cfg.meteo.dir
        cfg.meteo.dir = os.path.join(work_root, mother_name, job_id, 'cosmo',
                                     'output')
        cfg.meteo.inc = 1
        cfg.meteo.prefix = 'lffd'

    # ICON
    if cfg.model.startswith('icon'):
        setattr(cfg, 'icon_base', os.path.join(chain_root, 'icon'))
        setattr(cfg, 'icon_input', os.path.join(chain_root, 'icon', 'input'))
        setattr(cfg, 'icon_input_icbc',
                os.path.join(chain_root, 'icon', 'input', 'icbc'))
        setattr(cfg, 'icon_input_oae',
                os.path.join(chain_root, 'icon', 'input', 'OEM'))
        setattr(cfg, 'icon_input_grid',
                os.path.join(chain_root, 'icon', 'input', 'grid'))
        setattr(cfg, 'icon_input_mapping',
                os.path.join(chain_root, 'icon', 'input', 'mapping'))
        setattr(cfg, 'icon_input_rad',
                os.path.join(chain_root, 'icon', 'input', 'rad'))
        setattr(cfg, 'icon_input_xml',
                os.path.join(chain_root, 'icon', 'input', 'XML'))
        setattr(cfg, 'icon_work', os.path.join(chain_root, 'icon', 'run'))
        setattr(cfg, 'icon_output', os.path.join(chain_root, 'icon', 'output'))
        setattr(cfg, 'icon_output_reduced',
                os.path.join(chain_root, 'icon', 'output_reduced'))

        for varname in cfg.input_files:
            file_info = cfg.input_files[varname]
            setattr(cfg, varname,
                    os.path.join(cfg.input_root, file_info[1], file_info[0]))
            setattr(cfg, f'{varname}_scratch',
                    os.path.join(cfg.icon_input, file_info[1], file_info[0]))
        ini_datetime_string = (
            start_time +
            timedelta(hours=hstart)).strftime('%Y-%m-%dT%H:00:00Z')
        end_datetime_string = (
            start_time + timedelta(hours=hstart) +
            timedelta(hours=hstop)).strftime('%Y-%m-%dT%H:00:00Z')
        setattr(cfg, 'ini_datetime_string', ini_datetime_string)
        setattr(cfg, 'end_datetime_string', end_datetime_string)
        # Set restart directories
        setattr(cfg, 'icon_restart_out',
                os.path.join(chain_root, 'icon', 'restart'))
        setattr(cfg, 'icon_restart_in',
                os.path.join(chain_root_last_run, 'icon', 'restart'))
        # TODO: Set correct restart setting
        setattr(cfg, 'lrestart', '.FALSE.')

    # logging
    log_working_dir = os.path.join(chain_root, 'checkpoints', 'working')
    log_finished_dir = os.path.join(chain_root, 'checkpoints', 'finished')
    setattr(cfg, 'log_working_dir', log_working_dir)
    setattr(cfg, 'log_finished_dir', log_finished_dir)

    # create working dirs
    tools.create_dir(chain_root, "chain_root")
    tools.create_dir(log_working_dir, "log_working")
    tools.create_dir(log_finished_dir, "log_finished")

    # number of levels and switch for unit conversion for 'reduce_output' job
    if not hasattr(cfg, 'output_levels'):
        setattr(cfg, 'output_levels', -1)
    if not hasattr(cfg, 'convert_gas'):
        setattr(cfg, 'convert_gas', True)

    # run jobs (if required)
    for job in job_names:
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
                    to_call.main(start_time, hstart, hstop, cfg, model_cfg)

                    shutil.copy(logfile, logfile_finish)

                    exitcode = 0
                    try_count = 0
                except:
                    subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                        job, job_id)
                    logging.exception(subject)
                    if cfg.user_mail:
                        message = tools.prepare_message(
                            os.path.join(log_working_dir, job))
                        logging.info('Sending log file to %s' % cfg.user_mail)
                        tools.send_mail(cfg.user_mail, subject, message)
                    if try_count == 0:
                        raise RuntimeError(subject)

            if exitcode != 0 or not os.path.exists(
                    os.path.join(log_finished_dir, job)):
                subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                    job, job_id)
                if cfg.user_mail:
                    message = tools.prepare_message(
                        os.path.join(log_working_dir, job))
                    logging.info('Sending log file to %s' % cfg.user_mail)
                    tools.send_mail(cfg.user_mail, subject, message)
                raise RuntimeError(subject)


def restart_runs(work_root, model_cfg, cfg, start, hstart, hstop, job_names,
                 force):
    """Starts the subchains in the specified intervals.
    
    Slices the total runtime of the chain according to ``cfg.restart_step``.
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
    # run restarts
    for time in tools.iter_hours(start, hstart, hstop, cfg.restart_step):
        sub_hstart = (time - start).total_seconds() / 3600.0
        runtime = min(cfg.restart_step, hstop - sub_hstart)
        if runtime == 0:
            # don't start simuation with 0 runtime
            continue
        sub_hstop = sub_hstart + runtime

        print("Starting run with starttime {}".format(time))

        run_chain(work_root=work_root,
                  model_cfg=model_cfg,
                  cfg=cfg,
                  start_time=start,
                  hstart=sub_hstart,
                  hstop=sub_hstop,
                  job_names=job_names,
                  force=force)


def restart_runs_spinup(work_root, model_cfg, cfg, start, hstart, hstop,
                        job_names, force):
    """Starts the subchains in the specified intervals.
    
    Slices the total runtime of the chain according to ``cfg.restart_step``.
    Calls ``run_chain()`` for each step.

    Runs custom "restarts" (= simulations with spin-up and tracer recycling).
    The first simulation is a normal one, with ``run_time = cfg.restart_step``.
    Consecutive simulations start at
    ``start + N * cfg.restart_step - cfg.spinup``.
    
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

    for time in tools.iter_hours(start, hstart, hstop, cfg.restart_step):
        print(time)
        if time == start:
            setattr(cfg, "first_one", True)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.FALSE.')
            run_time = min(cfg.restart_step, hstop - hstart)
            print('First simulation')
        elif time == start + timedelta(hours=cfg.restart_step):
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", True)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = min(cfg.restart_step + cfg.spinup, hstop - hstart)
            print('Second simulation')
        else:
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = min(cfg.restart_step + cfg.spinup, hstop - hstart)

        if run_time == 0:
            # don't start simuation with 0 runtime
            continue

        endtime_act_sim = time - timedelta(hours=cfg.restart_step) \
                               + timedelta(hours=run_time)
        if endtime_act_sim > start + timedelta(hours=hstop):
            continue

        print(f'Runtime of sub-simulation: {run_time} h')

        if cfg.first_one:
            run_chain(work_root=work_root,
                      model_cfg=model_cfg,
                      cfg=cfg,
                      start_time=time,
                      hstart=0,
                      hstop=run_time,
                      job_names=job_names,
                      force=force)
        else:
            run_chain(work_root=work_root,
                      model_cfg=model_cfg,
                      cfg=cfg,
                      start_time=time,
                      hstart=-cfg.spinup,
                      hstop=run_time - cfg.spinup,
                      job_names=job_names,
                      force=force)


def load_model_config_yaml(yamlfile):
    with open(yamlfile) as file:
        model_cfg = yaml.safe_load(file)
    return model_cfg


if __name__ == '__main__':
    args = parse_arguments()

    for casename in args.casenames:
        # Load configs
        model_cfg = load_model_config_yaml('config/models.yaml')
        cfg = Config(casename)

        # Print config
        cfg.print_config()

        # Check if jobs are set or if default ones are used
        if args.job_list is None:
            args.job_list = model_cfg['models'][cfg.model]['jobs']

        print(f"Starting chain for case {casename} and model {cfg.model}")

        # check for restart compatibility and spinup
        if 'restart' in model_cfg['models'][cfg.model]['features']:
            if hasattr(cfg, 'spinup'):
                print("This is a spinup simulation.")
                restart_runs_spinup(work_root=cfg.work_root,
                                    model_cfg=model_cfg,
                                    cfg=cfg,
                                    start=cfg.startdate,
                                    hstart=cfg.hstart,
                                    hstop=cfg.hstop,
                                    job_names=args.job_list,
                                    force=args.force)
            else:
                print("Built-in model restart is used.")
                restart_runs(work_root=cfg.work_root,
                             model_cfg=model_cfg,
                             cfg=cfg,
                             start=cfg.startdate,
                             hstart=cfg.hstart,
                             hstop=cfg.hstop,
                             job_names=args.job_list,
                             force=args.force)
        else:
            print("No restart is used.")
            run_chain(work_root=cfg.work_root,
                      cfg=cfg,
                      start_time=cfg.startdate,
                      hstart=cfg.hstart,
                      hstop=cfg.hstop,
                      job_names=args.job_list,
                      force=args.force)

    print('>>> finished chain for good or bad! <<<')

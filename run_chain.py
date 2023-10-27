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
            if self.model.startswith('icon'):
                if self.run_on == 'gpu':
                    self.ntasks_per_node = 1
                elif self.run_on == 'cpu':
                    self.ntasks_per_node = 12
                else:
                    raise ValueError(
                        "Invalid value for 'run_on' in the configuration."
                        "It should be either 'gpu' or 'cpu'.")
            else:
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

    def set_restart_step_hours(self):
        self.restart_step_hours = int(
            tools.iso8601_duration_to_hours(self.restart_step))

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

    def convert_paths_to_absolute(self):
        # Loop through all variables and their dictionary entries
        for attr_name, attr_value in self.__dict__.items():
            if isinstance(attr_value, str):
                if os.path.isabs(attr_value):
                    # If the value is already an absolute path, continue to the next iteration
                    continue
                # Convert relative paths to absolute paths
                if attr_value.startswith('./'):
                    self.__dict__[attr_name] = os.path.abspath(attr_value)
            elif isinstance(attr_value, dict):
                # If the attribute is a dictionary, loop through its entries
                for key, value in attr_value.items():
                    if isinstance(value, str):
                        if os.path.isabs(value):
                            # If the value is already an absolute path, continue to the next iteration
                            continue
                        # Convert relative paths to absolute paths
                        if value.startswith('./'):
                            self.__dict__[attr_name][key] = os.path.abspath(
                                value)

        return self

    def create_vars_from_dicts(self):
        # Create a copy of the object's __dict__ to avoid modifying it during iteration
        object_dict = vars(self).copy()

        for key, value in object_dict.items():
            if isinstance(value, dict):
                for sub_key, sub_value in value.items():
                    setattr(self, key + '_' + sub_key, sub_value)
        return self


def run_chain(work_root, model_cfg, cfg, startdate_sim, enddate_sim, job_names,
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

    # Write current start and end dates to config variables
    cfg.startdate_sim = startdate_sim
    cfg.enddate_sim = enddate_sim

    # Set forecast time
    cfg.forecasttime = (cfg.enddate_sim - cfg.startdate_sim).total_seconds() / 3600

    # String variables for startdate_sim
    cfg.startdate_sim_yyyymmddhh = startdate_sim.strftime('%Y%m%d%H')
    cfg.enddate_sim_yyyymmddhh = enddate_sim.strftime('%Y%m%d%H')

    # Folder naming and structure
    cfg.job_id = f'{cfg.startdate_sim_yyyymmddhh}_{cfg.enddate_sim_yyyymmddhh}'
    cfg.chain_root = os.path.join(work_root, cfg.casename, cfg.job_id)

    # Config variables for spinup runs (datetimes, job-id, etc.)
    if hasattr(cfg, 'spinup'):
        if cfg.first_one:  # first run in spinup
            cfg.chain_root_prev = None
        else:  # consecutive runs in spinup
            cfg.startdate_sim_yyyymmddhh = cfg.startdate_sim.strftime('%Y%m%d%H')
            enddate_sim_yyyymmddhh_prev = (cfg.enddate_sim - timedelta(hours=cfg.restart_step_hours)).strftime('%Y%m%d%H')

            if cfg.second_one:  # second run (i.e., get job_id from first run)
                startdate_sim_yyyymmddhh_prev = (cfg.enddate_sim - timedelta(hours=2*cfg.restart_step_hours)).strftime('%Y%m%d%H')
            else:  # all other runs (i.e., get job_id from previous run)
                startdate_sim_yyyymmddhh_prev = (cfg.enddate_sim - timedelta(hours=2*cfg.restart_step_hours+cfg.spinup)).strftime('%Y%m%d%H')

            cfg.job_id_prev = f'{startdate_sim_yyyymmddhh_prev}_{enddate_sim_yyyymmddhh_prev}'
            cfg.chain_root_prev = os.path.join(work_root, cfg.casename,
                                                   cfg.job_id_prev)
        cfg.last_cosmo_output = os.path.join(cfg.chain_root_prev, 'cosmo',
                                             'output')

        # No restart for spinup simulations (= default values for no restart)
        cfg.cosmo_restart_out = ''
        cfg.cosmo_restart_in = ''
    elif 'restart' in model_cfg['models'][cfg.model]['features']:
        cfg.startdate_sim_prev = cfg.startdate_sim - timedelta(
            hours=cfg.restart_step_hours)
        cfg.enddate_sim_prev = cfg.enddate_sim - timedelta(
            hours=cfg.restart_step_hours)
        cfg.job_id_prev = f'{cfg.startdate_sim_prev_yyyymmddhh}_{cfg.enddate_sim_prev_yyyymmddhh}'
        cfg.chain_root_prev = os.path.join(work_root, cfg.casename,
                                               cfg.job_id_prev)

        # Set restart directories
        cfg.cosmo_restart_out = os.path.join(cfg.chain_root, 'cosmo',
                                             'restart')
        cfg.cosmo_restart_in = os.path.join(cfg.chain_root_prev, 'cosmo',
                                            'restart')

    # Check constraint
    if hasattr(cfg, 'constraint'):
        assert cfg.constraint in ['gpu', 'mc'], ("Unknown constraint, use"
                                                 "gpu or mc")

    # If nested run: use output of mother-simulation
    if 'nesting' in model_cfg['models'][
            cfg.model]['features'] and not os.path.isdir(cfg.meteo.dir):
        # if ifs_hres_dir doesn't point to a directory,
        # it is the name of the mother run
        mother_name = cfg.meteo.dir
        cfg.meteo.dir = os.path.join(work_root, mother_name, cfg.job_id,
                                     'cosmo', 'output')
        cfg.meteo.inc = 1
        cfg.meteo.prefix = 'lffd'

    # Logging
    log_working_dir = os.path.join(cfg.chain_root, 'checkpoints', 'working')
    log_finished_dir = os.path.join(cfg.chain_root, 'checkpoints', 'finished')
    setattr(cfg, 'log_working_dir', log_working_dir)
    setattr(cfg, 'log_finished_dir', log_finished_dir)

    # Create working directories
    tools.create_dir(cfg.chain_root, "chain_root")
    tools.create_dir(log_working_dir, "log_working")
    tools.create_dir(log_finished_dir, "log_finished")

    # Number of levels and switch for unit conversion for 'reduce_output' job
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
                        print('Skip "%s" for chain "%s"' % (job, cfg.job_id))
                        skip = True
                        break
                    else:
                        print('Wait for "%s" of chain "%s"' %
                              (job, cfg.job_id))
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
            print('Process "%s" for chain "%s"' % (job, cfg.job_id))
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
                    to_call.main(cfg.startdate_sim, enddate_sim, cfg, model_cfg)

                    shutil.copy(logfile, logfile_finish)

                    exitcode = 0
                    try_count = 0
                except:
                    subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (
                        job, cfg.job_id)
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
                    job, cfg.job_id)
                if cfg.user_mail:
                    message = tools.prepare_message(
                        os.path.join(log_working_dir, job))
                    logging.info('Sending log file to %s' % cfg.user_mail)
                    tools.send_mail(cfg.user_mail, subject, message)
                raise RuntimeError(subject)


def restart_runs(work_root, model_cfg, cfg, job_names, force):
    """Starts the subchains in the specified intervals.
    
    Slices the total runtime of the chain according to ``cfg.restart_step_hours``.
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
    for startdate_sim in tools.iter_hours(cfg.startdate, cfg.enddate, cfg.restart_step_hours):
        startdate_sim = startdate_sim
        enddate_sim = startdate_sim + timedelta(hours=cfg.restart_step_hours)
        runtime_sim = (enddate_sim - startdate_sim).total_seconds() / 3600

        if enddate_sim > cfg.enddate:
            continue

        # Set restart variable (only takes effect for ICON)
        if startdate_sim == cfg.startdate:
            setattr(cfg, "lrestart", '.FALSE.')
        else:
            setattr(cfg, "lrestart", '.TRUE.')

        print("Starting run with startdate {}".format(startdate_sim))

        run_chain(work_root=work_root,
                  model_cfg=model_cfg,
                  cfg=cfg,
                  startdate_sim=startdate_sim,
                  enddate_sim=enddate_sim,
                  job_names=job_names,
                  force=force)


def restart_runs_spinup(work_root, model_cfg, cfg, job_names, force):
    """Starts the subchains in the specified intervals.
    
    Slices the total runtime of the chain according to ``cfg.restart_step_hours``.
    Calls ``run_chain()`` for each step.

    Runs custom "restarts" (= simulations with spin-up and tracer recycling).
    The first simulation is a normal one, with ``run_time = cfg.restart_step_hours``.
    Consecutive simulations start at
    ``start + N * cfg.restart_step_hours - cfg.spinup``.
    
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

    for startdate_sim in tools.iter_hours(cfg.startdate, cfg.enddate, cfg.restart_step_hours):
        if startdate_sim == cfg.startdate:
            setattr(cfg, "first_one", True)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.FALSE.')
            run_time = cfg.restart_step_hours
            startdate_sim_spinup = startdate_sim
        elif startdate_sim == cfg.startdate + timedelta(hours=cfg.restart_step_hours-cfg.spinup):
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", True)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = cfg.restart_step_hours + cfg.spinup
            startdate_sim_spinup = startdate_sim - timedelta(cfg.spinup)
        else:
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = cfg.restart_step_hours + cfg.spinup
            startdate_sim_spinup = startdate_sim - timedelta(cfg.spinup)

        # If current enddate is later than global enddate, skip
        enddate_sim = startdate_sim + timedelta(hours=cfg.restart_step_hours)
        if enddate_sim > cfg.enddate:
            continue

        print(f'Runtime of sub-simulation: {run_time} h')

        run_chain(work_root=work_root,
                  model_cfg=model_cfg,
                  cfg=cfg,
                  startdate_sim=startdate_sim_spinup,
                  enddate_sim=enddate_sim,
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

        # Convert relative to absolute paths
        cfg.convert_paths_to_absolute()

        # Set restart step in hours
        cfg.set_restart_step_hours()

        # Print config before duplication of dict variables
        cfg.print_config()

        # Duplicate variables in the form of <dict>_<value> for better
        # access within namelist template.
        # E.g.: cfg.meteo['dir'] -> cfg.meteo_dir
        cfg.create_vars_from_dicts()

        # Check if jobs are set or if default ones are used
        if args.job_list is None:
            args.job_list = model_cfg['models'][cfg.model]['jobs']

        print(f"Starting chain for case {casename} and model {cfg.model}")

        # check for restart compatibility and spinup
        if 'restart' in model_cfg['models'][cfg.model]['features']:
            if hasattr(cfg, 'spinup'):
                print("Spinup restart is used.")
                restart_runs_spinup(work_root=cfg.work_root,
                                    model_cfg=model_cfg,
                                    cfg=cfg,
                                    job_names=args.job_list,
                                    force=args.force)
            else:
                print("Built-in model restart is used.")
                restart_runs(work_root=cfg.work_root,
                             model_cfg=model_cfg,
                             cfg=cfg,
                             job_names=args.job_list,
                             force=args.force)
        else:
            print("No restart is used.")
            run_chain(work_root=cfg.work_root,
                      cfg=cfg,
                      startdate_sim=cfg.startdate,
                      enddate_sim=cfg.enddate,
                      job_names=args.job_list,
                      force=args.force)

    print('>>> finished chain for good or bad! <<<')

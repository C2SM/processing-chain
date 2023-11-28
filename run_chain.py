#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta

import logging
import os
import sys
import time
import shutil
import argparse
import yaml

import jobs
from jobs import tools


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


class Config():

    def __init__(self, casename):
        """Initialize an instance of the Config class.

        Initializes an instance of the Config class with user-specific
        and default attributes. The class represents a processing chain for a
        particular case, and its attributes are populated based on the provided
        `casename`.

        Parameters
        ----------
        casename : str
            The identifier for the case, typically specifying the configuration
            and settings to be used in the processing chain.

        Attributes
        ----------
        user_name : str
            The username of the current user, obtained from the 'USER' environment variable.
        email : str
            The user's email address, initially set to None and updated using the `set_email` method.
        casename : str
            The specified case name for the processing chain.
        chain_src_dir : str
            The source directory for the processing chain, typically the current working directory.
        case_path : str
            The path to the case directory under 'cases/' for the specified `casename`.
        work_root : str
            The root directory for processing chain execution, typically located under the source directory.

        Notes
        -----
        The method also loads user-defined attributes from the configuration file,
        sets specific settings based on the node type ('gpu' or 'mc'), and initializes
        other instance-specific attributes.
        """
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

        # Set workflow
        with open('config/workflows.yaml') as file:
            workflows = yaml.safe_load(file)
        self.workflow = workflows[self.workflow_name]

        # Set if async
        self.async = 'dependencies' in self.workflow
        
        # Initiate empty job ids dictionnary so that it can be filled in later
        self.job_ids = {'current': {}, 'previous': {}}

    def load_config_file(self, casename):
        """Load configuration settings from a YAML file and set them as attributes.

        This method reads the configuration settings from a YAML file located in
        the 'cases/casename' directory and sets them as attributes of the instance.

        Parameters
        ----------
        casename : str
            Name of the folder in 'cases/' where the configuration files are stored.

        Returns
        -------
        Config
            The same `Config` instance with configuration settings as attributes.

        Raises
        ------
        FileNotFoundError
            If the specified configuration file or case directory is not found.

        Notes
        -----
        If the configuration file does not exist, the method will attempt to suggest
        a similar case directory based on a Levenshtein distance comparison with
        existing case directories. The method directly assigns values from the
        configuration file to instance attributes for easy access.
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

        # rename the workflow attribute to avoid name clash
        self.workflow_name = self.workflow

        return self

    def set_account(self):
        """Set the compute account based on user information.

        This method determines the compute account to be used based on the user's
        name and system configuration.

        Returns
        -------
        Config
            The same `Config` instance with the `compute_account` attribute set.

        Notes
        -----
        - If the user name is 'jenkins', the compute account is set to 'g110' for
        Jenkins testing.
        - If an account is specified in the user's '~/.acct' file, it will be used
        as the compute account.
        - If neither of the above conditions is met, the standard account is
        determined using the 'id -gn' command.
        """
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
        """Set node-specific information based on configuration settings.

        This method configures node-specific settings, such as the number of tasks
        per node and CUDA-related environment variables, based on the provided
        configuration settings in the instance.

        Returns
        -------
        Config
            The same `Config` instance with updated node-specific attributes.

        Raises
        ------
        ValueError
            If the 'constraint' or 'run_on' configuration values are invalid.
        """
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
        """Set the restart step in hours.

        Converts the 'restart_step' attribute, which is in ISO8601 duration format,
        to hours and stores the result in the 'restart_step_hours' attribute.

        Returns
        -------
        Config
            The same `Config` instance with the 'restart_step_hours' attribute set.
        """
        self.restart_step_hours = int(
            tools.iso8601_duration_to_hours(self.restart_step))

        return self

    def set_email(self):
        """Set the user's email address based on system configuration.

        This method determines the user's email address based on the user's name
        and system configuration.

        Returns
        -------
        Config
            The same `Config` instance with the `user_mail` attribute set.

        Notes
        -----
        - If the user name is 'jenkins', the user's email address is set to None.
        - If an email address is specified in the user's '~/.forward' file, it will
        be used as the user's email address.
        - If neither of the above conditions is met, the user's email address is set
        to None.
        """
        if self.user_name == 'jenkins':
            self.user_mail = None
        elif os.path.exists(os.environ['HOME'] + '/.forward'):
            with open(os.environ['HOME'] + '/.forward', 'r') as file:
                self.user_mail = file.read().rstrip()
        else:
            self.user_mail = None

        return self

    def print_config(self):
        """Print the configuration attributes and their values.

        This method displays the configuration attributes and their corresponding
        values in a formatted manner. Lists and dictionaries within the configuration
        are also displayed with appropriate indentation.

        Notes
        -----
        - The maximum column width for the attribute names is automatically determined.
        - The method prints the attribute name, its type, and its value.
        - If an attribute is a list, it is displayed with each item indented.
        - If an attribute is a dictionary, it is also displayed with each key-value
        pair indented.
        """
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
        """Convert relative file paths to absolute paths in the configuration.

        This method iterates through all variables and their dictionary entries in
        the configuration and checks for string values that represent file paths.
        If a file path is relative (starts with './'), it is converted to an
        absolute path using `os.path.abspath`.

        Returns
        -------
        Config
            The same `Config` instance with relative file paths converted to absolute paths.
        """
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
        """Create instance attributes from dictionary entries in the configuration.

        This method iterates through the instance's attribute dictionary and checks
        for dictionary values. For each dictionary encountered, it creates new
        instance attributes by concatenating the original attribute name and the
        dictionary key, and assigns the corresponding values.

        Returns
        -------
        Config
            The same `Config` instance with new attributes created from dictionary entries.
        """
        # Create a copy of the object's __dict__ to avoid modifying it during iteration
        object_dict = vars(self).copy()

        for key, value in object_dict.items():
            if isinstance(value, dict):
                for sub_key, sub_value in value.items():
                    setattr(self, key + '_' + sub_key, sub_value)
        return self

    def get_dep_cmd(self, job_name):
        """Generate the part of the sbatch command that sepcifies dependencies for job_name.

        Returns
        -------
        str
            The relevant part of the sbatch command, can be None
        """

        if self.async:
            # async case
            if deps := dep_dict.get(job_name):
                # job_name has dependencies
                deps_ids = []
                # Get job ids of previous and current dependencies
                for stage in 'previous', 'current':
                    if dep_stage := deps.get(stage):
                        for job in dep_stage:
                            deps_ids.extend(self.job_ids[stage][job])
                dep_str = ':'.join(map(str, deps_ids))
                return f'--dependency=afterok:{dep_str}'
            else:
                # job_name has no dependencies but still belongs to an async workflow
                # so don't use --wait
                return 'None
        else:
            # sequential case
            return '--wait'


def run_chain(work_root, cfg, startdate_sim, enddate_sim, job_names,
              force, resume):
    """Run the processing chain, managing job execution and logging.

    This function sets up and manages the execution of a processing chain, handling
    job execution, logging, and various configuration settings.

    Parameters
    ----------
    work_root : str
        The path to the directory where the processing chain writes files during execution.
    cfg : Config
        Object holding user-defined configuration parameters as attributes.
    startdate_sim : datetime-object
        The start date of the simulation.
    enddate_sim : datetime-object
        The end date of the simulation.
    job_names : list of str
        List of names of jobs to execute on every timeslice.
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
    # Write current start and end dates to config variables
    cfg.startdate_sim = startdate_sim
    cfg.enddate_sim = enddate_sim

    # Set forecast time
    cfg.forecasttime = (cfg.enddate_sim -
                        cfg.startdate_sim).total_seconds() / 3600

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
            cfg.startdate_sim_yyyymmddhh = cfg.startdate_sim.strftime(
                '%Y%m%d%H')
            enddate_sim_yyyymmddhh_prev = (
                cfg.enddate_sim -
                timedelta(hours=cfg.restart_step_hours)).strftime('%Y%m%d%H')

            if cfg.second_one:
                startdate_sim_yyyymmddhh_prev = (cfg.enddate_sim - timedelta(
                    hours=2 * cfg.restart_step_hours)).strftime('%Y%m%d%H')
            else:  # all other runs (i.e., get job_id from previous run)
                startdate_sim_yyyymmddhh_prev = (
                    cfg.enddate_sim -
                    timedelta(hours=2 * cfg.restart_step_hours +
                              cfg.spinup)).strftime('%Y%m%d%H')

            cfg.job_id_prev = f'{startdate_sim_yyyymmddhh_prev}_{enddate_sim_yyyymmddhh_prev}'
            cfg.chain_root_prev = os.path.join(work_root, cfg.casename,
                                               cfg.job_id_prev)
            cfg.last_cosmo_output = os.path.join(cfg.chain_root_prev, 'cosmo',
                                                 'output')

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
    if 'nesting' in cfg.workflow['features'] and not os.path.isdir(cfg.meteo.dir):
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
                    elif resume:
                        resume = False
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
                    to_call.main(cfg)

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


def restart_runs(work_root, cfg, job_names, force, resume):
    """Start subchains in specified intervals and manage restarts.

    This function slices the total runtime of the processing chain according to the
    `cfg.restart_step_hours` configuration. It calls `run_chain()` for each
    specified interval.

    Parameters
    ----------
    work_root : str
        The path to the directory in which the chain writes files during execution.
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    job_names : list of str
        List of names of jobs to execute on every timeslice.
    force : bool
        If True, it will force the execution of jobs regardless of their completion status.
    resume : bool
        If True, it will resume the last unfinished job.

    Notes
    -----
    - The function iterates over specified intervals, calling `run_chain()` for each.
    - It manages restart settings and logging for each subchain.
    """
    # run restarts
    for startdate_sim in tools.iter_hours(cfg.startdate, cfg.enddate,
                                          cfg.restart_step_hours):
        enddate_sim = startdate_sim + timedelta(hours=cfg.restart_step_hours)

        if enddate_sim > cfg.enddate:
            continue

        # Set restart variable (only takes effect for ICON)
        if startdate_sim == cfg.startdate:
            setattr(cfg, "lrestart", '.FALSE.')
        else:
            setattr(cfg, "lrestart", '.TRUE.')

        print("Starting run with startdate {}".format(startdate_sim))

        run_chain(work_root=work_root,
                  cfg=cfg,
                  startdate_sim=startdate_sim,
                  enddate_sim=enddate_sim,
                  job_names=job_names,
                  force=force,
                  resume=resume)


def restart_runs_spinup(work_root, cfg, job_names, force, resume):
    """Start subchains in specified intervals and manage restarts with spin-up.

    This function slices the total runtime of the processing chain according to the
    `cfg.restart_step_hours` configuration. It calls `run_chain()` for each specified
    interval, managing restarts with spin-up.

    Parameters
    ----------
    work_root : str
        The path to the directory in which the chain writes files during execution.
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    job_names : list of str
        List of names of jobs to execute on every timeslice.
    force : bool
        If True, it will force the execution of jobs regardless of their completion status.
    resume : bool
        If True, it will resume the last unfinished job.

    Notes
    -----
    - The function iterates over specified intervals, calling `run_chain()` for each.
    - It manages restart settings and logging for each subchain, including spin-up.
    """
    for startdate_sim in tools.iter_hours(cfg.startdate, cfg.enddate,
                                          cfg.restart_step_hours):
        if startdate_sim == cfg.startdate:
            setattr(cfg, "first_one", True)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.FALSE.')
            run_time = cfg.restart_step_hours
            startdate_sim_spinup = startdate_sim
        elif startdate_sim == cfg.startdate + timedelta(
                hours=cfg.restart_step_hours):
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", True)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = cfg.restart_step_hours + cfg.spinup
            startdate_sim_spinup = startdate_sim - timedelta(hours=cfg.spinup)
        else:
            setattr(cfg, "first_one", False)
            setattr(cfg, "second_one", False)
            setattr(cfg, "lrestart", '.TRUE.')
            run_time = cfg.restart_step_hours + cfg.spinup
            startdate_sim_spinup = startdate_sim - timedelta(hours=cfg.spinup)

        # If current enddate is later than global enddate, skip
        enddate_sim = startdate_sim + timedelta(hours=cfg.restart_step_hours)
        if enddate_sim > cfg.enddate:
            continue

        print(f'Runtime of sub-simulation: {run_time} h')

        run_chain(work_root=work_root,
                  cfg=cfg,
                  startdate_sim=startdate_sim_spinup,
                  enddate_sim=enddate_sim,
                  job_names=job_names,
                  force=force,
                  resume=resume)


if __name__ == '__main__':
    """Main script for running a processing chain.

    This script handles the execution of a processing chain for one or more specified cases. It loads model configurations, prepares the environment, and starts the chain based on the provided settings.

    Parameters
    ----------
    None (Command-line arguments are parsed internally)

    Notes
    -----
    - This script uses command-line arguments to specify cases and job lists.
    - It loads model configurations, converts paths to absolute, sets restart settings, and starts the chain.
    - Depending on the model's features, it may run with or without restarts or utilize spin-up restarts.
    """
    args = parse_arguments()

    for casename in args.casenames:
        # Load configs
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
            args.job_list = cfg.workflow['jobs']

        print(f"Starting chain for case {casename} and workflow {cfg.workflow_name}")

        # Check for restart compatibility and spinup
        if 'restart' in cfg.workflow['features']:
            if hasattr(cfg, 'spinup'):
                print("Using spin-up restarts.")
                restart_runs_spinup(work_root=cfg.work_root,
                                    cfg=cfg,
                                    job_names=args.job_list,
                                    force=args.force,
                                    resume=args.resume)
            else:
                print("Using built-in model restarts.")
                restart_runs(work_root=cfg.work_root,
                             cfg=cfg,
                             job_names=args.job_list,
                             force=args.force,
                             resume=args.resume)
        else:
            print("No restarts are used.")
            run_chain(work_root=cfg.work_root,
                      cfg=cfg,
                      startdate_sim=cfg.startdate,
                      enddate_sim=cfg.enddate,
                      job_names=args.job_list,
                      force=args.force,
                      resume=args.resume)

    print('>>> Finished the processing chain successfully <<<')

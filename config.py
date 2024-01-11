import subprocess
import os
import yaml

from jobs import tools
from pathlib import Path
from datetime import datetime


class Config():

    # Requested slurm info keys and corresponding printed width
    info_requests = {
        'JobName': 10,
        'JobID': 8,
        'Partition': 9,
        'NNodes': 6,
        'State': 14,
        'Start': 13,
        'End': 13,
        'Elapsed': 9
    }

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

        self.chain_src_dir = Path.cwd()
        self.case_path = self.chain_src_dir / 'cases' / casename
        self.work_root = self.chain_src_dir / 'work'

        # User-defined attributes from config file
        self.load_config_file(casename)

        # Set case root
        self.case_root = self.work_root / self.casename

        # Set workflow and async attributes and initiate job ids dict
        self.set_workflow()

        # Specific settings based on the node type ('gpu' or 'mc')
        self.set_node_info()

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
        cfg_file = Path('cases', casename, 'config.yaml').resolve()

        if not cfg_file.is_file():
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
            with cfg_file.open('r') as yaml_file:
                cfg_data = yaml.load(yaml_file, Loader=yaml.FullLoader)
        except FileNotFoundError:
            raise FileNotFoundError(
                f"No file 'config.yaml' in {cfg_file.parent}")

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
        elif (p := Path.home() / '.acct').exists():
            # Use account specified in ~/.acct file
            with p.open('r') as file:
                self.compute_account = file.read().rstrip()
        else:
            # Use standard account
            self.compute_account = os.popen("id -gn").read().splitlines()[0]

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
            if self.workflow_name.startswith('icon'):
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

    def set_workflow(self):
        """set workflow and async attr, initiate job ids dict"""
        # If a workflow name is specified, load from workflows.yaml
        if isinstance(self.workflow, str):
            with open('workflows.yaml') as file:
                workflows = yaml.safe_load(file)
            self.workflow = workflows[self.workflow_name]
        # Otherwise, use custom workflow from config.yaml directly
        elif isinstance(self.workflow, dict):
            self.workflow_name = self.casename
        else:
            raise InvalidWorkflowType(
                "Invalid workflow type. Must be either a string or a dictionary."
            )

        self.is_async = 'dependencies' in self.workflow

        # Initiate empty job ids dictionnary so that it can be filled in later
        self.job_ids = {'current': {}, 'previous': {}}

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
        elif (p := Path.home() / '.forward').exists():
            with p.open('r') as file:
                self.user_mail = file.read().rstrip()
        else:
            self.user_mail = None

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
                    item_type = "Path" if type(
                        item).__name__ == "PosixPath" else type(item).__name__
                    print(f"  - {item:<{max_col_width-4}} {item_type}")
            elif isinstance(value, dict):
                # If the value is a dictionary, format it as before
                print(f"{key:<{max_col_width}} dict")
                for sub_key, sub_value in value.items():
                    sub_value_type = "Path" if type(
                        sub_value).__name__ == "PosixPath" else type(
                            sub_value).__name__
                    print(
                        f"  - {sub_key:<{max_col_width-4}} {sub_value_type:<4} {sub_value}"
                    )
            else:
                # Standard output
                key_type = type(key).__name__
                print(f"{key:<{max_col_width}} {key_type:<4} {value}")

    def convert_paths_to_absolute(self, dct=None):
        """Convert relative file paths to absolute paths in the configuration.

        Recursively convert all strings starting with './' in the instance
        attributes to absolute paths.
        """
        if dct is None:
            self.convert_paths_to_absolute(dct=vars(self))
        else:
            for k, v in dct.items():
                if isinstance(v, dict):
                    self.convert_paths_to_absolute(dct=v)
                elif isinstance(v, str) and v.startswith('./'):
                    dct[k] = Path(v).absolute()

    def create_vars_from_dicts(self, dct=None, key=None):
        """Create instance attributes from dictionary entries in the configuration.

        This method recursively iterates through the instance's attribute dictionary
        and checks for dictionary values. For each dictionary encountered, it creates
        new instance attributes by concatenating the original attribute name and the
        dictionary key, and assigns the corresponding values.
        """
        if dct is None:
            self.create_vars_from_dicts(dct=vars(self).copy())
        else:
            for k, v in dct.items():
                subkey = k if key is None else key + '_' + k
                if isinstance(v, dict):
                    self.create_vars_from_dicts(dct=v, key=subkey)
                else:
                    setattr(self, subkey, v)

    def log_job_status(self, job, status, launch_time, duration=None):
        """
        Log the status of a job in a chain to a file.

        Parameters:
        - job (str): The type of job, either 'chain' or a specific job name.
        - status (str): The status of the job, e.g., 'FINISH', 'ERROR', etc.
        - launch_time (datetime.datetime): The timestamp when the job was launched or finished.
        - duration (datetime.timedelta, optional): The duration of the job. Default is None.

        The function logs the job information to a file named 'chain_status.log' in the case root directory.
        If the log file doesn't exist, it creates a header with the column names.
        The logged entry includes the job type, job ID (if applicable), status, launch time,
        and optionally, the duration if provided.

        Example:
        - log_job_status('chain', 'FINISH', datetime.datetime.now(), '00:15:30')
        - log_job_status('task_1', 'ERROR', datetime.datetime(2023, 12, 20, 8, 30, 15))
        """
        log_file = self.case_root / "chain_status.log"

        # Check if the file exists, if not, create it and write header
        if not log_file.is_file():
            header = "Name            ID                    Status Time                     Duration\n"
            with open(log_file, 'w') as f:
                f.write(header)

        # Format duration and chunk_id
        if job == 'chain':
            if duration is not None:
                duration = self.format_duration(duration)
            chunk_id = self.casename
        else:
            if duration is not None:
                duration = f"{str(int(duration.total_seconds()))} s"
            chunk_id = self.chunk_id

        # Log the job information
        launch_time = launch_time.strftime("%a %b %d %Y %H:%M:%S")
        if status == 'FINISH' and duration:
            log_entry = f"{job:<15} {chunk_id:<21} {status:<6} {launch_time:<24} {duration}\n"
        else:
            log_entry = f"{job:<15} {chunk_id:<21} {status:<6} {launch_time:<24}\n"

        with open(log_file, 'a') as f:
            f.write(log_entry)

    def format_duration(self, duration):
        """
        Format a duration represented by a datetime.timedelta object into a human-readable string.

        Parameters:
        - duration (datetime.timedelta): The duration to be formatted.

        Returns:
        - str: A string representing the formatted duration in the "0d 0h 0m 0s" format.
        """
        seconds = duration.total_seconds()
        days, remainder = divmod(seconds, 86400)
        hours, remainder = divmod(remainder, 3600)
        minutes, seconds = divmod(remainder, 60)

        formatted_duration = f"{int(days)}d {int(hours)}h {int(minutes)}m {int(seconds)}s"
        return formatted_duration

    def init_time_logging(self, job):
        launch_time = datetime.now()
        self.log_job_status(job, 'START', launch_time)

        return launch_time

    def finish_time_logging(self, job, launch_time):
        end_time = datetime.now()
        duration = end_time - launch_time
        self.log_job_status(job, 'FINISH', end_time, duration)

    def get_dep_ids(self, job_name, add_dep=None):
        """Get dependency job ids for `job_name`"""
        # Initial list of dependencies
        if add_dep is not None:
            if isinstance(add_dep, int):
                dep_id_list = [add_dep]
            else:
                try:
                    dep_id_list = list(add_dep)
                except TypeError:
                    print("add_dep must be an iterable")
        else:
            dep_id_list = []

        # Add job dependencies
        if self.is_async:
            # Could be that job has no dependency, even in an async config,
            # e.g., prepare_data
            if deps := self.workflow['dependencies'].get(job_name):
                for stage in 'previous', 'current':
                    if dep_stage := deps.get(stage):
                        for job in dep_stage:
                            # Could be that dep job id does not exist, e.g.,
                            # if dep job is deactivated or it's the first chunk
                            if dep_id := self.job_ids[stage].get(job):
                                dep_id_list.extend(dep_id)
        return dep_id_list

    def get_dep_cmd(self, job_name, add_dep=None):
        """Generate the part of the sbatch command that sepcifies dependencies for job_name."""
        if self.is_async:
            # async case
            if dep_ids := self.get_dep_ids(job_name, add_dep=add_dep):
                dep_str = ':'.join(map(str, dep_ids))
                return f'--dependency=afterok:{dep_str}'
            else:
                # job_name has no dependencies but still belongs to an async workflow
                # so don't use --wait
                return None
        else:
            # sequential case
            return '--wait'

    def submit(self, job_name, script, add_dep=None, logfile=None):
        """Submit job with dependencies"""
        script_path = Path(script)
        sbatch_cmd = ['sbatch', '--parsable']
        if dep_cmd := self.get_dep_cmd(job_name, add_dep=add_dep):
            sbatch_cmd.append(dep_cmd)
        sbatch_cmd.append(script_path.name)

        result = subprocess.run(sbatch_cmd,
                                cwd=script_path.parent,
                                capture_output=True)
        job_id = int(result.stdout)
        print(f'        └── Submitted batch job {job_id}')

        if job_name not in self.job_ids['current']:
            self.job_ids['current'][job_name] = [job_id]
        else:
            self.job_ids['current'][job_name].append(job_id)

        exitcode = result.returncode
        self.check_job(exitcode, logfile=logfile)

        return job_id

    def check_job(self, exitcode, logfile=None):
        """Check the exitcode returned by a job. 
        In case of ICON-ART, ignore the "invalid pointer" error on a successful run.
        """
        if logfile and tools.grep("ART: ", logfile)['success'] and \
            tools.grep("free(): invalid pointer", logfile)['success'] and \
            tools.grep("clean-up finished", logfile)['success']:
            exitcode = 0

        if exitcode != 0:
            raise RuntimeError(f"sbatch returned exitcode {exitcode}")

    def create_sbatch_script(self, job_name):
        """Create an sbatch script to launch jobs individually.
        Use run_chain.py arguments to submit those jobs.
        """
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="{job_name}_{self.chunk_id}"',
            '#SBATCH --nodes=1',
            f'#SBATCH --output={self.logfile}',
            '#SBATCH --open-mode=append',
            f'#SBATCH --account={self.compute_account}',
            f'#SBATCH --partition={self.compute_queue}',
            f'#SBATCH --constraint={self.constraint}',
            '',
            f'cd {self.chain_src_dir}',
            'eval "$(conda shell.bash hook)"',
            'conda activate proc-chain',
            f'./run_chain.py {self.casename} -j {job_name} -c {self.chunk_id} -f -s --no-logging',
            '',
        ]

        job_file = self.chain_root / f'{job_name}.sh'
        with open(job_file, mode='w') as job_script:
            job_script.write('\n'.join(script_lines))

        return job_file

    def wait_for_previous(self):
        """Wait for all jobs of the previous stage to be finished.

        Do this by submitting a fake job depending on all jobs from the
        'previous' stage.
        """
        dep_ids = []
        for ids in self.job_ids['previous'].values():
            dep_ids.extend(ids)
        if dep_ids:
            job_file = self.case_root / 'submit.wait.slurm'
            log_file = self.case_root / 'wait.log'
            dep_str = ':'.join(map(str, dep_ids))
            script_lines = [
                '#!/usr/bin/env bash', '#SBATCH --job-name="wait"',
                '#SBATCH --nodes=1', f'#SBATCH --output={log_file}',
                f'#SBATCH --account={self.compute_account}',
                f'#SBATCH --partition={self.compute_queue}',
                f'#SBATCH --constraint={self.constraint}',
                f'#SBATCH --dependency=afterany:{dep_str}', '', '# Do nothing',
                'exit 0'
            ]
            with open(job_file, mode='w') as wait_job:
                wait_job.write('\n'.join(script_lines))

            subprocess.run(['sbatch', '--wait', job_file], check=True)

    @staticmethod
    def get_job_info(job_id,
                     slurm_keys=['JobName', 'Elapsed', 'ExitCode'],
                     parse=True):
        """Retrieve slurm job info as given by sacct

        if parse is True, return the raw string from sacct else parse info into a dict.
        All possible keys are given by `sacct --helpformat`"""

        # Get info from sacct
        cmd = ["sacct", f"--format={', '.join(slurm_keys)}", "-j", str(job_id)]

        if parse:
            cmd.append("--parsable")

        info_str = subprocess.run(cmd, capture_output=True, check=True).stdout

        if parse:
            # Parse in a dictionnary before returning
            # The inner most process should be the relevant one, hence the 1 index
            slurm_info = info_str.split(b'\n')[1].split(b'|')
            return {k: v.decode() for k, v in zip(slurm_keys, slurm_info)}
        else:
            return info_str.decode()

    def get_slurm_summary(self):
        """get slurm info summary or all jobs of current chunk"""

        # Get job info for all jobs
        self.slurm_info = {}
        for job_name in self.jobs:
            for job_id in self.job_ids['current'][job_name]:
                self.slurm_info[job_name] = []
                self.slurm_info[job_name].append(
                    self.get_job_info(job_id,
                                      slurm_keys=self.info_requests.keys(),
                                      parse=True))

    def print_slurm_summary(self):
        # Build table header and line format
        # (could be done class-wide, not for each call)
        headers = []
        hlines = []
        formats = []
        for k, l in self.info_requests.items():
            formats.append(f"{{{k}:>{l}.{l}}}")
            headers.append(f"{k:>{l}.{l}}")
            hlines.append("-" * l)

        table_header = '\n'.join((' '.join(headers), ' '.join(hlines)))
        line_format = " ".join(formats)

        print("    └── Slurm info of submitted jobs")

        for job_name in self.jobs:
            print(f"        └── {job_name}")
            print(table_header)
            for info in self.slurm_info[job_name]:
                print(line_format.format(**info))  # KeyError: 'JobID'

    def check_chunk_success(self):
        status = 0
        for job_name, info_list in self.slurm_info.items():
            for info in info_list:
                if info['State'] != 'COMPLETED':
                    status += 1

        if status > 0:
            raise RuntimeError("One or more job failed")


class InvalidWorkflowType(Exception):
    pass

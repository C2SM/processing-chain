import subprocess
import os
import yaml
from jobs import tools
from pathlib import Path


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

        self.chain_src_dir = Path.cwd()
        self.case_path = self.chain_src_dir / 'cases' / casename
        self.work_root = self.chain_src_dir / 'work'

        # User-defined attributes from config file
        self.load_config_file(casename)

        # Specific settings based on the node type ('gpu' or 'mc')
        self.set_node_info()

        # Set workflow and async attributes and initiate job ids dict
        self.set_workflow()

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

        with open('workflows.yaml') as file:
            workflows = yaml.safe_load(file)
        self.workflow = workflows[self.workflow_name]
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

    def convert_paths_to_absolute(self, dct=None):
        """Convert relative file paths to absolute paths in the configuration.

        Recursively convert all strings starting with './' in the instance
        attributes to absolute paths.

        Returns
        -------
        Config
            The same `Config` instance with relative file paths converted to absolute paths.
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

        Returns
        -------
        Config
            The same `Config` instance with new attributes created from dictionary entries.
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

    def get_dep_ids(self, job_name, add_dep=None):
        """Get dependency job ids for `job_name`"""

        # Initial list of dependencies
        if add_dep is not None:
            if type(add_dep) is int:
                dep_id_list = [add_dep]
            else:
                try:
                    dep_id_list = list(add_dep)
                except TypeError:
                    print(f'add_dep must be an iterable')
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

    def submit(self, job_name, script, add_dep=None):
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
        if not job_name in self.job_ids['current']:
            self.job_ids['current'][job_name] = [job_id]
        else:
            self.job_ids['current'][job_name].append(job_id)

        # If needed internaly in a multi-job task like prepare_data
        # Can then be passed as add_dep keyword
        return job_id

    def wait_for_previous(self):
        """wait for all jobs of the previous stage to be finished

        Do this by submitting a fake job depending on all jobs from the 'previous' stage.
        """

        dep_ids = []
        for ids in self.job_ids['previous'].values():
            dep_ids.extend(ids)
        if dep_ids:
            job_file = 'submit.wait.slurm'
            with open(job_file, mode='w') as wait_job:
                wait_job.write("""#!/bin/bash\n#Do nothing\nexit 0""")

            subprocess.run([
                'sbatch', '-W', '--nodes=1', '--job-name=wait',
                f'--account={self.compute_account}', job_file
            ],
                           check=True)

# <img src="https://polybox.ethz.ch/index.php/s/yc3zMmoXKyI2rJm/download" width="64" valign="middle" alt="Spack"/> Processing Chain

The Processing Chain is a python script that prepares necessary input
data, submits compute-jobs to the queue on Piz Daint and does
post-processing steps. It supports different types of models and simulation types,
including **COSMO**, **COSMO-GHG**, **COSMO-ART**, **ICON** and
**ICON-ART**. The chain can flexibly be adapted according to your needs,
e.g., by creating your own case or adding new jobs.

## Environment Setup

The following steps allow you to create and use your own virtual
environment to run the Processing Chain. We recommend to use a conda
environment for the usage of the provided scripts. Please follow the
instruction for the installation. The following steps only need to be 
performed once.

### 1\. Install Miniconda

Install as user specific Miniconda, e.g. on your `$HOME` directory,
which is the default location.

> **Note**: Only conda itself should be installed in your `$HOME`.
> All environments should be stored in your `$PROJECT` directory,
> otherwise you risk filling up your `$HOME` directory. See below for instructions.

To install the latest Miniconda, type:

    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh

Further deails on Miniconda can be found on the [Miniconda documentation page](https://docs.conda.io/en/latest/miniconda.html).

### 2\. Create the Conda Environment

Create a conda environment `proc-chain` with and install requirements:

    conda env create --prefix $PROJECT/envs/proc-chain -f env/environment.yml

To be able to activate your conda environment by simply using `conda activate proc-chain` instead of the full path, add the following to your `.bashrc`:

    export CONDA_ENVS_PATH=$PROJECT/envs

Activate the environment (use "source activate" in case "conda activate"
does not work):

    conda activate proc-chain

If you already have the environment but want to update it:

    conda env update --file env/environment.yml --prune

### 3\. Store user-specific data

To register your email address and standard project account, store them into
these files within your home directory:

    echo <your_account_id> > ~/.acct
    echo <your_email_address> > ~/.forward

These settings are optional. The Processing Chain will first check the content
of those files. If desired, the corresponding variables can be overridden by setting
the `compute_account` and `user_mail` variables in the `config.yaml` file.

## Run the Chain

Once everything has been set up correctly according to the above steps,
you just need to execute the following command to activate your
environment (if not done already):

    conda activate proc-chain

To test if your environment has been successfully set, use the command
line help to display the available arguments for the main script:

    python run_chain.py -h

To run the test cases with their standard jobs, please ensure
that you clone the Processing Chain to `$SCRATCH`, as input and
output data are stored in subdirectories of the Processing Chain repository
itself.

> **Note**: For your own setups, you can use the Processing Chain
> on a backed-up file system like `/project` or `/store`. In that case,
> adapt the configuration file `config.yaml` in your case folder so that
> the output files are written to a specified folder on `$SCRATCH`.

For these pre-defined test cases, you can use the Jenkins script

    ./jenkins/scripts/jenkins.sh

This script calls other scripts that are located in `jenkins/scripts/`. 
They will
- activate the conda environment (if not done already)
- setup spack-c2sm
- download input data to `input/`
- build `int2lm`, `cosmo-ghg`, `icon` and `icon-art`
- test the following cases:
    - `cosmo-ghg-spinup-test`
    - `cosmo-ghg-test`
    - `icon-test`
    - `icon-art-oem-test`
    - `icon-art-global-test`

To run the test cases manually, type:

```bash
    # replace <casename> with one of the above tests
    python run_chain.py <casename>
```

## Documentation

For more information about the file structure, configuration options,
namelist templates etc., please read the official
[documentation](https://c2sm.github.io/processing-chain/).

## Contributing

If you think your (well-documented) developments might also be useful to
others, we encourage you to create a pull request for this repository.

## Credits

The Processing Chain was originally developed in 2018 at
[Empa](https://www.empa.ch) by the [Atmospheric Modeling and Remote
Sensing](https://www.empa.ch/web/s503/modelling-remote-sensing) group.
The following persons contributed significantly to the initial
development (in alphabetic order):

  - Pavle Arsenovic
  - Dominik Brunner
  - Jean-Matthieu Haussaire
  - Gerrit Kuhlmann
  - Qing Mu
  - David Ochsner
  - Michael Steiner

Since 2021, the code is public and hosted by C2SM. More information can
be found at the
[C2SM User Landing Page](https://c2sm.github.io/tools/processing_chain.html).
The current code owner is Michael Jähn (<michael.jaehn@c2sm.ethz.ch>).

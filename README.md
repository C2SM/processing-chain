# Processing Chain for COSMO and ICON Simulations

The Processing Chain is a python script that prepares necessary input
data, submits compute-jobs to the queue on Piz Daint and does
post-processing steps. It supports different types of simulations,
including **COSMO**, **COSMO-GHG**, **COSMO-ART**, **ICON** and
**ICON-ART**. The chain can flexibly be adapted according to your needs,
e.g., by creating your own case or adding new jobs.

## Environment Setup

The following steps allow you to create and use your own virtual
environment to run the Processing Chain. We recommend to use a conda
environment for the usage of the provided scripts. Please follow the
instruction for the installation.

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

## Run the Chain

Once everything has been set up correctly according to the above steps,
you just need to execute the following command to activate your
environment (if not done already):

    conda activate proc-chain

To test if your environment has been successfully set, use the command
line help to see the available arguments for the main script:

    python run_chain.py -h

To run the test cases with their standard jobs, please ensure
that you clone the Processing Chain to `$SCRATCH`, as input and
output data are stored in subdirectories of the chain itself.

> **Note**: For your own setups, you can use the Processing Chain
> on a backed-up file system like `/project` or `/store`. In that case,
> adapt the configuration file `config.py` in your case folder in
> a way that output files are written to a specified folder on `$SCRATCH`.

For these test cases, you can use the Jenkins script

    ./jenkins/scripts/jenkins.sh

This script calls other scripts that are located in `jenkins/scripts/`. 
They will
- activate the conda environment (if not done already)
- setup spack-c2sm
- download input data to `input/`
- build `int2lm`, `cosmo-ghg`, `icon` and `icon-art`
- test `cosmo-ghg`, `icon`, `icon-art-oem`, `icon-art-global`

To run the test cases manually, type:

```bash
    # COSMO-GHG
    python run_chain.py cosmo-ghg-11km-test 2015-01-01 0 24
    # ICON
    python run_chain.py icon-test 2018-01-01 0 24 -j prepare_data icon
    # ICON-ART (OEM)
    python run_chain.py icon-art-oem-test 2018-01-01 0 24 -j prepare_data oae icon
    # ICON-ART (global)
    python run_chain.py icon-art-global-test 2018-01-01 0 24 -j prepare_data oae icon
```

## Documentation

To set up your own simulation case or to check all available
configuration options, please read the official
[documentation](https://processing-chain.readthedocs.io).

## Contributing

If you think your (well-documented) developments might also be useful to
others, we encourage you to create a pull request for this repository.

## Credits

The Processing Chain was originally developed at
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

The current code owner is Michael Jähn (<michael.jaehn@c2sm.ethz.ch>).

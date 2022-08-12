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

Install as user specific miniconda, e.g. on `/scratch` (enter `cd
$SCRATCH` and `pwd` at the command line to get to your personal scratch
directory on Daint). When the command prompt asks for installation
location, provide the path to your scratch and append `/miniconda3`.

> **Note**: The default location would be on your `$HOME` directory, which
> may lead to memory issues.

To install the latest miniconda, type:

    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh

Then, export `$PATH` to your conda installation:

    export PATH="$SCRATCH/miniconda3/bin:$PATH"

### 2\. Create the Conda Environment

Create a conda environment `proc-chain` with and install requirements:

    conda env create -f env/environment.yml

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
environment:

    conda activate proc-chain

To test if your environment has been successfully set, use the command
line help to see the available arguments for the main script:

    python run_chain.py -h

To run the example cases with their standard jobs, please ensure
that you clone the Processing Chain to `$SCRATCH`, as input and
output data are stored in subdirectories of the chain itself.

> **Note**: For your own setups, you can use the Processing Chain
> on a backed-up file system like `/project` or `/store`. In that case,
> adapt the configuration file `config.py` in your case folder in
> a way that output files are written to a specified folder on `$SCRATCH`.

For these test cases, the necessary input data can be obtained via
the following script (this may take some time):

    ./get_data.sh

Furthermore, executables for COSMO-GHG and ICON are needed. For COSMO-GHG,
a spack instance needs to be initialized first. Then, COSMO-GHG can be 
installed:

    source /project/g110/spack/user/daint/spack/share/spack/setup-env.sh
    spack installcosmo cosmo@empa-ghg%nvhpc cosmo_target=gpu +cppdycore

For ICON, use the following script:

    ./jenkins/scripts/create-icon-binary.sh

Finally, to run the COSMO-GHG test case, type:

    python run_chain.py cosmo-ghg-11km-test 2015-01-01 0 24

> **Note**: Be sure to have spack initialized via 
> `source /project/g110/spack/user/daint/spack/share/spack/setup-env.sh`
> before you run COSMO jobs.

For ICON, type:

    python run_chain.py icon-test 2018-01-01 0 24 -j prepare_data icon

Empa users can perform additional tests:

    python run_chain.py cosmo-art-mother-test cosmo-art-nested-test 2015-06-26 0 24

or:

    python run_chain.py icon-art-test 2018-01-01 0 24 -j prepare_data icon

or:

    python run_chain.py icon-art-oem-test 2018-01-01 0 24 -j prepare_data oae icon

or:

    python run_chain.py icon-art-oem-ensembles-test 2018-01-01 0 24 -j prepare_data oae icon

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

# <img src="https://polybox.ethz.ch/index.php/s/yc3zMmoXKyI2rJm/download" width="64" valign="middle" alt="Processing Chain"/> Processing Chain

The Processing Chain is a python script that prepares necessary input
data, submits compute-jobs to the Slurm queue of the supported HPC systems
and does post-processing steps. It supports different types of models and
simulation types, including **COSMO**, **COSMO-GHG**, **COSMO-ART**,
**ICON** and **ICON-ART**. The chain can flexibly be adapted according to
your needs, e.g., by creating your own case or adding new jobs.

## Environment Setup

Two installation paths are available. See the
[full documentation](https://c2sm.github.io/processing-chain/latest/environment.html)
for step-by-step instructions.

**Option A — Conda (Miniforge, recommended):**

```bash
# 1. Install Miniforge (once)
wget https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
bash Miniforge3-Linux-x86_64.sh

# 2. Create the environment
conda env create --prefix $PROJECT/envs/proc-chain -f environment.yml

# 3. Activate
conda activate proc-chain
```

**Option B — pip (virtual environment):**

> **Note**: `cdo` and `nco` are not available via pip. On HPC systems, load
> them via the module system first (see machine-specific setup below).

```bash
python3 -m venv $PROJECT/envs/proc-chain
source $PROJECT/envs/proc-chain/bin/activate
pip install -r requirements.txt
```

### Machine-specific setup

Ready-made scripts under `machines/` load system software and activate the
environment in one step.

**Euler (ETH Zürich)**

```bash
# Load system modules only (e.g. in job scripts):
source machines/euler/modules.sh

# Modules + venv activation (interactive sessions):
source machines/euler/setup_env.sh
```

**Santis (CSCS)**

On Santis, `uenv start` spawns a new shell and cannot be sourced; use the
wrapper instead:

```bash
# One-stop interactive setup (uenv + venv):
bash machines/santis/setup_env.sh

# Or start the uenv manually:
uenv start climtools/25.2:v1 --view=climtools

# Run a single command without an interactive shell:
uenv run climtools/25.2:v1 --view=climtools -- ./run_chain.py <casename>
```

See `machines/` for the structure to follow when adding other machines.

## Run the Chain

To activate your environment, type:

**Conda:**

    conda activate proc-chain

**pip:**

    source $PROJECT/envs/proc-chain/bin/activate

To test if your environment has been successfully set, use the command
line help to display the available arguments for the main script:

    ./run_chain.py -h

To run the test cases with their standard jobs, please ensure
that you clone the Processing Chain to `$SCRATCH`, as input and
output data are stored in subdirectories of the Processing Chain repository
itself.

> **Note**: For your own setups, you can use the Processing Chain
> on a backed-up file system like `/project` or `/store`. In that case,
> adapt the configuration file `config.yaml` in your case folder so that
> the output files are written to a specified folder on `$SCRATCH`.

For the pre-defined test cases, you can use the driver script

    ./testing/run_tests.sh --pip

This script calls the other scripts in `testing/scripts/` and will:
- create the Python environment (`--pip` for a venv, otherwise conda)
- download input data to `input/`
- build `icon`
- test the `icon-test-euler` case

To run the test cases manually, type:

    # replace <casename> with one of the above tests
    ./run_chain.py <casename>

## Documentation

For more information about the file structure, configuration options,
namelist templates etc., please read the [official
documentation](https://c2sm.github.io/processing-chain/latest/).

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

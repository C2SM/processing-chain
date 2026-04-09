.. _environment-section:

Environment Setup
=================

The following steps allow you to create and use your own virtual environment to run the Processing Chain. You can use either **conda** (recommended) or **pip** with a standard Python virtual environment. Please follow the instructions for your preferred method. The following steps only need to be performed once.

Option A: Conda
---------------

1. Install Miniconda
~~~~~~~~~~~~~~~~~~~~

Install Miniconda as user-specific Miniconda, e.g., in your ``$HOME`` directory, which is the default location.

.. note::
   Only conda itself should be installed in your ``$HOME``. All environments should be stored in your ``$PROJECT`` directory; otherwise, you risk filling up your ``$HOME`` directory. See below for instructions.

To install the latest Miniconda, type:

.. code-block:: bash

    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh

Further details on Miniconda can be found on the `Miniconda documentation page <https://docs.conda.io/en/latest/miniconda.html>`_.

2. Create the Conda Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a conda environment ``proc-chain`` and install the requirements:

.. code-block:: bash

    conda env create --prefix $PROJECT/envs/proc-chain -f env/environment.yml

To be able to activate your conda environment by simply using ``conda activate proc-chain`` instead of the full path, add the following to your ``.bashrc``:

.. code-block:: bash

    export CONDA_ENVS_PATH=$PROJECT/envs

Activate the environment (use "source activate" in case "conda activate" does not work):

.. code-block:: bash

    conda activate proc-chain

If you already have the environment but want to update it:

.. code-block:: bash

    conda env update --file environment.yml --prune

3. Store user-specific data
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To register your email address and standard project account, store them in these files within your home directory:

.. code-block:: bash

    echo <your_account_id> > ~/.acct
    echo <your_email_address> > ~/.forward

These settings are optional. The Processing Chain will first check the content of those files. If desired, the corresponding variables can be overridden by setting the ``compute_account`` and ``user_mail`` variables in the ``config.yaml`` file.

Option B: pip (virtual environment)
-------------------------------------

If you prefer not to use conda, you can set up a standard Python virtual environment with pip instead.

.. note::
   Python 3.11 or later is required. Note that ``cdo`` and ``nco`` are not available via pip and must be installed separately (e.g., via your system package manager or a module system).

1. Create and activate the virtual environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a virtual environment, for example in your ``$PROJECT`` directory:

.. code-block:: bash

    python3 -m venv $PROJECT/envs/proc-chain
    source $PROJECT/envs/proc-chain/bin/activate

2. Install the requirements
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Install all Python dependencies from ``requirements.txt``:

.. code-block:: bash

    pip install -r requirements.txt

To update an existing virtual environment:

.. code-block:: bash

    pip install --upgrade -r requirements.txt

3. Activate the environment in future sessions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each time you start a new session, activate the environment with:

.. code-block:: bash

    source $PROJECT/envs/proc-chain/bin/activate

You can add this line to your ``.bashrc`` to activate it automatically.

4. Store user-specific data
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Follow the same steps as described in the conda section above (store
``~/.acct`` and ``~/.forward``) if needed.

Machine-specific Setup
----------------------

The ``machines/`` directory contains ready-made scripts for loading the
correct system modules and activating the Python environment on supported
HPC systems. Each machine has its own sub-directory:

.. code-block:: text

    machines/
    ├── euler/
    │   ├── modules.sh    # module load commands only
    │   └── setup_env.sh  # modules + venv activation (one-stop setup)
    └── santis/
        ├── modules.sh    # uenv image/view reference
        └── setup_env.sh  # uenv start + venv activation

Euler (ETH Zürich)
~~~~~~~~~~~~~~~~~~

**Load modules only** (useful inside Slurm job scripts):

.. code-block:: bash

    source machines/euler/modules.sh

This loads:

.. code-block:: bash

    module load stack/2024-06 gcc/12.2.0 openmpi/4.1.6 python/3.12.8
    module load cdo/2.2.2 nco/5.1.6 netcdf-c/4.9.2

**Full interactive-session setup** (modules + venv activation):

.. code-block:: bash

    source machines/euler/setup_env.sh

By default this activates the virtual environment at ``<repo>/venv``.
To use a different location, set ``PROC_CHAIN_VENV`` before sourcing:

.. code-block:: bash

    export PROC_CHAIN_VENV=$PROJECT/envs/proc-chain
    source machines/euler/setup_env.sh

If the virtual environment does not exist yet, create it first (see
:ref:`Option B <environment-section>` above):

.. code-block:: bash

    python3 -m venv $PROJECT/envs/proc-chain
    pip install -r requirements.txt

Santis (CSCS)
~~~~~~~~~~~~~

On Santis, software is provided through **uenv** (user environments) instead
of the traditional module system.  Because ``uenv start`` spawns a new shell,
it cannot be sourced inside an existing session.

**Start an interactive session** with the required environment:

.. code-block:: bash

    uenv start climtools/25.2:v1 --view=climtools

**Or use the provided wrapper** (starts the uenv and activates the venv):

.. code-block:: bash

    bash machines/santis/setup_env.sh

If the virtual environment already exists and you are **already inside the
uenv**, you can activate it directly:

.. code-block:: bash

    source machines/santis/setup_env.sh --no-uenv

To use a custom venv location, set ``PROC_CHAIN_VENV`` first:

.. code-block:: bash

    export PROC_CHAIN_VENV=$SCRATCH/envs/proc-chain
    bash machines/santis/setup_env.sh

**Run a single command** inside the uenv without entering an interactive shell:

.. code-block:: bash

    uenv run climtools/25.2:v1 --view=climtools -- ./run_chain.py <casename>

If the virtual environment does not exist yet, enter the uenv first and
create it:

.. code-block:: bash

    uenv start climtools/25.2:v1 --view=climtools
    python3 -m venv $SCRATCH/envs/proc-chain
    pip install -r requirements.txt

Adding support for a new machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a new sub-directory under ``machines/`` following the same
pattern:

.. code-block:: text

    machines/
    └── <machine-name>/
        ├── modules.sh
        └── setup_env.sh

Use ``machines/euler/`` as a template and adapt the ``module load``
commands for the target system.

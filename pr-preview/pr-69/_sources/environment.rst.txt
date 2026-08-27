.. _environment-section:

Environment Setup
=================

The following steps guide you through setting up a Python environment for the
Processing Chain. **All steps only need to be performed once.**

Two installation paths are available:

- :ref:`Option A <conda-option>` — **Conda (Miniforge)**: self-contained,
  installs its own Python and all tools including ``cdo`` and ``nco``.
  Recommended for most users.
- :ref:`Option B <pip-option>` — **pip**: uses a system-provided Python.
  Requires ``cdo`` and ``nco`` to be available separately
  (e.g. via the HPC module system — see :ref:`machine-setup`).

.. _conda-option:

Option A: Conda (Miniforge)
----------------------------

Step 1: Install Miniforge
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install Miniforge as a user-specific conda distribution, e.g., into your
``$HOME`` directory (the default location):

.. code-block:: bash

    wget https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
    bash Miniforge3-Linux-x86_64.sh

.. note::
   Only Miniforge itself should be installed in ``$HOME``. Store all
   environments in ``$PROJECT`` (or ``$SCRATCH``) to avoid filling up your
   home directory.

Further details can be found on the
`Miniforge documentation page <https://github.com/conda-forge/miniforge>`_.

Step 2: Create the Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a conda environment named ``proc-chain`` with all dependencies:

.. code-block:: bash

    conda env create --prefix $PROJECT/envs/proc-chain -f environment.yml

To be able to activate the environment with the short name
``proc-chain`` instead of the full path, add the following to your
``.bashrc``:

.. code-block:: bash

    export CONDA_ENVS_PATH=$PROJECT/envs

Step 3: Activate the Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    conda activate proc-chain

Use ``source activate proc-chain`` if ``conda activate`` does not work.

You are now ready to :ref:`run the chain <howtorun-section>`.

Step 4: Update the Environment (if needed)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the ``environment.yml`` has changed since you created the environment,
update it with:

.. code-block:: bash

    conda env update --prefix $PROJECT/envs/proc-chain --file environment.yml --prune

.. _pip-option:

Option B: pip (Virtual Environment)
-------------------------------------

.. note::
   Python 3.11 or later is required. ``cdo`` and ``nco`` are not available
   via pip and must be provided by the system (e.g. loaded via modules on
   HPC clusters — see :ref:`machine-setup`).

Step 1: Load System Python
~~~~~~~~~~~~~~~~~~~~~~~~~~~

On HPC systems, load the appropriate modules before continuing.
See :ref:`machine-setup` for machine-specific instructions.

On a generic Linux system with Python 3.11+ already installed, skip
directly to Step 2.

Step 2: Create the Virtual Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a virtual environment, for example in ``$PROJECT``:

.. code-block:: bash

    python3 -m venv $PROJECT/envs/proc-chain

Step 3: Activate and Install
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Activate the environment and install all Python dependencies:

.. code-block:: bash

    source $PROJECT/envs/proc-chain/bin/activate
    pip install -r requirements.txt

Step 4: Activate in Future Sessions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each time you open a new session, activate the environment with:

.. code-block:: bash

    source $PROJECT/envs/proc-chain/bin/activate

You can add this line to your ``.bashrc`` to activate it automatically, or
use the machine-specific convenience scripts described in :ref:`machine-setup`.

Step 5: Update the Environment (if needed)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    pip install --upgrade -r requirements.txt

.. _machine-setup:

Machine-specific Setup
-----------------------

The ``machines/`` directory provides ready-made scripts that load the
required system software and optionally activate the Python virtual
environment. Each supported machine has its own sub-directory:

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

Euler uses the traditional ``module`` system.

**1. Load modules** (required before using pip or running job scripts):

.. code-block:: bash

    source machines/euler/modules.sh

This executes:

.. code-block:: bash

    module load stack/2024-06 gcc/12.2.0 openmpi/4.1.6 python/3.12.8
    module load cdo/2.2.2 nco/5.1.6 netcdf-c/4.9.2

**2. One-stop interactive setup** (modules + venv activation):

.. code-block:: bash

    source machines/euler/setup_env.sh

By default, this activates the virtual environment at ``<repo>/venv``.
To use a different location, set ``PROC_CHAIN_VENV`` before sourcing:

.. code-block:: bash

    export PROC_CHAIN_VENV=$PROJECT/envs/proc-chain
    source machines/euler/setup_env.sh

If the virtual environment does not exist yet, load the modules first and
then create it (see :ref:`Option B <pip-option>` above):

.. code-block:: bash

    source machines/euler/modules.sh
    python3 -m venv $PROJECT/envs/proc-chain
    pip install -r requirements.txt

Santis (CSCS)
~~~~~~~~~~~~~

On Santis, software is provided through **uenv** (user environments).
Because ``uenv start`` spawns a new shell, it cannot be sourced inside an
existing session.

**1. Start an interactive shell** with the required environment:

.. code-block:: bash

    uenv start climtools/25.2:v1 --view=climtools

**2. One-stop interactive setup** (uenv + venv activation, recommended):

.. code-block:: bash

    bash machines/santis/setup_env.sh

If you are already inside the uenv and only need to activate the venv:

.. code-block:: bash

    source machines/santis/setup_env.sh --no-uenv

To use a custom venv location, set ``PROC_CHAIN_VENV`` first:

.. code-block:: bash

    export PROC_CHAIN_VENV=$SCRATCH/envs/proc-chain
    bash machines/santis/setup_env.sh

**Run a single command** without entering an interactive shell:

.. code-block:: bash

    uenv run climtools/25.2:v1 --view=climtools -- ./run_chain.py <casename>

If the virtual environment does not exist yet, enter the uenv first and
create it (see :ref:`Option B <pip-option>` above):

.. code-block:: bash

    uenv start climtools/25.2:v1 --view=climtools
    python3 -m venv $SCRATCH/envs/proc-chain
    pip install -r requirements.txt

Adding Support for a New Machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a new sub-directory under ``machines/`` following the same pattern:

.. code-block:: text

    machines/
    └── <machine-name>/
        ├── modules.sh    # load system software
        └── setup_env.sh  # system software + venv activation

Use ``machines/euler/`` as a template and adapt the ``module load``
commands for the target system.

Store User-specific Data (Optional)
-------------------------------------

To register your email address and standard compute account, store them in
these files in your home directory:

.. code-block:: bash

    echo <your_account_id> > ~/.acct
    echo <your_email_address> > ~/.forward

The Processing Chain reads these files automatically. They can be overridden
at any time by setting ``compute_account`` and ``user_mail`` in your case's
``config.yaml``.


.. _environment-section:

Conda Environment
=================

The following steps allow you to create and use your own virtual environment to run the Processing Chain. We recommend using a conda environment for the usage of the provided scripts. Please follow the instructions for the installation. The following steps only need to be performed once.

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

    conda env update --file env/environment.yml --prune

3. Store user-specific data
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To register your email address and standard project account, store them in these files within your home directory:

.. code-block:: bash

    echo <your_account_id> > ~/.acct
    echo <your_email_address> > ~/.forward

These settings are optional. The Processing Chain will first check the content of those files. If desired, the corresponding variables can be overridden by setting the ``compute_account`` and ``user_mail`` variables in the ``config.yaml`` file.

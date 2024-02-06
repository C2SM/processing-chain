.. _config-section:

The Processing Chain uses cases to describe a simulation. A case is a
subdirectory in ``cases/``, containing a ``config.yaml`` and several
`namelist` (e.g., ``int2lm_INPUT.cfg``) and `runscripts` (e.g.,
``icon_runjob.cfg``) :ref:`templates<namelists-section>`,
which define the simulation.

.. _config.yaml:

Configuration File
------------------

The case-dependent configuration file ``<casename>/config.yaml`` contains most
of the information that the :ref:`jobs<jobs-section>` need to prepare
and run the simulation, for example the location of the input data.
This configuration file is loaded in ``run_chain.py`` as an instance
of the ``Config()`` class in ``config.py``. 

Configuration Variables
~~~~~~~~~~~~~~~~~~~~~~~

This is a non-exhaustive list containing the most important configuration variables: 

+------------------------+-------------------------------------------------------------------------+
| Variable               | Description                                                             |
+========================+=========================================================================+
|| ``case_path``         || The path to the case directory under ``cases/`` for the specified      |
||                       || casename.                                                              |
+------------------------+-------------------------------------------------------------------------+
| ``casename``           | The name of the case. Derived from the folder name under ``case_path``. |
+------------------------+-------------------------------------------------------------------------+
|| ``chain_src_dir``     || The source directory for the processing chain, typically the current   |
||                       || working directory.                                                     |
+------------------------+-------------------------------------------------------------------------+
| ``compute_account``    | The compute account to be used based on user information.               |
+------------------------+-------------------------------------------------------------------------+
| ``constraint``         | The computational constraint (``gpu`` or ``mc``).                       |
+------------------------+-------------------------------------------------------------------------+
|| ``email``             || The user's email address, initially set to None and updated using the  |
||                       || set_email method.                                                      |
+------------------------+-------------------------------------------------------------------------+
|| ``enddate``           || The end date of the simulation in ISO 8601 format                      |
||                       || (``YYYY-MM-DDTHH:mm:ssZ``).                                            |
+------------------------+-------------------------------------------------------------------------+
| ``jobs``               | List of job-names to be executed.                                       |
+------------------------+-------------------------------------------------------------------------+
| ``log_finished_dir``   | The directory for finished log files.                                   |
+------------------------+-------------------------------------------------------------------------+
| ``log_working_dir``    | The directory for working log files.                                    |
+------------------------+-------------------------------------------------------------------------+
| ``ntasks_per_node``    | The number of tasks per node, based on the node type.                   |
+------------------------+-------------------------------------------------------------------------+
| ``restart_step``       | The restart step in ISO 8601 format.                                    |
+------------------------+-------------------------------------------------------------------------+
| ``restart_step_hours`` | The restart step in hours, derived from the ``restart_step`` attribute. |
+------------------------+-------------------------------------------------------------------------+
| ``run_on``             | The architecture the model runs on (``cpu`` or ``gpu``).                |
+------------------------+-------------------------------------------------------------------------+
| ``spinup``             | Spin-up duration in hours. Activates spinup behavior if set.            |
+------------------------+-------------------------------------------------------------------------+
|| ``startdate``         || The start date of the simulation in ISO 8601 format                    |
||                       || (``YYYY-MM-DDTHH:mm:ssZ``).                                            |
+------------------------+-------------------------------------------------------------------------+
| ``user_mail``          | The user's email address, determined based on system configuration.     |
+------------------------+-------------------------------------------------------------------------+
|| ``user_name``         || The username of the current user, obtained from the ``$USER``          |
||                       || environment variable.                                                  |
+------------------------+-------------------------------------------------------------------------+
| ``workflow``           | The name of the workflow from ``workflows.yaml`` or a self-defined one. |
+------------------------+-------------------------------------------------------------------------+
|| ``work_root``         || The root directory for processing chain execution, typically located   |
||                       || under the source directory.                                            |
+------------------------+-------------------------------------------------------------------------+


Variables to Set in ``config.yaml``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here are two examples of which general variables should be set by the user in the
case configuration file.

Header of ``config.yaml`` for the ``cosmo-ghg-spinup-test`` case
================================================================

::

    workflow: cosmo-ghg
    constraint: gpu
    ntasks_per_node: 12
    restart_step: PT6H
    spinup: 3
    startdate: 2015-01-01T00:00:00Z
    enddate: 2015-01-01T18:00:00Z

Header of ``config.yaml`` for the ``icon-art-oem-test`` case
============================================================

::

    workflow: icon-art-oem
    constraint: gpu
    run_on: cpu
    compute_queue: normal
    ntasks_per_node: 12
    restart_step: PT6H
    startdate: 2018-01-01T00:00:00Z
    enddate: 2018-01-01T12:00:00Z

    eccodes_dir: ./input/eccodes_definitions
    iconremap_bin: iconremap
    iconsub_bin: iconsub
    latbc_filename: ifs_<y><m><d><h>_lbc.nc
    inidata_prefix: ifs_init_
    inidata_nameformat: '%Y%m%d%H'
    inidata_filename_suffix: .nc
    output_filename: icon-art-oem-test
    filename_format: <output_filename>_DOM<physdom>_<ddhhmmss>
    lateral_boundary_grid_order: lateral_boundary
    art_input_folder: ./input/icon-art-oem/ART

Further variables
=================

Furthermore, there are additional variables to set that are tied to the individual jobs.
These config variables themselves are dictionaries. Let's have a look at and example
for the the ``cfg.meteo`` variable::

    meteo:
        dir: ./input/cosmo-ghg/meteo
        prefix: laf
        nameformat: laf%Y%m%d%H
        inc: 1

These config variables can be accessed via ``cfg.meteo['dir']``, ``cfg.meteo['prefix']``, etc.
as they are Python dictionaries. 

.. hint::
    In :ref:`namelist and runscript template<namelists-section>` files
    (see next section), this accessing does not work because of how the ``.format()``
    method is implemented in Python. For that reason, the Processing Chain automatically
    creates new variables in the form of ``cfg.meteo_dir``, ``cfg.meteo_prefix``, etc.
    at the start to make them accessible for namelist and runjob templates.

List of dictionary variables
****************************

The following is a list of dictionary variables that exist for the Processing Chain.
For the individual elements of those variables, please refer to the ``config.yaml``
files within the test cases.

+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| Dictionary variable   | Used in job                                                                                                                         |
+=======================+=====================================================================================================================================+
| ``meteo``             | ``prepare_cosmo``, ``prepare_icon``, ``icontools``, ``int2lm``, ``icon``                                                            |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``icontools_runjobs`` | ``icontools``                                                                                                                       |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``input_files``       | ``prepare_icon``                                                                                                                    |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``chem``              | ``prepare_icon``                                                                                                                    |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``era5``              | ``prepare_icon``                                                                                                                    |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``cams``              | ``prepare_cosmo``                                                                                                                   |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``emissions``         | ``emissions``                                                                                                                       |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``vprm``              | ``biofluxes``                                                                                                                       |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``oem``               | ``oem``, ``cosmo``                                                                                                                  |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``online_vprm``       | ``online_vprm``                                                                                                                     |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``int2lm``            | ``prepare_cosmo``, ``emissions``, ``biofluxes``, ``octe``, ``int2lm``, ``post_int2lm``, ``cosmo``, ``post_cosmo``                   |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``post_int2lm``       | ``post_int2lm``                                                                                                                     |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``cosmo``             | ``reduce_output``, ``oem``, ``photo_rate``, ``octe``, ``check_output``, ``post_cosmo``, ``cosmo``, ``obs_nudging``, ``online_vprm`` |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``reduce_output``     | ``reduce_output``                                                                                                                   |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``post_cosmo``        | ``post_cosmo``                                                                                                                      |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``verify_chain``      | ``verify_chain``                                                                                                                    |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| ``icon``              | ``oem``, ``prepare_icon``, ``icon``                                                                                                 |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------------+

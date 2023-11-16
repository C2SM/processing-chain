.. _config-section:

The Processing Chain uses cases to describe a simulation. A case is a
subdirectory in ``cases/``, containing a ``config.yaml`` and several
`namelists` (for example ``int2lm_INPUT.cfg``) which define the
simulation.

.. _config.yaml:

Configuration File
------------------

The configuration file contains most of the information that the :ref:`jobs<jobs-section>` need to prepare and run the simulation, for example the location of the input data.
This configuration-file is imported as a module in ``run_chain.py``, and therefore
it can contain python expression which are evaluated at runtime.

..
    Creating these tables by hand is a pain. Use the script/csv in the tables/ director

General variables in ``run_chain.py``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a list of configuration variables used in the main script of the Processing Chain:

+------------------------+------------------------------------------------------------------------+
| Variable               | Description                                                            |
+========================+========================================================================+
| ``case_path``          | The path to the case directory under 'cases/' for the specified        |
|                        | casename.                                                              |
+------------------------+------------------------------------------------------------------------+
| ``casename``           | The name of the case.                                                  |
+------------------------+------------------------------------------------------------------------+
| ``chain_src_dir``      | The source directory for the processing chain, typically the current   |
|                        | working directory.                                                     |
+------------------------+------------------------------------------------------------------------+
| ``compute_account``    | The compute account to be used based on user information.              |
+------------------------+------------------------------------------------------------------------+
| ``constraint``         | The computational constraint ('gpu' or 'mc').                          |
+------------------------+------------------------------------------------------------------------+
| ``convert_gas``        | Switch for unit conversion for 'reduce_output' job.                    |
+------------------------+------------------------------------------------------------------------+
| ``cosmo_restart_in``   | The path to the COSMO model restart input directory.                   |
+------------------------+------------------------------------------------------------------------+
| ``cosmo_restart_out``  | The path to the COSMO model restart output directory.                  |
+------------------------+------------------------------------------------------------------------+
| ``email``              | The user's email address, initially set to None and updated using the  |
|                        | set_email method.                                                      |
+------------------------+------------------------------------------------------------------------+
| ``enddate``            | The end date of the simulation.                                        |
+------------------------+------------------------------------------------------------------------+
| ``force``              | Boolean indicating whether to force the processing chain to redo all   |
|                        | specified jobs.                                                        |
+------------------------+------------------------------------------------------------------------+
| ``job_list``           | List of job-names to be executed.                                      |
+------------------------+------------------------------------------------------------------------+
| ``last_cosmo_output``  | The path to the last COSMO model output in spin-up simulations.        |
+------------------------+------------------------------------------------------------------------+
| ``log_finished_dir``   | The directory for finished log files.                                  |
+------------------------+------------------------------------------------------------------------+
| ``log_working_dir``    | The directory for working log files.                                   |
+------------------------+------------------------------------------------------------------------+
| ``model``              | The model used in the simulation.                                      |
+------------------------+------------------------------------------------------------------------+
| ``mpich_cuda``         | CUDA-related environment variables, set based on the configuration     |
|                        | settings.                                                              |
+------------------------+------------------------------------------------------------------------+
| ``ntry``               | Amount of time the cosmo job is re-tried before crashing.              |
+------------------------+------------------------------------------------------------------------+
| ``ntasks_per_node``    | The number of tasks per node, based on the node type.                  |
+------------------------+------------------------------------------------------------------------+
| ``output_levels``      | Number of levels for output.                                           |
+------------------------+------------------------------------------------------------------------+
| ``restart_step_hours`` | The restart step in hours, derived from the 'restart_step' attribute.  |
+------------------------+------------------------------------------------------------------------+
| ``resume``             | Boolean indicating whether to resume the processing chain by           |
|                        | restarting the last unfinished job.                                    |
+------------------------+------------------------------------------------------------------------+
| ``run_on``             | The architecture the model runs on ('cpu' or 'gpu').                   |
+------------------------+------------------------------------------------------------------------+
| ``spinup``             | Spin-up duration in hours.                                             |
+------------------------+------------------------------------------------------------------------+
| ``startdate``          | The start date of the simulation.                                      |
+------------------------+------------------------------------------------------------------------+
| ``user_mail``          | The user's email address, determined based on system configuration.    |
+------------------------+------------------------------------------------------------------------+
| ``user_name``          | The username of the current user, obtained from the 'USER' environment |
|                        | variable.                                                              |
+------------------------+------------------------------------------------------------------------+
| ``work_root``          | The root directory for processing chain execution, typically located   |
|                        | under the source directory.                                            |
+------------------------+------------------------------------------------------------------------+


Variables to set in ``config.yaml``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here are two examples of which general variables should be set by the user in the
case configuration file.

Header of ``config.yaml`` for the ``cosmo-ghg-spinup-test`` case
================================================================

::

    model: cosmo-ghg
    constraint: gpu
    ntasks_per_node: 12
    restart_step: PT6H
    variant: spinup
    spinup: 3
    startdate: 2015-01-01T00:00:00Z
    enddate: 2015-01-01T18:00:00Z

Header of ``config.yaml`` for the ``icon-art-oem-test`` case
============================================================

::

    model: icon-art-oem
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
for the the ``cfg.meteo`` variable:::

    meteo:
        dir: ./input/cosmo-ghg/meteo
        prefix: laf
        nameformat: laf%Y%m%d%H
        inc: 1

These config variables can be accessed via ``cfg.meteo['dir']``, ``cfg.meteo['prefix']``, etc.
as they are Python dictionaries. However, in the :ref:`namelist template<namelists-section>` files
(see next section), this accessing does not work because of how the ``.format()`` method is implemented
in Python. For that reason, the Processing Chain automatically creates new variables in the form of
``cfg.meteo_dir``, ``cfg.meteo_prefix``, etc. at the start the make them available for namelist
and runjob templates.

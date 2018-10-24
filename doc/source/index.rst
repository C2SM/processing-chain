.. Processing Chain documentation master file, created by
   sphinx-quickstart on Thu Sep 27 14:11:04 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Processing Chain for **COSMO**/**COSMOART** simulations
=======================================================

This python-script prepares the necessary data and submits compute-jobs to the
queue on Piz Daint. The usage is described in the section :ref:`script-section`.
The user can decide which tasks the script should execute. These jobs are
described in :ref:`jobs-section`. Some common functionality and data-conversion
functionality is described in :ref:`tools-section`.

.. toctree::
    :maxdepth: 1
    :caption: Contents:

    runchain
    jobs
    tools

File structure
--------------

The file-structure of this package is as follows::

    + README
    + run_script.py           # main script
    + jobs/
    |    + *.py               # jobs-files
    |    \ tools/             # tools-files
    + cases/                  # examples
    |    + example/           # example COSMO
    |    |    + config.py     # user-configuration
    |    |    \ *.cfg         # templates for namelists & batch-jobs
    |    \ example_cosmoart/
    |         + config.py
    |         \ *.cfg
    \ doc/
         + source/            # *.rst documentation files
         \ Makefile           # Makefile for generating docs

.. _requirements-section:

Requirements
------------

The processing chain has to be run on Piz Daint. Depending on whether you target
**COSMO** (gpu) or **COSMOART** (mc), different modules need to be loaded.

**COSMO**::

    module load daint-gpu
    module load netcdf-python

**COSMOART**::

    module load daint-mc
    module load netcdf-python


Indices and tables
==================

* :ref:`search`

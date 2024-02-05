.. Processing Chain documentation master file, created by
   sphinx-quickstart on Thu Sep 27 14:11:04 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Processing Chain
================

The Processing Chain is a Python-based workflow tool designed to streamline
weather and climate simulations.
It facilitates the preparation of essential input data, submission of compute
jobs to the queue on CSCS HPC systems, and the implementation of post-processing
steps.
In addition to supporting standard versions of the COSMO and ICON models,
it is equipped to handle various model variants, notably COSMO-GHG
(Greenhouse Gas Extension) and ICON-ART (Aerosols and Reactive Trace Gases)

The Processing Chain can be easily customized to meet your specific requirements.
This includes defining custom workflows, creating your own simulation cases,
and integrating new jobs and auxiliary scripts.

.. toctree::
    :maxdepth: 2
    :caption: Getting Started

    features
    environment
    howtorun

.. toctree::
    :maxdepth: 2
    :caption: Configuration

    code-structure
    config
    namelists


.. toctree::
    :maxdepth: 2
    :caption: Jobs

    jobs

.. toctree::
    :maxdepth: 2
    :caption: API

    functions


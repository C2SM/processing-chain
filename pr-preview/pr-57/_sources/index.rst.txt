.. Processing Chain documentation master file, created by
   sphinx-quickstart on Thu Sep 27 14:11:04 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Processing Chain
================

The Processing Chain is a collection of Python scripts to prepare the
necessary input data, submit the compute jobs to the queue on CSCS systems
and to apply post-processing steps. In addition to the standard versions
of the COSMO and ICON models, it can also handle several variants of these
models, most notably COSMO-GHG (greenhouse gas extension) and
ICON-ART (Aerosols and Reactive Trace gases).

The Processing Chain can be flexibly adapted according to your needs, e.g.,
by defining your own workflows, by creating your own simulation cases, 
or by adding new jobs and scripts.

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


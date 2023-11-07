.. Processing Chain documentation master file, created by
   sphinx-quickstart on Thu Sep 27 14:11:04 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Processing Chain
================

The Processing Chain is a collection of Python scripts to prepare the
necessary input data, submit the compute jobs to the queue on Piz Daint
and to apply post-processing steps. In addition to the standard versions
of the COSMO and ICON models, it can als handle several variants of these
models, namely COSMO-GHG, COSMO-ART and ICON-ART. The chain can be flexibly
adapted according to your needs, e.g., by creating your own case,
adding new jobs or custom scripts.

.. toctree::
    :maxdepth: 2
    :caption: Getting Started

    features
    environment
    howtorun

.. toctree::
    :maxdepth: 2
    :caption: Configuration

    file-structure
    config
    namelists

.. toctree::
    :maxdepth: 2
    :caption: Jobs

    jobs
    tools


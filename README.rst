COSMO processing chain
======================

Command line help::
    
    $ python run_chain.py -h

Run examples::

   $ python run_chain.py example 2015-01-01 0 24
   $ python run_chain.py example_cosmoart_mother example_cosmoart_nested 2015-02-04 0 12

Requirements: amrs_
    
Build documentation locally (make sure you have sphinx installed)::

    $ cd doc/
    $ make html
    $ make text

If all else fails: read documentation source files at docs/source/*.rst

.. _amrs: https://gitlab.empa.ch/abt503/apps/amrs

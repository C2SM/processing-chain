COSMO processing chain
======================

Command line help::
    
    $ python run_chain.py -h
    
Build documentation locally (make sure you have sphinx installed)::

    $ cd doc/
    $ make html
    $ make text

If all else fails: read documentation source files at docs/source/*.rst

Requirements::

    $ module load daint-gpu # daint-mc for COSMOART
    $ module load netcdf-python
COSMO processing chain
======================

Command line help::
    
    $ python run_chain.py -h

Run examples::

   $ python run_chain.py example 2015-01-01 0 24
   $ python run_chain.py example_cosmoart 2015-02-04 0 24 -j emissions ifs_hres_bc photo_rate obs_nudging icbc int2lm comso post_cosmo

Requirements::

    $ module load daint-gpu # daint-mc for COSMOART
    $ module load netcdf-python
    
Build documentation locally (make sure you have sphinx installed)::

    $ cd doc/
    $ make html
    $ make text

If all else fails: read documentation source files at docs/source/*.rst

COSMO processing chain
======================

Command line help::
    
    $ python run_chain.py -h

Run examples::

    $ python run_chain.py example 2015-01-01 0 24
    $ python run_chain.py example_v5.6a 2015-01-01 0 24
    $ python run_chain.py example_oae 2015-01-01 0 24 -j meteo icbc oae biofluxes int2lm post_int2lm cosmo post_cosmo
    $ python run_chain.py example_oae_v5.6a 2015-01-01 0 24 -j meteo icbc oae biofluxes int2lm post_int2lm cosmo post_cosmo
    $ python run_chain.py example_vprm 2015-01-01 0 24 -j meteo icbc online_vprm int2lm post_int2lm cosmo post_cosmo
    $ python run_chain.py example_vprm_v5.6a 2015-01-01 0 24 -j meteo icbc online_vprm int2lm post_int2lm cosmo post_cosmo
    $ python run_chain.py example_cosmoart_mother example_cosmoart_nested 2015-06-26 0 12

Requirements: amrs_
      
Build documentation locally (make sure you have sphinx installed)::

    $ cd doc/
    $ make html
    $ make text

If all else fails: read documentation source files at docs/source/*.rst

.. _amrs: https://gitlab.empa.ch/abt503/apps/amrs

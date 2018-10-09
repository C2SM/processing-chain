.. _script-section:

Script
======

This is the main script of the processing chain. It reads the user's input from
the command line and from the ``config.py`` file. Then it will start the
processing chain.

Starting the chain
------------------

After loading the required modules (:ref:`requirements-section`), run the chain
with the following command: ::

    $ python run_chain.py <casename> <startdate> <hstart> <hstop> -j [jobs]

``<casename>`` is the name of a directory in the ``cases/``-directory where
there is a ``config.py``-file specifying the configurations, aswell as templates
for the necessary **int2lm** and **COSMO** namelist-files.

If you don't supply a joblist, the default joblist will be executed; ``meteo``
``icbc`` ``emissions`` ``biofluxes`` ``int2lm`` ``post_int2lm cosmo``
``post_cosmo``.

To run the **COSMO** example case, run::

    $ python run_chain.py example 2015-01-01 0 24 -j meteo icbc emissions biofluxes int2lm post_int2lm cosmo post_cosmo

To run the **COSMOART** example case, run::

    $ python run_chain.py example_cosmoart 2015-02-04 0 24 -j emissions ifs_hres_bc photo_rate obs_nudging icbc int2lm cosmo

        
run_chain.py
------------

run_chain.py reads the command-line arguments and the config-file.

.. autofunction:: run_chain.restart_runs

.. autofunction:: run_chain.run_chain

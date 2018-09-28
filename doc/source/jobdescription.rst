Jobs
====

The following jobs are available for use in the processing chain.

There is some ordering to the jobs, but you have to find out for yourself?
I think it generally goes like

1.  biofluxes | icbc | meteo | emissions
2.  int2lm
3.  post_int2lm
4.  cosmo
5.  post_cosmo | extract_2d_data | verify_chain

List of available jobs
----------------------

.. autofunction:: jobs.biofluxes.main
.. autofunction:: jobs.icbc.main
.. autofunction:: jobs.meteo.main
.. autofunction:: jobs.emissions.main
.. autofunction:: jobs.int2lm.main
.. autofunction:: jobs.post_int2lm.main
.. autofunction:: jobs.cosmo.main
.. autofunction:: jobs.post_cosmo.main
.. autofunction:: jobs.extract_2d_data.main
.. autofunction:: jobs.verify_chain.main

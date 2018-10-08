.. _jobs-section:

Jobs
====

The following jobs are available for use in the processing chain.

For **COSMO**, you can choose from the following jobs. The order of the jobs
needs to be as indicated:

1.  biofluxes | icbc | meteo | emissions
2.  int2lm
3.  post_int2lm
4.  cosmo
5.  post_cosmo | extract_2d_data | verify_chain

The same for **COSMOART**:

1. emissions | ifs_hres_bc | photo_rate | obs_nudging | icbc
2. int2lm
3. cosmo
4. post_cosmo | extract_2d_data | verify_chain

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

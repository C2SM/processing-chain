.. _jobs-section:

Jobs
====

The jobs described here are available for use in the processing chain.

For **COSMO**, you can choose from the following jobs. As some jobs depend
on the result of others, the order indicated here as to be respected:

1.  :func:`jobs.biofluxes.main` | :func:`jobs.icbc.main` | 
    :func:`jobs.meteo.main` | :func:`jobs.emissions.main`
2.  :func:`jobs.int2lm.main`
3.  :func:`jobs.post_int2lm.main`
4.  :func:`jobs.cosmo.main`
5.  :func:`jobs.post_cosmo.main` | :func:`jobs.extract_2d_data.main` |
    :func:`jobs.verify_chain.main`

The same for **COSMOART**:

1. :func:`jobs.emissions.main` | :func:`jobs.meteo.main` |
   :func:`jobs.photo_rate.main` | :func:`jobs.obs_nudging.main` |
   :func:`jobs.icbc.main`
2. :func:`jobs.int2lm.main`
3. :func:`jobs.cosmo.main`
4. :func:`jobs.post_cosmo.main` | :func:`jobs.extract_2d_data.main` |
   :func:`jobs.verify_chain.main`

Adding new jobs
---------------

Adding a new job to the chain is simple:

1. In the directory ``jobs/``, create a file called ``<jobname>.py`` containing
   a function called ``main`` which takes the same arguments as every other job.
   Make sure the function is documented with a docstring.
2. Import it in ``jobs/__init__.py`` to make it accessible to ``run_chain.py``.
3. Add the function to the documentation. You find the file describing this page
   at ``doc/source/jobs.rst``.

List of available jobs
----------------------

* :func:`jobs.biofluxes.main`
* :func:`jobs.cosmo.main`
* :func:`jobs.emissions.main`
* :func:`jobs.extract_2d_data.main`
* :func:`jobs.icbc.main`
* :func:`jobs.int2lm.main`
* :func:`jobs.meteo.main`
* :func:`jobs.obs_nudging.main`
* :func:`jobs.photo_rate.main`
* :func:`jobs.post_cosmo.main`
* :func:`jobs.post_int2lm.main`
* :func:`jobs.verify_chain.main`

-------------------------------------------

.. autofunction:: jobs.biofluxes.main

-------------------------------------------

.. autofunction:: jobs.cosmo.main

-------------------------------------------

.. autofunction:: jobs.emissions.main

-------------------------------------------

.. autofunction:: jobs.extract_2d_data.main

-------------------------------------------

.. autofunction:: jobs.icbc.main

-------------------------------------------

.. autofunction:: jobs.int2lm.main

-------------------------------------------

.. autofunction:: jobs.meteo.main

-------------------------------------------

.. autofunction:: jobs.obs_nudging.main

-------------------------------------------

.. autofunction:: jobs.photo_rate.main

-------------------------------------------

.. autofunction:: jobs.post_cosmo.main

-------------------------------------------

.. autofunction:: jobs.post_int2lm.main

-------------------------------------------

.. autofunction:: jobs.verify_chain.main


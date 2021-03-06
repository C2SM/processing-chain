.. _jobs-section:

Overview
--------

The jobs described here are available for use in the processing chain.
For every target, you can choose from a list of available jobs.
As some jobs depend on the result of others, the order indicated here
has to be respected.

**COSMO**:

1.  :func:`jobs.biofluxes.main` | :func:`jobs.prepare_data.main` | 
    :func:`jobs.emissions.main` | :func:`jobs.online_vprm.main` | 
    :func:`jobs.oae.main` | :func:`jobs.obs_nudging.main`
2.  :func:`jobs.int2lm.main`
3.  :func:`jobs.post_int2lm.main`
4.  :func:`jobs.octe.main`
5.  :func:`jobs.cosmo.main`
6.  :func:`jobs.check_output.main`
7.  :func:`jobs.reduce_output.main` 
8.  :func:`jobs.post_cosmo.main` | :func:`jobs.verify_chain.main`

**COSMOART**:

1. :func:`jobs.emissions.main` | :func:`jobs.prepare_data.main` |
   :func:`jobs.photo_rate.main` | :func:`jobs.obs_nudging.main` |
   :func:`jobs.online_vprm.main` | :func:`jobs.oae.main`
2. :func:`jobs.int2lm.main`
3. :func:`jobs.cosmo.main`
4. :func:`jobs.check_output.main`
5. :func:`jobs.reduce_output.main`
6. :func:`jobs.post_cosmo.main` | :func:`jobs.verify_chain.main`

**ICON**:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.icon.main`

**ICONART**:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.icon.main`


**ICONARTOEM**:

1.  :func:`jobs.prepare_data.main` 
2.  :func:`jobs.oae.main`
3.  :func:`jobs.icon.main`


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
* :func:`jobs.check_output.main`
* :func:`jobs.cosmo.main`
* :func:`jobs.emissions.main`
* :func:`jobs.icon.main`
* :func:`jobs.int2lm.main`
* :func:`jobs.prepare_data.main`
* :func:`jobs.oae.main`
* :func:`jobs.obs_nudging.main`
* :func:`jobs.octe.main`
* :func:`jobs.online_vprm.main`  
* :func:`jobs.photo_rate.main`
* :func:`jobs.post_cosmo.main`
* :func:`jobs.post_int2lm.main`
* :func:`jobs.reduce_output.main`
* :func:`jobs.verify_chain.main`

-------------------------------------------

.. autofunction:: jobs.biofluxes.main

-------------------------------------------

.. autofunction:: jobs.check_output.main

-------------------------------------------

.. autofunction:: jobs.cosmo.main

-------------------------------------------

.. autofunction:: jobs.emissions.main

-------------------------------------------

.. autofunction:: jobs.icon.main

-------------------------------------------

.. autofunction:: jobs.int2lm.main

-------------------------------------------

.. autofunction:: jobs.prepare_data.main

-------------------------------------------

.. autofunction:: jobs.oae.main

-------------------------------------------

.. autofunction:: jobs.obs_nudging.main

-------------------------------------------

.. autofunction:: jobs.octe.main

-------------------------------------------

.. autofunction:: jobs.online_vprm.main

-------------------------------------------

.. autofunction:: jobs.photo_rate.main

-------------------------------------------

.. autofunction:: jobs.post_cosmo.main

-------------------------------------------

.. autofunction:: jobs.post_int2lm.main

-------------------------------------------

.. autofunction:: jobs.reduce_output.main

-------------------------------------------

.. autofunction:: jobs.verify_chain.main


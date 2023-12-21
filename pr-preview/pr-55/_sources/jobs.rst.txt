.. _jobs-section:

Overview
--------

The jobs described here are available for use in the processing chain.
For every model, you can choose from a list of available jobs.
As some jobs depend on the result of others, the order indicated here
has to be respected.

``cosmo``:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.int2lm.main`
3.  :func:`jobs.cosmo.main`
4.  :func:`jobs.check_output.main`
5.  :func:`jobs.reduce_output.main` 
6.  :func:`jobs.post_cosmo.main` |
    :func:`jobs.verify_chain.main`

``cosmo-ghg``:

1.  :func:`jobs.biofluxes.main` |   
    :func:`jobs.emissions.main` | 
    :func:`jobs.online_vprm.main` | 
    :func:`jobs.oem.main` | 
    :func:`jobs.obs_nudging.main` |
    :func:`jobs.prepare_data.main`
2.  :func:`jobs.int2lm.main`
3.  :func:`jobs.post_int2lm.main`
4.  :func:`jobs.octe.main`
5.  :func:`jobs.cosmo.main`
6.  :func:`jobs.check_output.main`
7.  :func:`jobs.reduce_output.main` 
8.  :func:`jobs.post_cosmo.main` |
    :func:`jobs.verify_chain.main`

``cosmo-art``:

1. :func:`jobs.emissions.main` | 
   :func:`jobs.photo_rate.main` | 
   :func:`jobs.obs_nudging.main` |
   :func:`jobs.online_vprm.main` | 
   :func:`jobs.oem.main` |
   :func:`jobs.prepare_data.main`
2. :func:`jobs.int2lm.main`
3. :func:`jobs.cosmo.main`
4. :func:`jobs.check_output.main`
5. :func:`jobs.reduce_output.main`
6. :func:`jobs.post_cosmo.main` | 
   :func:`jobs.verify_chain.main`

``icon``:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.icon.main`

``icon-art``:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.icon.main`

``icon-art-global``:

1.  :func:`jobs.prepare_data.main`
2.  :func:`jobs.icon.main`

``icon-art-oem``:

1.  :func:`jobs.prepare_data.main` 
2.  :func:`jobs.oem.main`
3.  :func:`jobs.icon.main`


Adding New Jobs
---------------

Adding a new job to the chain is simple:

1. In the directory ``jobs/``, create a file called ``<jobname>.py`` containing
   a function called ``main`` which takes the same arguments as every other job.
   Make sure the function is documented with a docstring.
2. Import it in ``jobs/__init__.py`` to make it accessible to ``run_chain.py``.
3. Add the function to the documentation. You find the file describing this page
   at ``docs/jobs.rst``.


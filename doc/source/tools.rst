.. _tools-section:

Tools
=====

The tools are a collection of functions used by the jobs.

Helper functions
----------------

Group helpers into a single file (atm in string2char, check_target and __init__)

.. autofunction:: jobs.tools.check_target.check_target
.. autofunction:: jobs.tools.string2char.main

Conversion functions
--------------------

These functions are used by the job :func:`jobs.icbc`. They convert data into
a format usable by **int2lm**.

.. autofunction:: jobs.tools.cams4int2cosmo.main
.. autofunction:: jobs.tools.ctnoaa4int2cosmo.main
.. autofunction:: jobs.tools.mozart2int2lm.main

Other functions
---------------

.. autofunction:: jobs.tools.vprmsplit.main
.. autofunction:: jobs.tools.write_cosmo_input_bgc.main
.. autofunction:: jobs.tools.write_int2lm_input_art.main

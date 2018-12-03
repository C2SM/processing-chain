.. _tools-section:

Tools
=====

The tools are a collection of functions used by the jobs.

Conversion functions
--------------------

These functions are used by the job :func:`jobs.icbc.main`. They convert data into
a format usable by **int2lm**.

.. autofunction:: jobs.tools.cams4int2cosmo.main
.. autofunction:: jobs.tools.ctnoaa4int2cosmo.main
.. autofunction:: jobs.tools.mozart2int2lm.main

Helper functions
----------------

.. autofunction:: jobs.tools.__init__.iter_hours
.. autofunction:: jobs.tools.__init__.send_mail
.. autofunction:: jobs.tools.__init__.change_logfile
.. autofunction:: jobs.tools.__init__.create_dir
.. autofunction:: jobs.tools.__init__.copy_file
.. autofunction:: jobs.tools.__init__.check_target
.. autofunction:: jobs.tools.string2char.main

Other functions
---------------

.. autofunction:: jobs.tools.vprmsplit.main
.. autofunction:: jobs.tools.write_cosmo_input_bgc.main
.. autofunction:: jobs.tools.write_int2lm_input_art.main

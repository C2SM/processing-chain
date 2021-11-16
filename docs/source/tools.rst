.. _tools-section:

Tools
=====

The tools are a collection of functions used by the jobs. Most of those
functions are well documented and listed here. For others, one may take 
a look into ``jobs/tools`` directly.

Conversion Functions
--------------------

These functions are used by the job :func:`jobs.prepare_data.main`. They convert data into
a format usable by **int2lm**.

.. autofunction:: jobs.tools.cams4int2cosmo.main
.. autofunction:: jobs.tools.ctnoaa4int2cosmo.main
.. autofunction:: jobs.tools.mozart2int2lm.main

Helper Functions
----------------

.. autofunction:: jobs.tools.__init__.iter_hours
.. autofunction:: jobs.tools.__init__.send_mail
.. autofunction:: jobs.tools.__init__.change_logfile
.. autofunction:: jobs.tools.__init__.create_dir
.. autofunction:: jobs.tools.__init__.copy_file
.. autofunction:: jobs.tools.__init__.check_target
.. autofunction:: jobs.tools.__init__.levenshtein
.. autofunction:: jobs.tools.string2char.main
.. autofunction:: jobs.tools.helper.common_unit
.. autofunction:: jobs.tools.helper.convert_unit
.. autofunction:: jobs.tools.helper.unit2quantity
.. autofunction:: jobs.tools.helper.rotpole2wgs
.. autofunction:: jobs.tools.helper.datasets_equal
.. autofunction:: jobs.tools.helper.ccprint
.. autofunction:: jobs.tools.helper.calculate_mair
.. autofunction:: jobs.tools.helper.calculate_xgas

Other Functions
---------------

.. autofunction:: jobs.tools.vprmsplit.main
.. autofunction:: jobs.tools.write_cosmo_input_ghg.main
.. autofunction:: jobs.tools.write_int2lm_input_art.main

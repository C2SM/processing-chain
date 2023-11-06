.. _file-structure-section:

File Structure
--------------

::

    + README.rst
    + run_script.py                 # main script
    + jobs/
    |    + *.py                     # jobs-files
    |    \ tools/                   # tools-files
    + cases/                        # example test cases
    |    + cosmo-ghg-11km-test/     # COSMO-GHG example
    |    |    + config.py           # user-configuration
    |    |    \ *.cfg               # templates for namelists & batch-jobs
    |    + cosmo-art-mother-test/   # COSMO-ART example (mother domain)
    |    |    + config.py
    |    |    \ *.cfg
    |    + cosmo-art-nested-test/   # COSMO-ART example (nested domain)
    |    |    + config.py
    |    |    \ *.cfg
    |    \ icon-test                # ICON example
    |    |    + config.py
    |    |    \ *.cfg
    |    \ icon-oem-test            # ICON-OEM example
    |    |    + config.py
    |    |    \ *.cfg
    + docs/
         + source/                  # *.rst documentation files
         \ Makefile                 # Makefile for generating docs


.. _file-structure-section:

File Structure
--------------

::

    + LICENSE                       # license file
    + README.md                     # README file
    + run_chain.py                  # main script
    + cases/                        # example test cases
    |    + cosmo-ghg-spinup-test/   # COSMO-GHG example with spinup
    |    |    + config.yaml         # case configuration file
    |    |    \ *.cfg               # templates for namelists & batch jobs
    |    \ cosmo-ghg-test/          # COSMO-GHG example with standard restart
    |    |    + config.yaml         
    |    |    \ *.cfg               
    |    \ icon-art-global-test     # ICON-ART test case (global domain)
    |    |    + config.yaml
    |    |    \ *.cfg
    |    \ icon-art-oem-test        # ICON-ART test case with online emissions
    |    |    + config.yaml
    |    |    \ *.cfg
    |    \ icon-test                # ICON test case
    |    |    + config.yaml
    |    |    \ *.cfg
    + config/
    |    + models.yaml              # file to store model features and standard jobs
    + docs/                         
    |    + conf.py                  # configuration file for the Sphinx documentation builder
    |    \ *.rst                    # documentation files (reStructuredText)
    + env/
    + jenkins
    + jobs/
    |    + *.py                     # job scripts
    |    \ tools/
    |    |    + *.py                # tool scripts
    + src/                          # path to store source codes (spack, models, etc.)


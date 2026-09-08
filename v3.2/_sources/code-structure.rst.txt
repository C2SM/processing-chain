.. _code-structure-section:

Code Structure
--------------

The Processing Chain code is structured as follows:

.. code-block:: bash

    $ tree -L 3 -F --dirsfirst 
    .
    ├── cases/                      # folder where all cases are stored
    │   ├── cosmo-ghg-spinup-test/  # COSMO-GHG test case with spinup restart
    │   │   ├── config.yaml         # case configuration file
    │   │   ├── *.cfg               # templates for namelists & batch jobs
    │   │   └── *.csv               # CSV files with tracer information
    │   ├── cosmo-ghg-test/         # COSMO-GHG testcase with standard restart
    │   │   ├── config.yaml
    │   │   ├── *.cfg
    │   │   └── *.csv
    │   ├── icon-art-global-test/   # ICON-ART test case (global domain)
    │   │   ├── config.yaml
    │   │   ├── icon_runjob.cfg     # template for ICON-ART runjob
    │   │   ├── *.sh                # pre-processing scripts
    │   │   └── mypartab
    │   ├── icon-art-oem-test/      # ICON-ART test case with online emissions
    │   │   ├── config.yaml
    │   │   └── *.cfg
    │   └── icon-test/              # ICON test case
    │       ├── config.yaml
    │       └── *.cfg
    ├── docs/                       # folder for Sphinx documentation 
    │   ├── _static/                # folder for static assets
    │   │   ├── custom.css          # custom CSS styles
    │   │   └── *.png|ico           # additional image assets
    │   ├── tables/                 # folder for tables used in documentation
    │   │   └── *.csv               # CSV files containing table data
    │   ├── conf.py                 # configuration file for the Sphinx builder
    │   └── *.rst                   # documentation files (reStructuredText)
    ├── ext/                        # folder for other code (spack, models, etc.)
    ├── jobs/
    │   ├── tools/
    │   │   └── *.py                # tool scripts
    │   └── *.py                    # job scripts
    ├── machines/                   # machine-specific environment setup
    │   └── <machine>/
    │       ├── modules.sh          # load system software
    │       └── setup_env.sh        # system software + venv activation
    ├── testing/                    # scripts to test the chain
    │   ├── scripts/
    │   │   └── *.sh                # data-download and test scripts
    │   └── run_tests.sh            # driver running all of the above
    ├── LICENSE                     # license file
    ├── README.md                   # README file
    ├── config.py                   # file containing the Config class
    ├── requirements.txt            # pip requirements file
    ├── run_chain.py                # main script
    └── workflows.yaml              # file to store workflows with job dependencies

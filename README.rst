Processing Chain for COSMO and ICON Simulations
===============================================

The Processing Chain is a python script that prepares necessary input data,
submits compute-jobs to the queue on Piz Daint and does post-processing steps.
It supports different types of simulations, including **COSMO**, **COSMO-GHG**,
**COSMO-ART**, **ICON** and **ICON-ART**. The chain can flexibly be adapted
according to your needs, e.g., by creating your own case or adding new jobs.

Setting up your Virtual Environment using ``pip``
*************************************************

The following steps allow you to create and use your own virtual environment
to run the Processing Chain. It is assumed that you are in the root folder
of this repository.

1. Install EasyBuild Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Processing Chain uses the ``cartopy`` package, which depends on ``GEOS`` and ``PROJ``.
However, these packages cannot be installed via ``pip``. Therefore, they have to be
installed using EasyBuild. For more information about the EasyBuild framework, you 
can have a look here: https://user.cscs.ch/computing/compilation/easybuild/ ::

    module load daint-gpu EasyBuild-custom
    eb easybuild/GEOS-3.9.1-CrayGNU-20.11.eb -r
    eb easybuild/PROJ-4.9.3-CrayGNU-20.11.eb -r
    
2. Create the Virtual Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

Now we create the virtual environment named ``venv``, which is installed into
the folder of the same name::

    module load cray-python/3.8.5.0
    python -m venv --system-site-packages venv
    source venv/bin/activate
    module load GEOS/3.9.1-CrayGNU-20.11
    module load PROJ/4.9.3-CrayGNU-20.11

3. Install Requirements
~~~~~~~~~~~~~~~~~~~~~~~

The requirements.txt_ contains all packages that are needed to run the Processing Chain. 
To install the requirements, type::

    python -m pip install -r requirements.txt

4. Create the settings.ini File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run the following bash script once to register your email address::

    ./generate_settings_file.sh

Afterwards, close and re-open your terminal, and continue with the next section.

Run the Chain
*************

Once everything has been set up correctly according to the above steps,
you just need to execute the following command to load the necessary modules
and activate your environment::

    source modules.env

To test if your environment has been successfully set,
use the command line help to see the available arguments for the main script::

    python run_chain.py -h

To run the example cases with their standard jobs, type::

    python run_chain.py cosmo-ghg-11km-test 2015-01-01 0 24

or::

    python run_chain.py cosmo-art-mother-test cosmo-art-nested-test 2015-06-26 0 12

or::

    python run_chain.py icon-test 2018-01-01 0 24

or::

    python run_chain.py icon-oem-test 2018-01-01 0 24
      
Documentation
*************

To set up your own simulation case or to check all available configuration options, 
please read the official documentation_.

Contributing
************

If you think your (well-documented) developments might also be useful to others,
we encourage you to create a pull request for this repository.

Credits
*******

The Processing Chain was originally developed at Empa_ by the 
`Atmospheric Modeling and Remote Sensing`_ group. The following persons 
contributed significantly to the initial development (in alphabetic order):

* Pavle Arsenovic
* Dominik Brunner
* Jean-Matthieu Haussaire
* Gerrit Kuhlmann
* Qing Mu
* David Ochsner
* Michael Steiner

The current code owner is Michael Jähn (michael.jaehn@c2sm.ethz.ch).

.. _requirements.txt: requirements.txt
.. _documentation: https://processing-chain.readthedocs.io
.. _python-cdo: https://pypi.org/project/cdo
.. _Empa: https://www.empa.ch
.. _Atmospheric Modeling and Remote Sensing: https://www.empa.ch/web/s503/modelling-remote-sensing

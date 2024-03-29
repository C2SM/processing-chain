# <img src="https://polybox.ethz.ch/index.php/s/yc3zMmoXKyI2rJm/download" width="64" valign="middle" alt="Processing Chain"/> Processing Chain

The Processing Chain is a python script that prepares necessary input
data, submits compute-jobs to the queue on Piz Daint and does
post-processing steps. It supports different types of models and simulation types,
including **COSMO**, **COSMO-GHG**, **COSMO-ART**, **ICON** and
**ICON-ART**. The chain can flexibly be adapted according to your needs,
e.g., by creating your own case or adding new jobs.

## Environment Setup

To setup your conda environment for the Processing Chain, please refer
to the part in the [official documentation](https://c2sm.github.io/processing-chain/latest/environment.html).

## Run the Chain

To activate your conda environment, type:

    conda activate proc-chain

To test if your environment has been successfully set, use the command
line help to display the available arguments for the main script:

    ./run_chain.py -h

To run the test cases with their standard jobs, please ensure
that you clone the Processing Chain to `$SCRATCH`, as input and
output data are stored in subdirectories of the Processing Chain repository
itself.

> **Note**: For your own setups, you can use the Processing Chain
> on a backed-up file system like `/project` or `/store`. In that case,
> adapt the configuration file `config.yaml` in your case folder so that
> the output files are written to a specified folder on `$SCRATCH`.

For these pre-defined test cases, you can use the Jenkins script

    ./jenkins/scripts/jenkins.sh

This script calls other scripts that are located in `jenkins/scripts/` and will: 
- activate the conda environment
- setup spack-c2sm
- download input data to `input/`
- build `int2lm`, `cosmo-ghg`, `icon` and `icon-art`
- test the following cases:
    - `cosmo-ghg-spinup-test`
    - `cosmo-ghg-test`
    - `icon-test`
    - `icon-art-oem-test`
    - `icon-art-global-test`

To run the test cases manually, type:

    # replace <casename> with one of the above tests
    ./run_chain.py <casename>

## Documentation

For more information about the file structure, configuration options,
namelist templates etc., please read the [official
documentation](https://c2sm.github.io/processing-chain/latest/).

## Contributing

If you think your (well-documented) developments might also be useful to
others, we encourage you to create a pull request for this repository.

## Credits

The Processing Chain was originally developed in 2018 at
[Empa](https://www.empa.ch) by the [Atmospheric Modeling and Remote
Sensing](https://www.empa.ch/web/s503/modelling-remote-sensing) group.
The following persons contributed significantly to the initial
development (in alphabetic order):

  - Pavle Arsenovic
  - Dominik Brunner
  - Jean-Matthieu Haussaire
  - Gerrit Kuhlmann
  - Qing Mu
  - David Ochsner
  - Michael Steiner

Since 2021, the code is public and hosted by C2SM. More information can
be found at the
[C2SM User Landing Page](https://c2sm.github.io/tools/processing_chain.html).
The current code owner is Michael Jähn (<michael.jaehn@c2sm.ethz.ch>).

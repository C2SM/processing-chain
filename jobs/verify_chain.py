#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import netCDF4 as nc

from . import tools


def comp_data(dataset1, dataset2, variables):
    """Use tools.helper.datasets_equal to compare the datasets.
    """
    tools.helper.datasets_equal(dataset1, dataset2, variables, verbose=True)


def main(cfg, model_cfg):
    """Compare outputs of the chain to a reference.

    Looks for the reference-file in ``cfg.verify_chain['reference_dir']``.
    
    Looks for the output file in ``cfg.verify_chain['output_dir']`` (if not ``None``), else it
    goes to the output directory created by the **COSMO**-job.
    
    In the dict ``cfg.verify_chain['values_to_check']``, the user specifies the names of the
    files to be compared as keys, and the variables to compare as a list.

    To compare the temperatures of the last output of the example case, the
    following variables should be added to the ``config.yaml`` file: ::

        verify_chain['reference_dir'] = os.path.join(input_root, "reference_output")
        verify_chain['output_dir'] = None
        verify_chain['values_to_check'] = {("reference_lffd2015010200.nc","lffd2015010200.nc"):
              ['T']}

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes
    model_cfg : dict 
        Model configuration settings loaded from the ``config\/models.yaml`` file.
    """
    logging.info("Started verification")
    for (ref_file,
         run_file), variables in cfg.verify_chain['values_to_check'].items():
        logging.info("Comparing " + str(variables))

        # reference file location
        ref_file_path = os.path.join(cfg.verify_chain['reference_dir'],
                                     ref_file)

        # run data location
        if cfg.verify_chain['output_dir'] is None:
            # Standard output location
            run_file_path = os.path.join(
                cfg.output_root, cfg.startdate_sim_yyyymmddhh + "_" +
                cfg.enddate_sim_yyyymmddhh, "cosmo_output", run_file)
        else:
            # User-provided output location
            run_file_path = os.path.join(cfg.verify_chain['output_dir'],
                                         run_file)

        logging.info("Output file: " + str(run_file_path))
        logging.info("Reference file: " + str(ref_file_path))

        # compare data
        with nc.Dataset(ref_file_path) as ref_data, nc.Dataset(
                run_file_path) as run_data:
            comp_data(ref_data, run_data, variables)

    logging.info("Finished verification")

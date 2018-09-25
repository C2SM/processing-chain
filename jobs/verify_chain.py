#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Job to verify the correctness of the chain. The output of the example-case is
# compared to a reference output.

# Author: dao, david.ochsner@empa.ch

import os
import logging
import netCDF4 as nc
from numpy import allclose

def import_data(filename, mode='r'):
    return nc.Dataset(filename, mode)


def import_datasets(ref_path, run_path):
    """
    Read the reference and run datasets
    """
    try:
        ref_data = import_data(ref_path)
    except:
        logging.error("Reading reference data failed")
        raise
    try:
        run_data = import_data(run_path)
    except:
        logging.error("Reading run data failed")
        raise
    return ref_data, run_data


def datasets_equal(dataset1, dataset2, variables):
    found_discrepancy = False
    for var in variables:
        if not allclose(dataset1[var], dataset2[var]):
            logging.info(var + ": Failed!")
            found_discrepancy = True
        else:
            logging.info(var + ": Passed")
    return not found_discrepancy


def main(starttime, hstart, hstop, cfg):
    logging.info("Started verification")
    for (ref_file, run_file), variables in cfg.values_to_check.items():
        logging.info("Comparing " + str(variables))

        # reference file location
        ref_file_path = os.path.join(cfg.reference_dir, ref_file)

        # run data location
        if cfg.output_dir is None:
            # Standard output location
            run_file_path = os.path.join(cfg.output_root,
                                         starttime.strftime('%Y%m%d%H') +
                                         "_" + str(int(hstart)) + "_" +
                                         str(int(hstop)),
                                         "cosmo_output",
                                         run_file)
        else:
            # User-provided output location
            run_file_path = os.path.join(cfg.output_dir, run_file)

        logging.info("Output file: " + str(run_file_path))
        logging.info("Reference file: " + str(ref_file_path))

        # read data
        ref_data, run_data = import_datasets(ref_file_path, run_file_path)

        #compare data
        identical = datasets_equal(ref_data, run_data, variables)
        if not identical:
            print("\033[91m" + "Some output fields don't match the reference!\n"
                  + "\033[0m" + "Check logfiles for details")
        else:
            print("Verification successful, output and reference agree")

    logging.info("Finished verification")

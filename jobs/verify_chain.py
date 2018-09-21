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


def compare_vals(dataset1, dataset2, variables):
    for var in variables:
        print("Comparing " + var)
        # if we use xarray: something like xr.apply_ufunc(isclose, dataset1[var], dataset2[var])
        if not allclose(dataset1[var], dataset2[var]):
            print("cosmo-ouput is not equal for " + var)
            # log some stuff
            break
        


def main(starttime, hstart, hstop, cfg):
    print("Verification!")

    for (ref_file, run_file), variables in cfg.values_to_check.items():
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

        # read data
        ref_data, run_data = import_datasets(ref_file_path, run_file_path)

        #compare data
        compare_vals(ref_data, run_data, variables)

    print("Done")

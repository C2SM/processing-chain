#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Job to verify the correctness of the chain. The output of the example-case is
# compared to a reference output.

# Author: dao, david.ochsner@empa.ch

import os
import logging
import netCDF4 as nc

def import_to_netCDF4(filename, mode='r'):
    return nc.Dataset(filename, mode)

def import_datasets(ref_path, run_path):
    """
    Read the reference and run datasets
    """
    try:
        ref_data = import_to_netCDF4(ref_path)
    except:
        logging.error("Reading reference data failed")
        raise
    try:
        run_data = import_to_netCDF4(run_path)
    except:
        logging.error("Reading run data failed")
        raise
    return ref_data, run_data

def compare_vals(dataset1, dataset2, vars):
    print(dataset1['T'][0,10,10,10])
    print(dataset2['T'][0,10,10,10])

def main(start_time, hstart, hstop, cfg):
    print("Verification!")

    # reference file location
    reference_file_name = "reference_lffd2015010200.nc"
    reference_file_path = os.path.join(cfg.reference_dir, reference_file_name)

    # run data location
    if cfg.ouput_file_path is None:
        # Standard output location
        run_file_name = "lffd2015010200.nc"
        run_file_path = os.path.join(cfg.output_root,
                                     starttime.strftime('%Y%m%d%H') +
                                     "_" + str(int(hstart)) + "_" +
                                     str(int(hstop)),
                                     "cosmo_output",
                                     run_file_name)
    else:
        run_file_path = cfg.output_file_path

    # read data
    ref_data, run_data = import_datasets(reference_file_path, run_file_path)

    #compare data
    compare_vals(ref_data, run_data, [])

    print("Done")


    




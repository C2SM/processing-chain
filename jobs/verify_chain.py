#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Job to verify the correctness of the chain. The output of the example-case is
# compared to a reference output.

# Author: dao, david.ochsner@empa.ch

import os
import netCDF4 as nc

def import_to_netCDF4(filename, mode='r'):
    return nc.Dataset(filename, mode)

def main(start_time, hstart, hstop, cfg):
    reference_file_name = "reference_lffd2015010200.nc"
    print("Verification!")
    print("Looking at: " + str(cfg.reference_dir))

    reference_file_path = os.path.join(cfg.reference_dir, reference_file_name)

    ref_data = import_to_netCDF4(reference_file_path)

    print(ref_data['T'][0,10,10,10])




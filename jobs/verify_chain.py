#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Job to verify the correctness of the chain. The output of the example-case is
# compared to a reference output.

# Author: dao, david.ochsner@empa.ch

import netCDF4 as nc

def main(start_time, hstart, hstop, cfg):
    print("Verification!")
    print("Looking at: " + str(cfg.reference_dir))


#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""\
Convert string to char for attributes globally and for variables.
"""

from __future__ import print_function

import sys
import numpy as np
import netCDF4

if (sys.version_info.major) >= 3:
    basestring = (str,bytes)

def string2char(nc, name, value):
    if isinstance(value, basestring):
        value = np.array(list(value), 'c')
        nc.setncattr(name, value)


def main(filename):
    """Convert the variable names of a netcdf-file from strings to a 
    ``np.array`` of chars.
    
    Parameters
    ----------
    filename : str
        Path to the netcdf-file
    """
    with netCDF4.Dataset(filename, 'a') as nc:
        for name in nc.ncattrs():
            value = nc.getncattr(name)
            string2char(nc, name, value)

        for v in nc.variables:
            var = nc.variables[v]
            for name in var.ncattrs():
                value = var.getncattr(name)
                string2char(var, name, value)

if __name__ == '__main__':
    main(sys.argv[1])

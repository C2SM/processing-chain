#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Script used by `reduce_output` job

Author: Michael Jaehn (jae), Empa, Switzerland
"""

from __future__ import print_function

import sys
import numpy as np
import netCDF4 as nc
from datetime import datetime
from datetime import timedelta
import glob
import os
import shutil
import csv

import amrs.misc.time as time
import amrs.misc.chem as chem
import amrs.constants as constants

import logging
logging.basicConfig(level=logging.DEBUG)


def get_attrs(v):
    return dict((k, v.getncattr(k)) for k in v.ncattrs())


def str2char(s):
    return np.array(list(s), 'c')


def append_variable(nc, name, values, attrs=None):
    """\
    Append variable (name, values) to netCDF file.
    """

    from six import string_types

    if attrs is None:
        attrs = {}

    var = nc.createVariable(name, values.dtype, ('time', 'rlat', 'rlon'),
                            zlib=True)
    var[0] = values

    # add or update attributes
    for key, value in attrs.items():
        if isinstance(value, string_types):
            var.setncattr(key, str2char(value))
        else:
            var.setncattr(key, value)


def reduce_output(infile, cfiles, h, nout_levels, output_path, fname_met, lsd):
    dtime = infile.split('lffd', 1)[1][0:10]
    """Get path and filename for output file"""
    path, output_filename = os.path.split(infile)
    outfile = os.path.join(output_path, output_filename)
    if 1:
        with nc.Dataset(infile, 'r') as inf, \
             nc.Dataset(outfile, 'w') as outf:
            # Copy global attributes all at once via dictionary
            outf.setncatts(inf.__dict__)
            # Copy dimensions
            for name, dimension in inf.dimensions.items():
                outf.createDimension(name, (len(dimension)
                    if not dimension.isunlimited() else None))
            # Create new dimension level2
            outf.createDimension('level2', nout_levels)
            # Get level information
            level = len(inf.dimensions['level'])
            level1 = len(inf.dimensions['level1'])


            # Copy variables
            for varname in inf.variables.keys():
                logging.info('%s: %s' % (output_filename, varname))
                var = inf.variables[varname]
                attrs = get_attrs(var)
                if (var.dimensions == ('time', 'level',
                                       'rlat', 'rlon') or
                    var.dimensions == ('time', 'level1',
                                       'rlat', 'rlon')):
                    var_dimensions = ('time', 'level2',
                                      'rlat', 'rlon')
                elif (var.dimensions == ('time', 'level',
                                         'srlat', 'rlon')):
                    var_dimensions = ('time', 'level2',
                                      'srlat', 'rlon')
                elif (var.dimensions == ('time', 'level',
                                         'rlat', 'srlon')):
                    var_dimensions = ('time', 'level2',
                                      'rlat', 'srlon')
                else:
                    var_dimensions = var.dimensions

                if len(var.dimensions) > 2:
                    if (varname in lsd):
                        outf.createVariable(varname, var.dtype,
                                            var_dimensions,
                                            zlib=True, 
                                            least_significant_digit=lsd[varname])
                    else:
                        outf.createVariable(varname, var.dtype,
                                            var_dimensions,
                                            zlib=True,
                                            least_significant_digit=8)
                else:
                    outf.createVariable(varname, var.dtype,
                                        var_dimensions, zlib=True)

                for attr in var.ncattrs():
                    outf[varname].setncattr(attr, inf[varname].getncattr(attr))

                if varname == 'T' or varname == 'P' or  varname == 'PS' \
                or varname == 'QV':
                    fname_met[varname] = infile

                gas = None
                if varname != 'rotated_pole':
                    # Check for 3D data and extract only lower_levels
                    if 'level1' in var.dimensions and \
                        len(var.dimensions) == 4:
                            lstart = level1 - 1 - nout_levels
                            lend = level1 - 1
                    elif 'level' in var.dimensions and \
                        len(var.dimensions) == 4:
                            lstart = level - 1 - nout_levels
                            lend = level - 1
                    else:
                        outf[varname][:] = inf[varname][:]

                outvar = outf.variables[varname]
                if varname.startswith('CO2_'):
                    gas = 'CO2'
                    outf[varname][:] = inf[varname][:,lstart:lend,:,:] * \
                                       constants.M['air'] / constants.M[gas] * 1e6
                    outvar.units = 'ppm'
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'CO2_mass_fraction', 'XCO2')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'mass fraction',
                            'column-averaged dry-air mole fraction')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'Mass fraction',
                            'Column-averaged dry-air mole fraction')
                    attrs['units'] = 'molecules cm-2'
                    attrs2 = attrs.copy()
                    attrs2['standard_name'] = attrs2['standard_name'].replace(
                                              'XCO2', 'YCO2')
                    attrs2['long_name'] = attrs2['long_name'].replace(
                                          'dry-air', 'moist-air')
                elif varname.startswith('CO_'):
                    gas = 'CO'
                    outf[varname][:] = inf[varname][:,lstart:lend,:,:] * \
                                       constants.M['air'] / constants.M[gas] * 1e9
                    outvar.units = 'ppb'
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'CO_mass_fraction', 'XCO')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'mass fraction',
                            'column-averaged dry-air mole fraction')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'Mass fraction',
                            'Column-averaged dry-air mole fraction')
                    attrs['units'] = 'molecules cm-2'
                    attrs2 = attrs.copy()
                    attrs2['standard_name'] = attrs2['standard_name'].replace(
                                              'XCO', 'YCO')
                    attrs2['long_name'] = attrs2['long_name'].replace(
                                          'dry-air', 'moist-air')
                elif varname.startswith('CH4_'):
                    gas = 'CH4'
                    outf[varname][:] = inf[varname][:,lstart:lend,:,:] * \
                                       constants.M['air'] / constants.M[gas] * 1e9
                    outvar.units = 'ppb'
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'CH4_mass_fraction', 'XCH4')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'mass fraction',
                            'column-averaged dry-air mole fraction')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'Mass fraction',
                            'Column-averaged dry-air mole fraction')
                    attrs['units'] = 'molecules cm-2'
                    attrs2 = attrs.copy()
                    attrs2['standard_name'] = attrs2['standard_name'].replace(
                                              'XCH4', 'YCH4')
                    attrs2['long_name'] = attrs2['long_name'].replace(
                                          'dry-air', 'moist-air')
                elif varname.startswith('NO2_') or varname.startswith('NOX_'):
                    gas = 'NO2'
                    outf[varname][:] = inf[varname][:,lstart:lend,:,:] * \
                                       constants.M['air'] / constants.M[gas] * 1e6
                    outvar.units = 'ppm'
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'NOX', 'NO2')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'NOX', 'NO2')
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'NO2_mass_fraction', 'XNO2')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'mass fraction',
                            'column-averaged dry-air mole fraction')
                    attrs['long_name'] = attrs['long_name'].replace(
                            'Mass fraction',
                            'Column-averaged dry-air mole fraction')
                    attrs['units'] = 'molecules cm-2'
                    attrs2 = attrs.copy()
                    attrs2['standard_name'] = attrs2['standard_name'].replace(
                                              'XNO2', 'YNO2')
                    attrs2['long_name'] = attrs2['long_name'].replace(
                                          'dry-air', 'moist-air')
                elif varname != 'rotated_pole' and len(var.dimensions) == 4 and \
                ('level1' in var.dimensions or 'level' in var.dimensions):
                    outf[varname][:] = inf[varname][:,lstart:lend,:,:]

                if gas:
                    with nc.Dataset(fname_met['QV'], 'r') as inf_qv, \
                         nc.Dataset(fname_met['T'], 'r') as inf_t, \
                         nc.Dataset(fname_met['PS'], 'r') as inf_ps, \
                         nc.Dataset(fname_met['P'], 'r') as inf_p:
                        qv = np.array(inf_qv.variables['QV'][0])
                        t = np.array(inf_t.variables['T'][0])
                        ps = np.array(inf_ps.variables['PS'][0])
                        p = np.array(inf_p.variables['P'][0])

                        xm = np.array(inf.variables[varname][0])
                        mair = chem.calculate_mair(p, ps, h)
                        # Column-averaged dry-air mole fraction (molecules/cm2)
                        column = chem.calculate_xgas(xm, mair, gas, qv)
                        column = column.astype('f4')
                        logging.info('%s: X%s' % (output_filename, varname))
                        append_variable(outf, 'X%s' % varname, column,
                                        attrs=attrs)
                        # Column-averaged moist-air mole fraction (molecules/cm2)
                        column2 = chem.calculate_xgas(xm, mair, gas, 0.0)
                        column2 = column2.astype('f4')
                        logging.info('%s: Y%s' % (output_filename, varname))
                        append_variable(outf, 'Y%s' % varname, column2,
                                        attrs=attrs2)

    else:
        logging.error("Reduce data from file %s to file %s "
                      "failed." % (infile, outfile))

    return fname_met


def main(indir, outdir, strdate_start, strdate_end, nout_levels, csvfile):
    """
    Script to reduce output.
    
    Output: TODO

    Parameters (TODO)
    ----------
    opath : str
        Output path where the processed data is going to be written
    """

    """Get list of constant files"""
    cfiles = []
    read_cfile = False
    for infile in sorted(glob.glob(os.path.join(indir, "lffd*[0-9]c*.nc"))): 
        cfiles.append(infile)
        if not read_cfile:
            # Read the first constant file and store height value
            logging.info(infile)
            with nc.Dataset(infile) as inf:
                h = np.array(inf.variables['HHL'][0])
            read_cfile = True

    if not read_cfile:
        logging.error('Constant file could not be read')


    """Get list of files"""
    infiles = sorted(glob.glob(os.path.join(indir, "lffd*.nc")))

    """Remove constant files from file list"""
    for cfile in cfiles:
        if cfile in infiles:
            infiles.remove(cfile)

    date_start = datetime.strptime(strdate_start, '%Y%m%d%H')
    date_end = datetime.strptime(strdate_end, '%Y%m%d%H')

    """Only take relevant date period into account"""
    for infile in list(infiles):
        basename = os.path.split(infile)[1]
        timestr = basename.split('lffd',1)[1][:10]
        mytime = datetime.strptime(timestr, '%Y%m%d%H')
        if mytime < date_start or mytime > date_end:
            infiles.remove(infile)

    """Initialize dict to remember file name for meteo variables"""
    fname_met = { 'T': None,
                  'P': None,
                  'PS': None,
                  'QV': None,
                }

    """Translate csv file to dict"""
    lsd = {}
    with open(csvfile) as inf:
        for row in csv.reader(inf, delimiter=','):
            if not row[0].startswith('#'):
                print(row[0])
                lsd[row[0]] = int(row[1])

    """Loop over all input files and apply output reduction"""
    for infile in infiles:
        fname_met = reduce_output(infile, cfiles, h, int(nout_levels),
                                  outdir, fname_met, lsd)


if __name__ == '__main__':
    indir = sys.argv[1]
    outdir = sys.argv[2]
    strdate_start = sys.argv[3]  
    strdate_end = sys.argv[4]  
    nout_levels = sys.argv[5]
    csvfile = sys.argv[6]
    main(indir, outdir, strdate_start, strdate_end, nout_levels, csvfile)


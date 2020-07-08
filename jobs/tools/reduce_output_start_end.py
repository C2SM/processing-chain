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

import helper

import logging
logging.basicConfig(level=logging.DEBUG)


def get_attrs(v):
    return dict((k, v.getncattr(k)) for k in v.ncattrs())


def str2char(s):
    return np.array(list(s), 'c')

def search_met(infile,fname_met):
    """
    Checks which file contains the meteorology data
    """
    time = None
    tocheck = []
    print(infile)
    for f in infile:
        basename = os.path.split(f)[1]
        print(basename)
        timestr = basename.split('lffd',1)[1][:10]
        mytime = datetime.strptime(timestr, '%Y%m%d%H')
        if time is None:
            time = mytime
        if mytime == time:
            tocheck.append(f)

    for f in tocheck:
        with nc.Dataset(f, 'r') as inf:
            for var in ['T', 'P', 'PS', 'QV']:
                if var in inf.variables:
                    basename = os.path.split(f)[1]
                    fileending = basename.split('lffd',1)[1][10:-3]
                    fname_met[var] = fileending

    return fname_met
    
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


def reduce_output(infile, cfiles, h, nout_levels, output_path, fname_met, lsd,
                  convert):
    dtime = infile.split('lffd', 1)[1][0:10]
    """Get path and filename for output file"""
    path, output_filename = os.path.split(infile)
    outfile = os.path.join(output_path, output_filename)
    with nc.Dataset(infile, 'r') as inf, \
         nc.Dataset(outfile, 'w') as outf:
        # Copy global attributes all at once via dictionary
        outf.setncatts(inf.__dict__)
        # Copy dimensions
        for name, dimension in inf.dimensions.items():
            outf.createDimension(name, (len(dimension)
                if not dimension.isunlimited() else None))

        # Get level information
        level = len(inf.dimensions['level'])
        level1 = len(inf.dimensions['level1'])

        # Create new dimension level2
        if nout_levels == -1:
            nout_levels = level
        else:
            outf.createDimension('level2', nout_levels)

        # Copy variables
        for varname in inf.variables.keys():
            logging.info('%s: %s' % (output_filename, varname))
            var = inf.variables[varname]
            attrs = get_attrs(var)
            if len(var.dimensions)==4:
                if var.dimensions[1] == 'level':
                    var_dimensions = list(var.dimensions)
                    var_dimensions[1] = 'level'+'2'*(nout_levels != level)
                elif var.dimensions[1] == 'level1':
                    var_dimensions = list(var.dimensions)
                    var_dimensions[1] = 'level'+'2'*(nout_levels != level) + '1'*(nout_levels == level)
                else:
                    var_dimensions = var.dimensions
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
                                        zlib=True)
            else:
                outf.createVariable(varname, var.dtype,
                                    var_dimensions, zlib=True)

            for attr in var.ncattrs():
                outf[varname].setncattr(attr, inf[varname].getncattr(attr))

            gas = None
            if varname != 'rotated_pole':
                # Check for 3D data and extract only lower_levels
                if 'level1' in var.dimensions and \
                    len(var.dimensions) == 4:
                        lstart = -nout_levels - (nout_levels == level)
                elif 'level' in var.dimensions and \
                    len(var.dimensions) == 4:
                        lstart = -nout_levels
                else:
                    outf[varname][:] = inf[varname][:]

            if (varname.startswith('CO2_') or varname.startswith('CO_') or 
                varname.startswith('CH4_') or varname.startswith('C14_') or 
                varname.startswith('NOX_') or varname.startswith('NO2_')):
                outvar = outf.variables[varname]
                gas = varname.split('_')[0]
                if gas == 'NOX':
                    attrs['standard_name'] = attrs['standard_name'].replace(
                            'NOX', 'NO2')
                    gas = 'NO2'
                field = inf[varname][:,lstart:,:,:] 
                unit = attrs['units']

                if convert:
                    outvar.units = helper.common_unit(gas)
                    if gas == 'C14':
                        outf[varname][:] = chem.convert_unit(
                                               field, unit, outvar.units, 45.993e-3)
                    else:
                        outf[varname][:] = chem.convert_unit(
                                               field, unit, outvar.units, gas)
                else:
                    outvar.units = unit
                    outf[varname][:] = field

                attrs['standard_name'] = attrs['standard_name'].replace(
                        '%s_mass_fraction' % gas, 'X%s' % gas)
                attrs['units'] = outvar.units
                attrs2 = attrs.copy()
                attrs2['standard_name'] = attrs2['standard_name'].replace(
                                          'X%s' % gas, 'Y%s' % gas)
            elif varname != 'rotated_pole' and len(var.dimensions) == 4 and \
            ('level1' in var.dimensions or 'level' in var.dimensions):
                outf[varname][:] = inf[varname][:,lstart:,:,:]

            if gas:
                fname_met_base = os.path.split(infile)[0]+'/'+os.path.split(infile)[1][:14]
                with nc.Dataset(fname_met_base+fname_met['QV']+'.nc', 'r') as inf_qv, \
                     nc.Dataset(fname_met_base+fname_met['T']+'.nc', 'r') as inf_t, \
                     nc.Dataset(fname_met_base+fname_met['PS']+'.nc', 'r') as inf_ps, \
                     nc.Dataset(fname_met_base+fname_met['P']+'.nc', 'r') as inf_p:
                    qv = np.array(inf_qv.variables['QV'][0])
                    t = np.array(inf_t.variables['T'][0])
                    ps = np.array(inf_ps.variables['PS'][0])
                    p = np.array(inf_p.variables['P'][0])

                    xm = np.array(inf.variables[varname][0])
                    mair = chem.calculate_mair(p, ps, h)

                    # Column-averaged dry-air mole fraction
                    column = chem.calculate_xgas(xm, mair, gas, qv)
                    column = column.astype('f4')
                    logging.info('%s: X%s' % (output_filename, varname))
                    append_variable(outf, 'X%s' % varname, column,
                                    attrs=attrs)

                    # Column-averaged moist-air mole fraction
                    column2 = chem.calculate_xgas(xm, mair, gas, 0.0)
                    column2 = column2.astype('f4')
                    logging.info('%s: Y%s' % (output_filename, varname))
                    append_variable(outf, 'Y%s' % varname, column2,
                                    attrs=attrs2)

    return fname_met


def main(indir, outdir, strdate_start, strdate_end, nout_levels, csvfile,
         convert_gas):
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
    fname_met = search_met(infiles, fname_met)
    
    """Translate csv file to dict"""
    variables = helper.find_variables_file(csvfile)
    lsd = variables['lsd'].to_dict()
    
    """Loop over all input files and apply output reduction"""
    for infile in infiles:
        fname_met = reduce_output(infile, cfiles, h, int(nout_levels),
                                  outdir, fname_met, lsd, convert_gas=='True')


if __name__ == '__main__':
    indir = sys.argv[1]
    outdir = sys.argv[2]
    strdate_start = sys.argv[3]  
    strdate_end = sys.argv[4]  
    nout_levels = sys.argv[5]
    csvfile = sys.argv[6]
    convert_gas = sys.argv[7]
    main(indir, outdir, strdate_start, strdate_end, nout_levels, csvfile,
         convert_gas)


#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import shutil
import datetime as dt
import glob 
import netCDF4 as nc
import subprocess
import numpy as np
import time
import resource

from . import tools
import amrs.misc.chem as chem
import amrs.nc.compress as compress
import amrs.constants as constants

lsd = { 'U': 2,
        'V': 2,
        'W': 3,
        'T': 2,
        'P': 1,
        'T_SO': 2,
        'T_2M': 2,
        'TD_2M': 2,
        'PS': 1,
        'CLCT': 3,
        'CLC': 3,
        'HPBL': 1,
        'ALHFL_S': 2,
        'ASHFL_S': 2,
        'ASOB_S': 2,
        'ATHB_S': 2,
        'APAB_S': 2,
        'ASWDIR_S': 2,
        'ASWDIFD_S': 2,
        'CO2_BG': 2, # ppm
        'CO2_A': 2, # ppm
        'CO2_GPP': 2, # ppm
        'CO2_RA': 2, # ppm
        'CO_BG': 1, # ppb
        'CO_A': 1, # ppb
        'CH4_BG': 1, # ppb
        'CH4_A': 1, # ppb
      } 

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


def reduce_output(infile, cfiles, h, nout_levels, output_path, fname_met):
    dtime = infile.split('lffd', 1)[1][0:10]
    """Get path and filename for output file"""
    path, output_filename = os.path.split(infile)
    outfile = os.path.join(output_path, output_filename)
    logging.info('Create reduced output file %s' % output_filename)
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
                logging.info(varname)
                var = inf.variables[varname]
                attrs = get_attrs(var)
                try:
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
                        outf[varname].setncattr(attr,
                            inf[varname].getncattr(attr))
                except RuntimeError:
                    pass

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
                        logging.info('X%s' % varname)
                        append_variable(outf, 'X%s' % varname, column,
                                        attrs=attrs)
                        # Column-averaged moist-air mole fraction (molecules/cm2)
                        column2 = chem.calculate_xgas(xm, mair, gas, 0.0)
                        column2 = column2.astype('f4')
                        logging.info('Y%s' % varname)
                        append_variable(outf, 'Y%s' % varname, column2,
                                        attrs=attrs2)

        """Force garbage collection"""
        logging.info('Memory usage         : % 2.2f MB' % round(
                     resource.getrusage(resource.RUSAGE_SELF)
                     .ru_maxrss/1024.0,1))

    else:
        logging.error("Reduce data from file %s to file %s "
                      "failed." % (infile,outfile))

    return fname_met


def main(starttime, hstart, hstop, cfg):
    """Extracts 2D column data and a fixed number of levels from **COSMO**
    output directory to a new netCDF file. Those files are written into 
    a new directory ``cosmo_output_reduced``.

    The number of levels is set by the configuration variable
    ``cfg.output_levels``.
    
    Important: This code only works if the tracers, for which the 
    column-averaged dry-air ('X') and moist-air ('Y') mole fractions are
    calculated, are 1.) saved in a separate output file and 2.) the output
    file appears alphabetically after the meteorological variables.
    For example, use a GRIBOUT section suffix '_met' for standard **COSMO**
    output, and '_trc' for tracers.

    Parameters
    ----------	
    starttime : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the starttime
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """

    cosmo_output = cfg.cosmo_output
    copy_path = os.path.join(cfg.output_root, starttime.strftime('%Y%m%d%H')+
                             "_"+str(int(hstart))+"_"+str(int(hstop)))
    output_path = os.path.join(copy_path, "cosmo_output_reduced")

    date = dt.datetime.today()

    to_print = """reduce_output

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" %date.strftime("%s")
    
    logging.info(to_print)
    
    tools.create_dir(output_path, "output")

    if cfg.compute_host!="daint":
        logging.error("The reduce_output script is supposed to be run on daint only, "
                      "not on %s" % cfg.compute_host)
        sys.exit(1)

    """Get list of constant files"""
    cfiles = []
    read_cfile = False
    for infile in sorted(glob.glob(os.path.join(cosmo_output, "lffd*[0-9]c*.nc"))): 
        cfiles.append(infile)
        if not read_cfile:
            # Read the first constant file and store height value
            logging.info(infile)
            with nc.Dataset(infile) as inf:
                h = np.array(inf.variables['HHL'][0])
            read_cfile = True
            logging.info('Successfully read constant file %s' % infile)

    if not read_cfile:
        logging.error('Constant file could not be read')

    """Copy constant file directly"""
    if os.path.exists(cfiles[0]):
        shutil.copy(cfiles[0], output_path)  
        logging.info('Copied constant file %s to %s.' % (cfiles[0], output_path))
    else:
        logging.error('Constant file could not be copied.')

    """Get list of files"""
    infiles = sorted(glob.glob(os.path.join(cosmo_output, "lffd*.nc")))

    """ Remove constant files from file list"""
    for cfile in cfiles:
        if cfile in infiles:
            infiles.remove(cfile)
            logging.info('Removed %s from file list.' % cfile)

    """Initialize dict to remember file name for meteo variables"""
    fname_met = { 'T': None,
                  'P': None,
                  'PS': None,
                  'QV': None,
                }

    """Loop over all input files and apply output reduction"""
    for infile in infiles:
        fname_met = reduce_output(infile, cfiles, h, cfg.output_levels,
                                  output_path, fname_met)

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)

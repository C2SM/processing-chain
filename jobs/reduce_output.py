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

from . import tools
import amrs.misc.chem as chem
import amrs.nc.compress as compress


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

    var = nc.createVariable(name, values.dtype, ('time', 'rlat', 'rlon'), zlib=True)
    var[0] = values

    # add or update attributes
    for key, value in attrs.items():
        if isinstance(value, string_types):
            var.setncattr(key, str2char(value))
        else:
            var.setncattr(key, value)


def main(starttime, hstart, hstop, cfg):
    """Extracts 2D column data and first 15 levels from **COSMO**
    output directory to a new file (``cosmo_output_reduced``)
    
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

    read_cfile = False
    for infile in sorted(glob.glob(os.path.join(cosmo_output, "lffd*[0-9]c*.nc"))): 
        if not read_cfile:
            logging.info(infile)
            # Read the first constant file and store height value
            with nc.Dataset(infile) as inf:
                h = inf.variables['HHL'][0]
            read_cfile = True
            logging.info('Successfully read constant file %s' % infile)

    if not read_cfile:
        logging.error('Constant file could not be read')

    for infile in sorted(glob.glob(os.path.join(cosmo_output, "lffd*.nc"))): 
        if os.path.exists(infile) and read_cfile and not 'c_' in infile:
            # Get path and filename for output file
            path, output_filename = os.path.split(infile)
            outfile = os.path.join(output_path, output_filename)
            logging.info('%s -> %s' % (infile, outfile))
            # Remove any pre-existing output file
            if os.path.exists(outfile): os.remove(outfile)
            # TODO: Write all data into one file per time step
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
                    outf.createDimension('level2', cfg.output_levels)
                    # Get level information
                    level = len(inf.dimensions['level'])
                    level1 = len(inf.dimensions['level1'])

                    # Read meteorological variables (needed for mair and XCO2
                    # calculations) 
                  # if '_met' in infile:
                  #     logging.info('Get temperature field')
                  #     T = inf.variables['T'][0]
                  #     logging.info('Get pressure field')
                  #     p = inf.variables['P'][0]
                  #     logging.info('Get surface pressure field')
                  #     ps = inf.variables['PS'][0]
                  #     logging.info('Get specific humidity field')
                  #     q = inf.variables['QV'][0]
                  #     logging.info('Calculate mass of dry air')
                  #     # mass of dry air (kg/m2)
                  #     mair = chem.calculate_mair(p, ps, h)
                  #     logging.info('Done!')

                    # Copy variables
                    for varname in inf.variables.keys():
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

                            outf.createVariable(varname, var.dtype,
                                                var_dimensions)
                            for attr in var.ncattrs():
                                outf[varname].setncattr(attr,
                                    inf[varname].getncattr(attr))
                        except RuntimeError:
                            pass

                        if varname != 'rotated_pole':
                            # Check for 3D data and extract only lower_levels
                            if 'level1' in var.dimensions and \
                                len(var.dimensions) == 4:
                                    lstart = level1 - 1 - cfg.output_levels
                                    outf[varname][:] = inf[varname][:,lstart:level1-1,:,:]
                            elif 'level' in var.dimensions and \
                                len(var.dimensions) == 4:
                                    lstart = level - 1 - cfg.output_levels
                                    outf[varname][:] = inf[varname][:,lstart:level-1,:,:]
                            else:
                                outf[varname][:] = inf[varname][:]

                        # Attributes
                        if varname.startswith('CO2_'):
                            attrs['standard_name'] = attrs['standard_name'].replace(
                                    'CO2_mass_fraction', 'CO2_column-averaged_dry-air_mole_fraction')
                            attrs['long_name'] = attrs['long_name'].replace(
                                    'CO2_mass_fraction', 'CO2_column-averaged_dry-air_mole_fraction')
                            attrs['units'] = 'ppm'
                            attrs2 = attrs.copy()
                            attrs2['long_name'] = attrs2['long_name'].replace('dry-air', 'moist-air')
                            attrs2['standard_name'] = attrs2['standard_name'].replace('dry-air', 'moist-air')

                        elif varname.startswith('NO2_'):
                            attrs['standard_name'] = attrs['standard_name'].replace(
                                    'NOX_mass_fraction', 'NO2_vertical_column_density')
                            attrs['long_name'] = attrs['long_name'].replace(
                                    'NOX_mass_fraction', 'NO2_vertical_column_density')
                            attrs['units'] = 'molecules cm-2'

                        elif varname.startswith('CO_'):
                            attrs['standard_name'] = attrs['standard_name'].replace(
                                    'CO_mass_fraction', 'CO_vertical_column_density')
                            attrs['long_name'] = attrs['long_name'].replace(
                                    'CO_mass_fraction', 'CO_vertical_column_density')
                            attrs['units'] = 'molecules cm-2'

                        elif varname.startswith('CH4_'):
                            attrs['standard_name'] = attrs['standard_name'].replace(
                                    'CH4_mass_fraction', 'CH4_vertical_column_density')
                            attrs['long_name'] = attrs['long_name'].replace(
                                    'CH4_mass_fraction', 'CH4_vertical_column_density')
                            attrs['units'] = 'molecules cm-2'

                      # gas = None
                      # if varname.startswith('CO2_'): gas = 'CO2'
                      # if varname.startswith('CO_'): gas = 'CO'
                      # if varname.startswith('CH4_'): gas = 'CH4'
                      # if varname.startswith('NO2_'): gas = 'NO2'

                      # if gas:
                        #   # Column-averaged dry-air mole fraction (molecules/cm2)
                        #   xm = inf.variables[varname][0]
                        #   column = chem.calculate_camf(xm, mair, gas, q)
                        #   column = column.astype('f4')
                        #   append_variable(outf, 'X%s' % varname, column,
                        #                   attrs=attrs)
                        #   # Column-averaged moist-air mole fraction (molecules/cm2)
                        #   column2 = chem.calculate_camf(xm, mair, gas, 0.0)
                        #   column2 = column2.astype('f4')
                        #   append_variable(outf, 'Y%s' % varname, column2,
                        #                   attrs=attrs2)
                        #   # Vertical column densities
                        #   column = chem.calculate_vcd(xm, p, T, h, gas, top=0)
                        #   column = column.astype('f4')
                        #   append_variable(outf, 'VCD_%s' % varname, column,
                        #                   attrs=attrs)

                logging.info('Apply compression')
                compress.main(outfile, outfile+'.tmp', overwrite=True,
                              set_precision=True)

            else:
                logging.error("Extracting 2D data from file %s to file %s "
                              "failed." % (infile,outfile))

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)

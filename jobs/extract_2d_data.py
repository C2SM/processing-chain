#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import shutil
import datetime as dt
import glob 
import netCDF4 as nc
from . import tools

import amrs


def get_attrs(v):
    return dict((k, v.getncattr(k)) for k in v.ncattrs())


def create_2d_fields(input_filename, constant_filename, output_filename,
        overwrite=True, do_levels=True):
    """\
    Create input data for calculating satellite swaths.

    input_filename: COSMO output for a given hour.
    constant_filename: COSMO output for constant fields

    output_filename: output filename for 2d fields
    """
    sys.stdout.flush()
    attrs = {
            'DESCRIPTION':  'Vertical integrated/averaged tracer fields',
            'DATAORIGIN':   'Tracer fields from COSMO-GHG simulations',
            'CREATOR':      config.user_name,
            'EMAIL':        config.user_email,
            'AFFILIATION':  config.user_affiliation,
            'VERSION':      '0.1',
            'DATE CREATED': time.ctime(time.time()),
            'STUDY':        config.study,
    }

    if not os.path.exists(input_filename):
        print(input_filename, '...missing')
        sys.stdout.flush()
        return 42

    if not overwrite and os.path.exists(output_filename):
        return 42

    print(input_filename, '...working')
    sys.stdout.flush()

    tracers = config.tracers.copy()

    with netCDF4.Dataset(constant_filename) as nc:
        h = nc.variables['HHL'][0]

    with netCDF4.Dataset(input_filename) as nc, netCDF4.Dataset(output_filename, 'w') as out:
        lon = nc.variables['lon'][:]
        lat = nc.variables['lat'][:]
        rlon = nc.variables['rlon'][:]
        rlat = nc.variables['rlat'][:]

        print(output_filename)
        sys.stdout.flush()
        sc.io.init_netcdf(out, rlon, rlat, lon, lat, attrs=attrs)

        T = nc.variables['T'][0]
        p = nc.variables['P'][0]
        ps = nc.variables['PS'][0]

        # specific humidity
        q = nc.variables['QV'][0]

        clct = nc.variables['CLCT'][0]
        clct_attrs = get_attrs(nc.variables['CLCT'])

        # write to netCDF 
        sc.io.append_variable(out, 'CLCT', clct, attrs=clct_attrs)

        # mass of dry air (kg/m2)
        mair = amrs.misc.chem.calculate_mair(p, ps, h)

        for tracer in tracers:
            xm = nc.variables[tracer][0]
            attrs = get_attrs(nc.variables[tracer])


            if tracer.startswith('CO2_'):
                if do_levels:
                    ground = amrs.misc.chem.xm_to_xv(xm[-1:-15], 'CO2')
                column = amrs.misc.chem.calculate_xco2(xm, mair, q)
                column = column.astype('f4')

                if tracer == 'CO2_BG':
                    column2 = amrs.misc.chem.calculate_xco2(xm, mair, 0.0)
                    column2 = column2.astype('f4')

            elif tracer.startswith('NOX_'):
                if do_levels:
                    ground = amrs.misc.chem.xm_to_cm(xm[-1], p[-1], T[-1])
                    ground = amrs.misc.chem.nox_to_no2(ground)
                    ground = amrs.misc.chem.cm_to_xm(ground, p[-1], T[-1])
                    ground = amrs.misc.chem.xm_to_xv(ground, 'NO2')

                tracer = tracer.replace('NOX_', 'NO2_')

                column = amrs.misc.chem.calculate_vcd(xm, p, T, h, 'NOx')
                column = column.astype('f4')

            elif tracer.startswith('CO_'):
                if do_levels:
                    ground = amrs.misc.chem.xm_to_xv(xm[-1], 'CO')
                column = amrs.misc.chem.calculate_vcd(xm, p, T, h, 'CO')
                column = column.astype('f4')

            elif tracer.startswith('CH4_'):
                if do_levels:
                    ground = amrs.misc.chem.xm_to_xv(xm[-1], 'CH4')
                column = amrs.misc.chem.calculate_vcd(xm, p, T, h, 'CH4')
                column = column.astype('f4')
            else:
                raise ValueError('Tracers "%s" not handled.' % tracer)


            if tracer.startswith('CO2_'):
                attrs['standard_name'] = attrs['standard_name'].replace(
                        'CO2_mass_fraction', 'CO2_column-averaged_dry-air_mole_fraction')
                attrs['long_name'] = attrs['long_name'].replace(
                        'CO2_mass_fraction', 'CO2_column-averaged_dry-air_mole_fraction')
                attrs['units'] = 'ppm'

                if tracer == 'CO2_BG':
                    attrs2 = attrs.copy()
                    attrs2['long_name'] = attrs2['long_name'].replace('dry-air', 'moist-air')
                    attrs2['standard_name'] = attrs2['standard_name'].replace('dry-air', 'moist-air')

            elif tracer.startswith('NO2_'):
                attrs['standard_name'] = attrs['standard_name'].replace(
                        'NOX_mass_fraction', 'NO2_vertical_column_density')
                attrs['long_name'] = attrs['long_name'].replace(
                        'NOX_mass_fraction', 'NO2_vertical_column_density')
                attrs['units'] = 'molecules cm-2'

            elif tracer.startswith('CO_'):
                attrs['standard_name'] = attrs['standard_name'].replace(
                        'CO_mass_fraction', 'CO_vertical_column_density')
                attrs['long_name'] = attrs['long_name'].replace(
                        'CO_mass_fraction', 'CO_vertical_column_density')
                attrs['units'] = 'molecules cm-2'

            elif tracer.startswith('CH4_'):
                attrs['standard_name'] = attrs['standard_name'].replace(
                        'CH4_mass_fraction', 'CH4_vertical_column_density')
                attrs['long_name'] = attrs['long_name'].replace(
                        'CH4_mass_fraction', 'CH4_vertical_column_density')
                attrs['units'] = 'molecules cm-2'



            # write ground concentrations
            if do_levels:
                sc.io.append_variable(
                    out, '%s' % tracer,
                    ground, attrs=None
                )

            # write columns
            if tracer.startswith('CO2'):
                sc.io.append_variable(
                    out,
                    'X%s' % tracer,
                    column, attrs=attrs
                )
                if tracer == 'CO2_BG':
                    sc.io.append_variable(
                        out,
                        'Y%s' % tracer,
                        column2, attrs=attrs2
                    )
            else:
                sc.io.append_variable(
                    out,
                    '%s' % tracer,
                    column, attrs=attrs
                )


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

    to_print = """extract_2d_data

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" %date.strftime("%s")
    
    logging.info(to_print)
    
    tools.create_dir(output_path, "output")

    if cfg.compute_host!="daint":
        logging.error("The copy script is supposed to be run on daint only, "
                      "not on %s" %cfg.compute_host)
        sys.exit(1)

    for infile in sorted(glob.glob(os.path.join(cosmo_output, "lffd*.nc"))): 
        if os.path.exists(infile) and not 'c_' in infile:
            # Get path and filename for output file
            path, output_filename = os.path.split(infile)
            logging.info(output_filename)
            outfile = os.path.join(output_path, output_filename)
            # Remove any pre-existing output file
            if os.path.exists(outfile): os.remove(outfile)
            try:
                with nc.Dataset(infile, 'r') as inf, \
                     nc.Dataset(outfile, 'w') as outf:
                    # Copy global attributes all at once via dictionary
                    outf.setncatts(inf.__dict__)
                    # Copy dimensions
                    for name, dimension in inf.dimensions.items():
                        outf.createDimension(name, (len(dimension)
                            if not dimension.isunlimited() else None))
                    # Create new dimension level_2d
                    outf.createDimension('level_2d', 1)
                    # Get level information
                    level = len(inf.dimensions['level'])
                    level1 = len(inf.dimensions['level1'])
                    # Copy variables
                    for varname in inf.variables.keys():
                        var = inf.variables[varname]
                        try:
                            if (var.dimensions == ('time', 'level',
                                                   'rlat', 'rlon') or
                                var.dimensions == ('time', 'level1',
                                                   'rlat', 'rlon')):
                                var_dimensions = ('time', 'level_2d',
                                                  'rlat', 'rlon')
                            elif (var.dimensions == ('time', 'level',
                                                     'srlat', 'rlon')):
                                var_dimensions = ('time', 'level_2d',
                                                  'srlat', 'rlon')
                            elif (var.dimensions == ('time', 'level',
                                                     'rlat', 'srlon')):
                                var_dimensions = ('time', 'level_2d',
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
                            # Check for 3D data and extract only ground level
                            if 'level1' in var.dimensions and \
                                len(var.dimensions) == 4:
                                outf[varname][:] = inf[varname][:,level1-1,:,:]
                            elif 'level' in var.dimensions and \
                                len(var.dimensions) == 4:
                                outf[varname][:] = inf[varname][:,level-1,:,:]
                            else:
                                outf[varname][:] = inf[varname][:]

            except:
                logging.error("Extracting 2D data from file %s to file %s "
                              "failed." % (infile,outfile))

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)

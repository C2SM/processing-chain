#!/usr/bin/env python
# -*- coding: utf-8 -*-

### DEVELOPMENT VERSION ###

import logging
import os
import shutil
import datetime as dt
import glob 
import netCDF4 as nc
from . import tools


def main(start_time, hstart, hstop, cfg):
    """Extracts 2D and surface data from **COSMO** output directory to a new
    file (``cosmo_output_2d``)
    
    Parameters
    ----------	
    start_time : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the start_time
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """

    cosmo_output = cfg.cosmo_output
    copy_path = os.path.join(cfg.output_root, start_time.strftime('%Y%m%d%H')+
                             "_"+str(int(hstart))+"_"+str(int(hstop)))
    output_path = os.path.join(copy_path, "cosmo_output_2d")

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

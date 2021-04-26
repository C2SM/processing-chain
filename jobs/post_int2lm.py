#!/usr/bin/env python
# -*- coding: utf-8 -*-

#################
##  TODO :
##  - The 3h increment could be a parameter as well
#################

import logging
import os
import shutil
import datetime as dt
import glob 
import netCDF4 as nc
from . import tools


def main(start_time, hstart, hstop, cfg):
    """Combine multiple **int2lm** tracer-output files into a single one for
    **COSMO**.

    Only necessary for **COSMO** simulations.

    **int2lm** puts tracers into different netCDF files. Combine the files
    specified in ``cfg.post_int2lm_species`` into a single netCDF file for
    **COSMO**.

    If ``cfg.spinup`` and ``cfg.post_int2lm_species_spinup`` are present,
    also read in the specified variables and take them as an input for 
    **COSMO**.
    
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
    tools.check_target(cfg, tools.Target.COSMOGHG)

    int2lm_output = cfg.int2lm_output
    inidate_int2lm_yyyymmddhh = cfg.inidate_int2lm_yyyymmddhh

    chem_list = cfg.post_int2lm_species

    date = dt.datetime.today()
    

    to_print = """POST_INT2LM

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" %date.strftime('%s')
    
    logging.info(to_print)

    logging.info('BOUNDARY CONDITIONS: Adding tracers from lbfd*t.nc files to regular int2lm files.')
    # Add background tracers in all 'lbfd**t.nc' files to
    # normal lbfd files, because CAMS tracers are only every 3 hours.
    # We add it 4 times to hour-1, hour+0, hour+1 and hour+2
    for f in sorted(glob.glob(os.path.join(int2lm_output,'lbfd*t.nc'))):
        logging.info(f)
        yyyymmddhh_str = os.path.basename(f)[4:-4]
        yyyymmddhh = dt.datetime.strptime(yyyymmddhh_str, '%Y%m%d%H')

        for hour in tools.iter_hours(yyyymmddhh, -1, 2):
            outfile1= os.path.join(int2lm_output,
                                   hour.strftime('lbfd%Y%m%d%H' + '.nc'))
            if os.path.exists(outfile1):
                with nc.Dataset(outfile1,'a') as outf,nc.Dataset(f) as inf:
                    for chem in chem_list:
                        try:
                            outf.createVariable(chem, inf[chem].dtype,
                                                inf[chem].dimensions)
                            for attr in inf[chem].ncattrs():
                                outf[chem].setncattr(attr,inf[chem].getncattr(attr))
                            logging.info('Variable ' + chem + 'added.')
                        except RuntimeError:
                            logging.warning('Variable {} already present in {}'
                                            .format(chem, outfile1))
                        outf[chem][:] = inf[chem][:]
    logging.info("OK")

    # Meteo spinup simulation with tracer recycling
    if cfg.target.subtarget is tools.Subtarget.SPINUP and \
    hasattr(cfg, 'post_int2lm_species_spinup') and not cfg.first_one:
        var_list = cfg.post_int2lm_species_spinup
        logging.info('INITIAL CONDITIONS (RECYCLING): Adding tracers %s from last COSMO run (%s) to regular int2lm files.' % (str(var_list), cfg.last_cosmo_output))

        infile_name = 'lffd' + start_time.strftime('%Y%m%d%H') + '*.nc'
        infile_paths = sorted(glob.glob(os.path.join(cfg.last_cosmo_output,
                                              infile_name)))
        outfile_name = 'laf' + inidate_int2lm_yyyymmddhh + '.nc'
        outfile_path = os.path.join(int2lm_output, outfile_name)

        for infile_path in infile_paths:
            with nc.Dataset(infile_path) as inf, nc.Dataset(outfile_path,'a') as outf:
                for var in var_list:
                    if var in inf.variables.keys():
                        if var in outf.variables.keys():
                            logging.warning('Recycling: Variable {} already present in {}'
                                            .format(var, outfile_path))
                        else:    
                            outf.createVariable(varname=var,
                                                datatype=inf[var].dtype,
                                                dimensions=inf[var].dimensions)
                            logging.info('Recycled variable ' + var + ' from '
                                         + infile_path)
                            logging.info('into ' + outfile_path)
                        outf[var][:] = inf[var][:]
                        for attr in inf[var].ncattrs():
                            outf[var].setncattr(attr, inf[var].getncattr(attr))
    # Normal run
    else:
        logging.info('INITIAL CONDITIONS: Adding tracers from laf*t.nc files to regular int2lm files.')
        infile =  os.path.join(int2lm_output, "laf" + 
                               inidate_int2lm_yyyymmddhh + "t.nc") 
        if os.path.exists(infile):
            outfile = infile[:-4]+ ".nc"
            try:
                with nc.Dataset(infile) as inf, nc.Dataset(outfile,"a") as outf:
                    for chem in chem_list:
                        if chem in outf.variables.keys():
                            logging.warning('Variable %s already present in file %s'
                                            % (chem, outfile))
                        else:
                            outf.createVariable(chem,inf[chem].dtype,inf[chem].dimensions)
                            outf[chem][:] = inf[chem][:]
                            for attr in inf[chem].ncattrs():
                                outf[chem].setncattr(attr,inf[chem].getncattr(attr))
                            logging.info('Variable %s added as initial condition.'
                                         % chem)
            except:
                logging.error('Appending tracers %s from file %s to file %s failed.'
                              % (str(chem_list), infile, outfile))

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime('%s')

    logging.info(to_print)

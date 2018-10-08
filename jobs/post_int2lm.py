#!/usr/bin/env python
# -*- coding: utf-8 -*-

#################
##  TO DO :
##  - Have the list of chemical species as parameters
##  - The 3h increment could be a parameter as well

### DEVELOPMENT VERSION ###

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
    tools.check_target(cfg, 'COSMO')

    int2lm_output = cfg.int2lm_output
    inidate_int2lm_yyyymmddhh = start_time.strftime('%Y%m%d%H')

    chem_list = cfg.post_int2lm_species

    date = dt.datetime.today()
    

    to_print = """POST_INT2LM

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" %date.strftime("%s")
    
    logging.info(to_print)
    logging.info("Adding tracers from laf*t.nc and lbfd*t.nc files to regular int2lm files ")
    # add CO2, CO and NOX tracers in "laf**t.nc" file to normal laf file
    infile = os.path.join(int2lm_output,"laf"+inidate_int2lm_yyyymmddhh+"t.nc")

    if os.path.exists(infile):
        outfile = os.path.join(int2lm_output,"laf"+inidate_int2lm_yyyymmddhh+".nc")
        try:
            with nc.Dataset(infile) as inf,nc.Dataset(outfile,"a") as outf:
                for chem in chem_list:
                    if chem in outf.variables.keys():
                        logging.warning('Variable %s already present in file %s'
                                        % (chem, outfile))
                    else:
                        outf.createVariable(chem,inf[chem].dtype,inf[chem].dimensions)
                        outf[chem][:] = inf[chem][:]
                        for attr in inf[chem].ncattrs():
                            outf[chem].setncattr(attr,inf[chem].getncattr(attr))
        except:
            logging.error("Appending BG tracer from file %s to file %s failed." % (infile,outfile))

    # Add CO2, CO and NOX background tracers in all "lbfd**t.nc" files to
    # normal lbfd files, because CAMS tracers are only every 3 hours.
    # We add it 4 times to hour-1, hour+0, hour+1 and hour+2
    for f in glob.glob(os.path.join(int2lm_output,"lbfd*t.nc")):
        logging.info(f)
        yyyymmddhh_str = os.path.basename(f)[4:-4]
        yyyymmddhh = dt.datetime.strptime(yyyymmddhh_str,"%Y%m%d%H")

        #changetime=False
        for hour in tools.iter_hours(yyyymmddhh, -1, 2):
            outfile1= os.path.join(int2lm_output,hour.strftime("lbfd%Y%m%d%H"+".nc"))
            if os.path.exists(outfile1):
                with nc.Dataset(outfile1,"a") as outf,nc.Dataset(f) as inf:
                    # for all the files except the first one, we need to copy the time coordinate in the reverse 
                    # direction because ncks -A -v CO2_BG also copies all associated coordinates
                    # if changetime:
                    #     outf.createDimension("time",len(inf["time"]))
                    #     outf.createVariable("time",inf["time"].dtype,inf["time"].dimensions)
                    #     outf["time"][:] = inf["time"][:]
                    # else:
                    #     changetime=False
                    for chem in chem_list:
                        try:
                            outf.createVariable(chem,inf[chem].dtype,inf[chem].dimensions)#("rlat","rlon"))#
                            for attr in inf[chem].ncattrs():
                                outf[chem].setncattr(attr,inf[chem].getncattr(attr))
                        except RuntimeError:
                            pass
                        outf[chem][:] = inf[chem][:]

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)

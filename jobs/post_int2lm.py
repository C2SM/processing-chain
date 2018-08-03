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

def main(start_time, hstart, hstop, cfg):
    int2lm_output = os.environ["int2lm_output"]
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

    if not os.path.exists(infile):
        logging.error("Appending BG tracer using file %s failed."%infile)
        raise FileNotFoundError

    print("im in 3")

    outfile = os.path.join(int2lm_output,"laf"+inidate_int2lm_yyyymmddhh+".nc")
    with nc.Dataset(infile) as inf,nc.Dataset(outfile,"a") as outf:
        for chem in chem_list:
            outf.createVariable(chem,inf[chem].dtype,inf[chem].dimensions)
            outf[chem][:] = inf[chem][:]
    print("im in 4")
    # Add CO2, CO and NOX background tracers in all "lbfd**t.nc" files to
    # normal lbfd files, because CAMS tracers are only every 3 hours.
    # We add it 4 times to hour-1, hour+0, hour+1 and hour+2
    for f in glob.glob("lbfd*t.nc"):
        logging.info(f)
        yyyymmddhh = f[4:-4]
        #changetime=False
        for time in tools.iter_hours(yyyymmddhh, -1, 2):
            outfile1= os.join.path(int2lm_output,"lbfd"+time.strftime("%Y%m%d%H"+".nc"))
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
                    outf.createVariable(chem,inf[chem].dtype,("lat","lon"))#inf[chem].dimensions)
                    outf[chem][:] = inf[chem][:]

    print("im in 5")
    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
====================================================="""%date.strftime("%s")

    logging.info(to_print)
    #shutil.copy(logfile, os.path.join(logfile,"post_int2lm"))


# if __name__ == "__main__":
    
#     main(start_time, hstart, hstop, cfg)

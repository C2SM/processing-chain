#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Check presence of CarbonTracker files, preprocess, and soft link
#
# Result in case of success: all emission input-files necessary are found in 
#                            ${int2lm_input}/icbc/
#
# Dominik Brunner, Jul 2013
#
# 2013-07-17 Initial release, based on Christoph Knotes' artbd.bash
# 2018-06-21 Translated to Python (hjm)

### DEVELOPMENT VERSION ###

import os
import shutil
import logging

from . import tools
#from "./tools" import cams4int2cosmo
#from tools import ctnoaa4int2cosmo
#from tools import mozart2int2lm

def main(starttime,hstart,hstop,cfg):
    try:
        CAMS = dict(fullname= "CAMS",nickname="cams",executable="cams4int2cosmo",indir = cfg.cams_dir_orig, outdir = cfg.cams_dir_proc, param=cfg.cams_parameters)
        CT = dict(fullname="CarbonTracker",nickname="ct",executable="ctnoaa4int2cosmo",indir = cfg.ct_dir_orig, outdir = cfg.ct_dir_proc,param=cfg.ct_parameters)
    except:
        print("todo")

    # TO DO 
    #MOZART = dict(fullname="MOZART", nickname="mozart",executable="cams4int2cosmo")
        
    logging.info("CAMS, CarbonTracker and/or MOZART data")

    scratch_path = os.path.join(os.environ['int2lm_input'],'icbc') #cfg.int2lm_input, 'icbc')
    try:
        os.makedirs(scratch_path, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating icbc input folder failed")
        raise

    inv_to_process=[CAMS,CT] #,"MOZART"]
    for inv in inv_to_process:
        logging.info(inv["fullname"]+" files")
        #process_inv(starttime,hstart,hstop,increment,inv,cfg)
        
        for p in inv["param"]:
            inc = p["inc"]
            for time in tools.iter_hours(starttime, hstart, hstop,inc):
                logging.info(time)

                filename = os.path.join(inv["outdir"],p["suffix"]+"_"+time.strftime("%Y%m%d%H")+".nc")
                if not os.path.exists(filename):
                    logging.info(filename)
                    try:    
                        to_call = getattr(tools,inv["executable"])
                        to_call.main(time,inv["indir"],inv["outdir"],p)
                    except:
                        logging.error("Preprocessing "+inv["fullname"] + " data failed")
                        raise

                    # copy to (temporary) run input directory
                    try:
                        shutil.copy(filename, scratch_path)
                    except FileNotFoundError:
                        logging.error("Processed "+inv["fullname"]+ " "+p["suffix"]+" data file not found at %s" % filename)
                        raise
                    except (PermissionError, OSError):
                        logging.error("Copying processed "+inv["fullname"]+" "+p["suffix"]+" data file failed")
                        raise

                logging.info("OK")

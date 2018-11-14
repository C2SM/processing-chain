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
    """Copy and if necessary process CAMS & CarbonTracker or Mozart
    files for **int2lm**

    Necessary for both **COSMO** and **COSMOART** simulations.

    Copy CAMS (**COSMO**) or CarbonTracker (**COSMO**) or Mozart (**COSMOART**)
    files from project folder to int2lm input folder on scratch
    (``cfg.int2lm_input/icbc``).
    If needed, launch ``cams4int2cosmo``, ``ctnoaa4int2cosmo`` or
    ``mozart2int2lm`` to adapt the files to **int2lm**.
    
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
    inv_to_process = []
    if cfg.target is tools.Target.COSMO:
        try:
            CAMS = dict(fullname = "CAMS",
                        nickname = "cams",
                        executable = "cams4int2cosmo",
                        indir = cfg.cams_dir_orig,
                        outdir = cfg.cams_dir_proc,
                        param = cfg.cams_parameters)
            inv_to_process.append(CAMS)
        except AttributeError:
            pass
        try:
            CT = dict(fullname = "CarbonTracker",
                      nickname = "ct",
                      executable = "ctnoaa4int2cosmo",
                      indir = cfg.ct_dir_orig,
                      outdir = cfg.ct_dir_proc,
                      param = cfg.ct_parameters)
            inv_to_process.append(CT)
        except AttributeError:
            pass
    elif cfg.target is tools.Target.COSMOART:
        try:
            MOZART = dict(fullname = 'MOZART',
                          nickname = 'mozart',
                          executable = 'mozart2int2lm',
                          indir = cfg.mozart_file_orig,
                          outdir = cfg.mozart_dir_proc,
                          param = [{'inc' : cfg.mozart_inc,
                                    'suffix' : cfg.mozart_prefix}])
            inv_to_process.append(MOZART)
        except AttributeError:
            pass
    else:
        # Unknown target
        raise RuntimeError("Unknown target: {}".format(cfg.target))

    # TO DO 
    #MOZART = dict(fullname="MOZART", nickname="mozart",executable="cams4int2cosmo")
        
    logging.info("Processing " + ", ".join([i["fullname"] for i in inv_to_process])+" data")

    scratch_path = os.path.join(cfg.int2lm_input,'icbc')
    tools.create_dir(scratch_path, "icbc input")

    for inv in inv_to_process:
        logging.info(inv["fullname"]+" files")
        #process_inv(starttime,hstart,hstop,increment,inv,cfg)
        
        for p in inv["param"]:
            inc = p["inc"]
            for time in tools.iter_hours(starttime, hstart, hstop, inc):
                logging.info(time)

                filename = os.path.join(inv["outdir"],p["suffix"]+"_"+time.strftime("%Y%m%d%H")+".nc")
                if not os.path.exists(filename):
                    logging.info(filename)
                    try:
                        to_call = getattr(tools, inv["executable"])
                        to_call.main(time,inv["indir"],inv["outdir"],p)
                    except:
                        logging.error("Preprocessing "+inv["fullname"] + " data failed")
                        raise

                # copy to (temporary) run input directory
                tools.copy_file(filename, scratch_path)

                logging.info("OK")

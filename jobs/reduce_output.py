#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import sys
import shutil
import datetime as dt
import glob
import netCDF4 as nc
import subprocess
import numpy as np
from datetime import datetime
import math

from . import tools


def main(startdate, enddate, cfg, model_cfg):
    """
    Calculates 2D column data and writes them into a new netCDF file.
    Only a fixed number of levels from **COSMO** output are considered.
    Those files are written into a new directory ``cosmo_output_reduced``.

    The number of levels is set by the configuration variable
    ``cfg.output_levels`` (default = all levels).
    
    **Important**: If several ``GRIBOUT`` sections are used to split the output
    data, then this code only works in case of the following:
    The tracers, for which the column-averaged dry-air (``X``) and
    moist-air (``Y``) mole fractions are calculated, have to be

        1. saved in a separate output file and
        2. the output file appears alphabetically **after** the meteorological
           variables.

    For example, use a GRIBOUT section suffix ``_met`` for standard **COSMO**
    output, and ``_trc`` for tracers.

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
    output_path = cfg.cosmo_output_reduced

    date = dt.datetime.today()

    to_print = """reduce_output

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" % date.strftime("%s")

    logging.info(to_print)

    tools.create_dir(output_path, "output")

    if cfg.compute_host != "daint":
        logging.error(
            "The reduce_output script is supposed to be run on daint only, "
            "not on %s" % cfg.compute_host)
        sys.exit(1)

    # Wait for Cosmo to finish first
    tools.check_job_completion(cfg.log_finished_dir, "cosmo")
    """Get list of constant files"""
    cfiles = []
    read_cfile = False
    for infile in sorted(
            glob.glob(os.path.join(cosmo_output, "lffd*[0-9]c*.nc"))):
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
        logging.info('Copied constant file %s to %s.' %
                     (cfiles[0], output_path))
    else:
        logging.error('Constant file could not be copied.')
    """Get list of files"""
    infiles = sorted(glob.glob(os.path.join(cosmo_output, "lffd*.nc")))
    mytimes = []
    for infile in list(infiles):
        basename = os.path.split(infile)[1]
        timestr = basename.split('lffd', 1)[1][:10]
        mytimes.append(datetime.strptime(timestr, '%Y%m%d%H'))

    str_startdate = mytimes[0].strftime('%Y-%m-%d %H')
    str_enddate = mytimes[-1].strftime('%Y-%m-%d %H')
    """Compute time step for parallel tasks"""
    ncores = 36
    total_time = mytimes[-1] - mytimes[0]
    nout_times = int(total_time.total_seconds() // 3600) + 1
    output_step = int(max(math.ceil(nout_times / ncores), 2))
    """Execute parallel bash script"""
    dir_path = os.path.dirname(os.path.realpath(__file__))
    tool_path = os.path.join(dir_path, 'tools')
    bash_file = os.path.join(tool_path, 'reduce_output_parallel.bash')
    py_file = os.path.join(tool_path, 'reduce_output_start_end.py')
    alternate_csv_file = os.path.join(cfg.chain_src_dir, 'cases', cfg.casename,
                                      'variables.csv')
    logfile = os.path.join(cfg.log_working_dir, 'reduce_output')
    logging.info('Submitting job to the queue...')

    result = subprocess.run([
        "sbatch", '--output=' + logfile, '--open-mode=append', '--wait',
        bash_file, py_file, cosmo_output, output_path, str_startdate,
        str_enddate,
        str(cfg.output_levels),
        str(output_step), alternate_csv_file,
        str(cfg.convert_gas)
    ])
    exitcode = result.returncode

    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
    """Check if all files have been processed"""
    cfile_names = []
    for cfile in cfiles:
        cfile_names.append(os.path.basename(cfile))
    files_cosmo = sorted([
        name for name in os.listdir(cosmo_output)
        if os.path.isfile(os.path.join(cosmo_output, name))
        and name not in cfile_names
    ])
    files_reduced = sorted([
        name for name in os.listdir(output_path)
        if os.path.isfile(os.path.join(output_path, name))
        and name not in cfile_names
    ])

    if not files_cosmo == files_reduced:
        logging.error(set(files_cosmo).symmetric_difference(files_reduced))
        logging.error('Reduced output files differ from original output!')
        raise RuntimeError('Error in reduce_output job')
    """Check for corrupted files"""
    for f in [
            os.path.join(output_path, name) for name in os.listdir(output_path)
    ]:
        with nc.Dataset(f) as _:
            logging.info('File %s is not corrupted' % f)

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
=====================================================""" % date.strftime("%s")

    logging.info(to_print)

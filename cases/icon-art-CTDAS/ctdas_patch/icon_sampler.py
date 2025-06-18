#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 15:07:13 2019

@author: friedemann

Modified on June 26, 10:40:12, 2023
Adaptation for sampling ICON instead of WRF.
@author: David Ho

"""

# Samples ICON-ART history files for CTDAS

# This is called as external executable from CTDAS
# to allow simple parallelization
#
# Usage:

# icon_sampler.py --arg1 val1 --arg2 val2 ...
# Arguments: See parser in code below

import os
import sys
#import itertools
#import bisect
import copy
import numpy as np
import xarray as xr
import netCDF4 as nc

# Import some CTDAS tools
pd = os.path.pardir
inc_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), pd, pd, pd)
inc_path = os.path.abspath(inc_path)
sys.path.append(inc_path)
from da.tools.icon.icon_helper import ICON_Helper
from da.tools.icon.utilities import utilities
import argparse

########## Parse options
parser = argparse.ArgumentParser()
parser.add_argument("--nproc",
                    type=int,
                    help="ID of this sampling process (0 ... nprocs-1)")
parser.add_argument("--nprocs", type=int, help="Number of sampling processes")
parser.add_argument("--sampling_coords_file", type=str,
                    help="File with sampling coordinates as created " + \
                         "by CTDAS column samples object")
parser.add_argument("--run_dir",
                    type=str,
                    help="Directory with icon output files")
parser.add_argument("--iconout_prefix",
                    type=str,
                    help="Headings of the ICON output files")
parser.add_argument("--icon_grid",
                    type=str,
                    help="Absolute path points to the ICON grid file")
parser.add_argument("--nmembers",
                    type=int,
                    help="Number of tracer ensemble members")
parser.add_argument("--tracer_optim", type=str,
                    help="Tracer that was optimized (e.g. CO2 for " + \
                         "ensemble members CO2_000 etc.)")
parser.add_argument("--outfile_prefix", type=str,
                    help="One process: output file. More processes: " + \
                         "output file is <outfile_prefix>.<nproc>.slice")
parser.add_argument("--footprint_samples_dim",
                    type=int,
                    help="Sample column footprint at n x n points")

args = parser.parse_args()
settings = copy.deepcopy(vars(args))

# Start (stupid) logging - should be updated
wd = os.getcwd()
try:
    os.makedirs("log")
except OSError:  # Case when directory already exists. Will look nicer in python 3...
    pass

logfile = os.path.join(
    wd, "log/iconout_sampler." + str(settings['nproc']) + ".log")

os.system("touch " + logfile)
os.system("rm " + logfile)
os.system("echo 'Process " + str(settings['nproc']) + " of " +
          str(settings['nprocs']) + ": start' >> " + logfile)
os.system("date >> " + logfile)

# David: could be helpful for validate arguments for icon sampling
########## Initialize iconhelper
iconhelper = ICON_Helper(settings)
iconhelper.validate_settings([
    'sampling_coords_file',
    'run_dir',
    'iconout_prefix',
    'icon_grid',
    'nproc',
    'nprocs',
    'nmembers',  # special case 0: sample 'tracer_optim'
    'tracer_optim',
    'outfile_prefix',
    'footprint_samples_dim'
])

cwd = os.getcwd()
os.chdir(iconhelper.settings['run_dir'])

# ########## Figure out which samples to process
# # Get number of samples
ncf = nc.Dataset(settings['sampling_coords_file'], "r")
nsamples = len(ncf.dimensions['soundings'])
ncf.close()

id0, id1 = utilities.get_slicing_ids(nsamples, settings['nproc'],
                                     settings['nprocs'])

os.system("echo 'id0=" + str(id0) + "' >> " + logfile)
os.system("echo 'id1=" + str(id1) + "' >> " + logfile)

# ########## Read samples from coord file
dat = iconhelper.read_sampling_coords(settings['sampling_coords_file'], id0,
                                      id1)

os.system("echo 'Data read, len=" + str(len(dat['sounding_id'])) + "' >> " +
          logfile)

########## Locate samples in ICON domains

# Take care of special case without ensemble
nmembers = settings['nmembers']

if nmembers == 0:
    # Special case: sample 'tracer_optim', don't add member suffix
    member_names = [settings['tracer_optim']]
    nmembers = 1
else:
    member_names = [
        settings['tracer_optim'] + "-%03d" % nm
        for nm in range(1, nmembers + 1)
    ]  # In ICON, ensemble member starts with XXX-001

#### Here gets the indexes of neighboring cells and the weights
#### Choose number of neighbours, recommend 4 as done in "cdo remapdis"

nneighb = 4

# Read grid file, and store the grid info. Only needs to do it once.
grid_file = settings['icon_grid']

# Import modules (takes 8 seconds)
from sklearn.neighbors import BallTree

# Get ICON grid specifics
ICON_GRID = xr.open_dataset(grid_file)
clon = ICON_GRID.clon.values
clat = ICON_GRID.clat.values

# Generate BallTree
test_points = np.column_stack([clat, clon])
tree = BallTree(test_points, metric='haversine')

lat_q = dat['latitude']
lon_q = dat['longitude']

# Query BallTree
(d, i) = tree.query(np.column_stack([np.deg2rad(lat_q),
                                     np.deg2rad(lon_q)]),
                    k=nneighb,
                    return_distance=True)

R = 6373.0  # approximate radius of earth in km

weight_list = 1. / (d * R)
nn_sel_list = i

######### Locate in time: Which file, time index, and temporal interpolation
# factor.
# MAYBE make this a function. See which quantities I need later.
# -- Initialize
id_t = np.zeros_like(dat['latitude'], int)
frac_t = np.ndarray(id_t.shape, float)
frac_t[:] = float("nan")

# Add a little flexibility by doing this per domain - namelists allow
# different output frequencies per domain.
iconout_files = dict()
iconout_times = dict()
iconout_start_time_ids = dict()

# -- Get full time vector
iconout_prefix = settings['iconout_prefix']
iconout_files = iconhelper.get_icon_filenames(iconout_prefix + "*")
iconout_times, iconout_start_time_ids = iconhelper.icon_times(iconout_files)

# time id
for idx in range(len(dat['latitude'])):
    # Look where it sorts in
    tmp = [i
          for i in range( len(iconout_times) -1 )
          if iconout_times[i] <= dat['time'][idx] \
          and dat['time'][idx] < iconout_times[i+1]]

    # Catch the case that the observation took place exactly at the last time step
    if len(tmp) == 1:
        id_t[idx] = tmp[0]
        time0 = iconout_times[id_t[idx]]
        time1 = iconout_times[id_t[idx] + 1]
        frac_t[idx] = (time1 - dat['time'][idx]).total_seconds() / (
            time1 - time0).total_seconds()

    else:  # len must be 0 in this case
        if len(tmp) > 1:\
            raise ValueError("wat")

        if dat['time'][idx] == iconout_times[-1]:
            # For debugging
            print('check dat[time]: %s' % (dat['time'][idx]))
            id_t[idx] = len(iconout_times) - 1
            frac_t[idx] = 1

        else:
            msg = "Sample %d, sounding_id %s: outside of simulated time." % (
                idx, dat['sounding_id'][idx])
            raise ValueError(msg)

# -- Create dictionary for column sampling:
loc_input = dict(nn_sel_list=nn_sel_list,
                 weight_list=weight_list,
                 id_t=id_t,
                 frac_t=frac_t,
                 files=iconout_files,
                 file_start_time_indices=iconout_start_time_ids,
                 times=dat['time'][:],
                 latitude=lat_q,
                 longitude=lon_q)

# -- Begin Sampling
ens_sim, prior = iconhelper.sample_total_columns_ICON(dat, loc_input,
                                                      member_names)

# -- Write results to file
obs_ids = dat['sounding_id']
# Remove simulations that are nan (=not in domain)
if ens_sim.shape[0] > 0:
    valid = np.apply_along_axis(lambda arr: not np.any(np.isnan(arr)), 1,
                                ens_sim)
    obs_ids_write = obs_ids[valid]
    ens_sim_write = ens_sim[valid, :]
    prior_sim_write = prior[valid, :]
else:
    obs_ids_write = obs_ids
    ens_sim_write = ens_sim
    prior_sim_write = prior
###
if settings['nprocs'] == 1:
    outfile = settings['outfile_prefix']
else:
    # Create output files with the appendix ".<nproc>.slice"
    # Format <nproc> so that they can later be easily sorted.
    len_nproc = int(np.floor(np.log10(settings['nprocs']))) + 1
    outfile = settings['outfile_prefix'] + (".%0" + str(len_nproc) +
                                            "d.slice") % settings['nproc']

os.system("echo 'Writing output file '" +
          os.path.join(iconhelper.settings['run_dir'], outfile) + " >> " +
          logfile)

### Write
iconhelper.write_simulated_columns(obs_id=obs_ids_write,
                                   simulated=ens_sim_write,
                                   nmembers=nmembers,
                                   outfile=outfile)

iconhelper.write_simulated_columns(obs_id=obs_ids_write,
                                   simulated=prior_sim_write,
                                   nmembers=1,
                                   outfile=outfile + '_prior.nc')

os.chdir(cwd)

os.system("echo 'Done' >> " + logfile)

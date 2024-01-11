#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
from os.path import dirname, realpath, basename
import shutil
from distutils.dir_util import copy_tree
import datetime as dt
import glob
import numpy as np
import xarray as xr
import pandas as pd
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
import multiprocessing
import subprocess
from matplotlib import gridspec
from PIL import Image

import matplotlib.pyplot as plt

plt.switch_backend('Agg')

try:
    from . import tools
except ImportError:
    import tools


BASIC_PYTHON_JOB = True


def pkl_path(folder, pid=None):
    """ Returns the path (and creates it, if necessary) to the stored
    pandas data file.

    Parameters
    ----------	
    folder : str
        Path to the chain_root or the output_root
    pid : int
        Process ID in case of parallel exectution

    Returns
    -------
    output_path : str
        Full file path to pickle file
    
    """
    if pid is None:
        # Main file with complete content
        filename = 'data.pkl'
    else:
        # Split file per process
        filename = 'data_' + str(pid) + '.pkl'

    if pid is None:
        output_folder = os.path.join(folder, 'check_output')
    else:
        output_folder = os.path.join(folder, 'check_output', 'data')
    tools.create_dir(output_folder, 'check_output')
    output_path = os.path.join(output_folder, filename)

    return output_path


def timeseries_path(cfg):
    """ Returns the path (and creates it, if necessary) to the timeseries
    plots.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes

    Returns
    -------
    output_path : str
        Full file path to timeseries directory
    """
    output_folder = os.path.join(cfg.output_root, 'check_output', 'timeseries')
    tools.create_dir(output_folder, 'timeseries')
    output_path = os.path.join(cfg.output_root, 'check_output', 'timeseries')

    return output_path


def maps_path(cfg):
    """ Returns the path (and creates it, if necessary) to the map plots.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes

    Returns
    -------
    output_folder : str
        Full file path to maps directory
    """
    output_folder = os.path.join(cfg.chain_root, 'check_output', 'maps')
    tools.create_dir(output_folder, 'check_output maps')

    return output_folder


def animations_path(cfg):
    """ Returns the path (and creates it, if necessary) to the animations.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes

    Returns
    -------
    output_folder : str
        Full file path to animations directory
    """
    output_folder = os.path.join(cfg.output_root, 'check_output', 'animations')
    tools.create_dir(output_folder, 'animations')
    output_path = os.path.join(cfg.output_root, 'check_output', 'animations')

    return output_path


def tracername2gas(tracername):
    """ Returns the chemical symbol from the COSMO tracer name to be 
    recognized by the helper convert_unit() function.

    Parameters
    ----------	
    tracername : str
        Full name of the tracer in COSMO output file

    Returns
    -------
    gas : str
        Chemical symbol as used in helper
    """
    gas = tracername.split('_')[0]
    if gas == 'NOX':
        gas = 'NO2'
    elif gas == 'C14':
        gas = '14CO2'

    return gas


def get_variable_names(cols):
    """ Get unique variable names from dataframe.
    
    Parameters
    ----------	
    cols : list of str
        List of column names from dataframe 

    Returns
    -------
    varnames : list of str
        List of unique variable names (e.g., CO2_A, NOX_BG, ...)
    
    """
    todels = ['max', 'min', 'ground', 'mean', 'std']
    varnames = []
    for col in cols:
        split_col = col.split('_')
        for todel in todels:
            split_col = list(filter(lambda x: x != todel, split_col))
        varnames.append('_'.join(split_col))
    varnames = list(dict.fromkeys(varnames))

    return varnames


def get_units(infiles, varnames):
    """ Get units of variables from netCDF files.

    Parameters
    ----------
    infiles : list of str
        List of netCDF files 
    varnames : list of str
        List of unique variable names (e.g., CO2_A, NOX_BG, ...)

    Returns
    -------
    units : dict
        Dictionary containing units of variable names
    
    """
    DS = xr.open_dataset(infiles[0])
    starttime = DS.time.values[-1].astype(dt.datetime)
    units = {}
    for infile in infiles:
        DS = xr.open_dataset(infile)
        time = DS.time.values[-1].astype(dt.datetime)
        if time > starttime:
            break
        for varname in varnames:
            try:
                if not varname in units:
                    units[varname] = DS[varname].units
            except KeyError:
                continue

    return units


def plot_timeseries(cfg, units):
    """ Plot of the min, max, mean and std values as time series. 
    
    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes
    units : dict
        Dictionary containing units os variables
    
    """
    data_path = pkl_path(cfg.output_root)
    df = pd.read_pickle(data_path)
    ts_path = timeseries_path(cfg)

    varnames = get_variable_names(df.columns.values)

    # Tweak figure properties
    style_label = 'seaborn-whitegrid'
    (fig_width, fig_height) = plt.rcParams['figure.figsize']
    fig_size = [fig_width * 2.2, fig_height * 1.8]

    for varname in varnames:
        logging.info('Plotting %s.png' % varname)

        # Get diagnostic values
        vmean = df[varname + '_mean']
        vstd = df[varname + '_std']
        vmin = df[varname + '_min']
        vmax = df[varname + '_max']

        # Unit conversion
        if (varname.startswith('CO2_') or varname.startswith('CO_')
                or varname.startswith('CH4_') or varname.startswith('C14_')
                or varname.startswith('NOX_') or varname.startswith('NO2_')):
            gas = tracername2gas(varname)
            in_unit = units[varname]
            out_unit = tools.helper.common_unit(gas)
            vmean = tools.helper.convert_unit(vmean, in_unit, out_unit, gas)
            vstd = tools.helper.convert_unit(vstd, in_unit, out_unit, gas)
            vmin = tools.helper.convert_unit(vmin, in_unit, out_unit, gas)
            vmax = tools.helper.convert_unit(vmax, in_unit, out_unit, gas)
        else:
            out_unit = units[varname]

        # Figure options
        fig, axes = plt.subplots(ncols=1,
                                 nrows=3,
                                 num=style_label,
                                 figsize=fig_size,
                                 squeeze=True)
        fig.suptitle(cfg.casename)

        axes[0].plot(df.index, vmean, linestyle='-')
        axes[0].set_title('Ground-level mean values')

        axes[1].plot(df.index, vmin, linestyle='-')
        axes[1].set_title('Global minimum values')

        axes[2].plot(df.index, vmax, linestyle='-')
        axes[2].set_title('Global maximum values')

        for ax in axes:
            ylabel = '%s (%s)' % (varname, out_unit)
            ax.set_ylabel(ylabel)
            ax.grid()

        axes[2].set_xlabel('Datetime')

        fig.tight_layout(rect=[0, 0, 1, 0.95])
        plt.savefig(os.path.join(ts_path, varname + '.png'))

        plt.close('all')


def get_data_single_file(infile, chain_src_dir, casename, chain_root):
    """Fetches the diagnostic data from COSMO output files.

    Parameters
    ----------
    infile : str
        Full file name to be read
    chain_src_dir : str
        Path to the directory where the processing chain is ran
    casename : str
        Name of the current case
    chain_root : str
        Path to the processing chain directory
    """
    logging.info(infile)

    DS = xr.open_dataset(infile)

    ground_level = DS.level.values[-1]
    nctime = DS.time.values[-1]

    # Read values from variables.csv
    alternate_csv_file = os.path.join(chain_src_dir, 'cases', casename,
                                      'variables.csv')
    variables = tools.helper.find_variables_file(alternate_csv_file)

    # Drop variables without min/max values
    variables['min_value'].replace('', np.nan, inplace=True)
    variables['max_value'].replace('', np.nan, inplace=True)
    variables['min_value'].replace(' ', np.nan, inplace=True)
    variables['max_value'].replace(' ', np.nan, inplace=True)
    variables.dropna(subset=['min_value'], inplace=True)
    variables.dropna(subset=['max_value'], inplace=True)

    # Get variable names
    varnames = variables.index.values

    data_values = dict()

    # Loop over variables
    for varname in varnames:
        try:
            da = DS[varname].sel(time=nctime).values
        except KeyError:
            #logging.warning('%s: Variable not in output file \n%s' %(varname,infile))
            continue
        if 'level' in DS[varname].dims:
            da_ground = DS[varname].sel(level=ground_level, time=nctime)\
                        .values.flatten()
        else:
            da_ground = da.flatten()

        da = da.flatten()
        minval = float(variables.loc[varname, 'min_value'])
        maxval = float(variables.loc[varname, 'max_value'])
        logging.info("{:10s} (min: {:11.4e}, max: {:11.4e})".format(
            varname, minval, maxval))

        varmin = np.nanmin(da)
        varmax = np.nanmax(da)
        varmin_ground = np.nanmin(da_ground)
        varmax_ground = np.nanmax(da_ground)
        varmean = np.mean(da_ground)
        varstd = np.std(da_ground)

        data_values[varname + '_min'] = varmin
        data_values[varname + '_max'] = varmax
        data_values[varname + '_min_ground'] = varmin_ground
        data_values[varname + '_max_ground'] = varmax_ground
        data_values[varname + '_mean'] = varmean
        data_values[varname + '_std'] = varstd

        if np.isnan(da).any():
            error_msg = ('Variable %s in file %s has NaNs!' %
                         (varname, infile))
            logging.error(error_msg)
        if (varmin < minval):
            error_msg = ('Variable %s in file %s falls below minimum value!\n'
                         'Allowed min = %e\n'
                         'Actual min = %e' % (varname, infile, minval, varmin))
            logging.error(error_msg)
        if (varmax > maxval):
            error_msg = ('Variable %s in file %s exceeds maximum value!\n'
                         'Allowed max = %e\n'
                         'Actual max = %e' % (varname, infile, maxval, varmax))
            logging.error(error_msg)

    # Get the time from the name
    time_str = os.path.basename(infile)[4:14]
    ctime = dt.datetime.strptime(time_str, '%Y%m%d%H')

    data = pd.DataFrame(data_values, index=[ctime])

    # Store the time series of min and max in a pandas file format.
    pid = os.getpid()
    logging.info('Storing data to %s' % pkl_path(chain_root, pid))
    store_data(data, chain_root, pid)


def merge_data(cfg):
    """ Merges multiple pickle files into one.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes

    Returns
    -------
    data : pandas.DataFrame
        Dataframe containing the whole diagnostic data 
    """
    output_path = pkl_path(cfg.output_root)
    input_path = os.path.join(cfg.chain_root, 'check_output', 'data')
    infiles = sorted(glob.glob(os.path.join(input_path, "data_*.pkl")))
    for infile in infiles:
        data = pd.read_pickle(infile)
        os.remove(infile)
        if os.path.exists(output_path):
            history = pd.read_pickle(output_path)
            history = history.combine_first(data)
            history.sort_index(inplace=True)
            history.to_pickle(output_path)
        else:
            data.sort_index(inplace=True)
            data.to_pickle(output_path)

    data = pd.read_pickle(output_path)

    return data


def store_data(data, folder, pid=None):
    """ Stores the time series diagnostic data in a pandas dataframe.
    Appends it to the existing file, without overwriting the previous dates. 
    This is useful if this is a continuous run with a spin-up period or 
    when using binary restart files.

    Parameters
    ----------	
    data: pandas.DataFrame
        Dataframe containing diagnostic values for each variable
    folder : str
        Path to the folder where the results are stored
    pid : int
        Process ID in case of parallel exectution
    """
    output_path = pkl_path(folder, pid)

    if os.path.exists(output_path):
        history = pd.read_pickle(output_path)
        history = history.combine_first(data)
        history.sort_index(inplace=True)
        history.to_pickle(output_path)
    else:
        data.sort_index(inplace=True)
        data.to_pickle(output_path)


def write_footnotetext(field):
    """ Formats the footnote text for map plots.
    
    Parameter
    ---------
    field : numpy.array
        Two-dimensional field of data to be shown in the plot

    Returns
    -------
    footnotetext : str
        Formatted footnote text with min/max/mean values of the field
    """
    fmin = np.min(field)
    fmax = np.max(field)
    fmean = np.mean(field)

    footnotetext = 'min: ' + "{:.1f}".format(fmin) + \
                   '  max: ' + "{:.1f}".format(fmax) + \
                   '  mean: ' + "{:.1f}".format(fmean)

    return footnotetext


def get_infiles(path):
    """ Returns a sorted file list of COSMO output files without *c.nc file. 

    Parameter
    ---------
    path : str
        Path to the COSMO output folder

    Returns
    -------
    infiles : list of str
        Sorted List of filenames of COSMO ouputs
    """
    infiles = sorted(glob.glob(os.path.join(path, "lffd*.nc")))
    infiles = [
        infile for infile in infiles
        if os.path.split(infile)[1].split('lffd', 1)[1][10] != 'c'
    ]

    # If this is a spin-up simulation, skip the first output files
    timestr = basename(dirname(dirname(realpath(path))))
    times = timestr.split("_")
    spinup_time = int(times[1])
    start = dt.datetime.strptime(times[0], '%Y%m%d%H')

    if spinup_time < 0:
        for f in infiles.copy():
            fname = basename(f)
            ftime = dt.datetime.strptime(fname[4:14], '%Y%m%d%H')
            if ftime < start:
                infiles.remove(f)

    return infiles


def plot_single_map(data, infile, output_path, varnames):
    """ Plots 2D maps of certain variables from a netCDF file.

    Parameters
    ----------	
    data: pandas.DataFrame
        Dataframe containing diagnostic values for each variable
    infile : str
        Full file name to be read
    output_path: str
        Full path where output files should be generated
    varnames : list of str
        Names of variables to be processed
    """
    # Read in files
    logging.info(infile)

    DS = xr.open_dataset(infile)

    ground_level = DS.level.values[-1]
    nctime = DS.time.values[-1]
    ts = nctime.astype(dt.datetime)
    ts = dt.datetime.utcfromtimestamp(ts / 1e9)
    timestr = ts.strftime("%Y%m%d%H")

    rlon = DS['rlon'].values
    rlat = DS['rlat'].values
    lon = DS['lon'].values
    lat = DS['lat'].values
    pollon = DS["rotated_pole"].attrs["grid_north_pole_longitude"]
    pollat = DS["rotated_pole"].attrs["grid_north_pole_latitude"]
    startlon = rlon[0]
    endlon = rlon[-1]
    startlat = rlat[0]
    endlat = rlat[-1]

    domain = tools.helper.Domain('COSMO',
                                 startlon,
                                 startlat,
                                 endlon,
                                 endlat,
                                 pollon=pollon,
                                 pollat=pollat)

    # Check for rotated pole coordinates
    if not isinstance(domain.proj, ccrs.RotatedPole):
        raise NotImplementedError('domain needs to use rotated pole coords')

    for varname in varnames:
        try:
            field = DS[varname].sel(time=nctime).values
        except KeyError:
            logging.warning('%s: Variable not in output file \n%s' %
                            (varname, infile))
            continue
        if len(np.shape(field)) == 3:
            field = DS[varname].sel(level=ground_level, time=nctime).values

        convert = False
        # Unit conversion
        if (varname.startswith('CO2_') or varname.startswith('CO_')
                or varname.startswith('CH4_') or varname.startswith('C14_')
                or varname.startswith('NOX_') or varname.startswith('NO2_')):
            convert = True
            gas = tracername2gas(varname)
            in_unit = DS[varname].units
            out_unit = tools.helper.common_unit(gas)
            field = tools.helper.convert_unit(field, in_unit, out_unit, gas)
        else:
            out_unit = DS[varname].units

        # Set min/max for colorbar and convert if needed
        vmin = data[varname + "_min_ground"].min()
        vmax = data[varname + "_max_ground"].max()
        if convert:
            vmin = tools.helper.convert_unit(vmin, in_unit, out_unit, gas)
            vmax = tools.helper.convert_unit(vmax, in_unit, out_unit, gas)

        # Set manual min/max range in case they are equal
        if vmax == vmin:
            vmax == vmin + 1

        # Create figure and axes objects
        fig = plt.figure()
        gs = gridspec.GridSpec(1, 2, width_ratios=[9, 1])
        ax = plt.subplot(gs[0], projection=domain.proj)
        cax = plt.subplot(gs[1])

        # Dimensions
        dlon = rlon[1] - rlon[0]
        left = rlon[0] - dlon / 2.0
        right = rlon[-1] + dlon / 2.0
        dlat = rlat[1] - rlat[0]
        bottom = rlat[0] - dlat / 2.0
        top = rlat[-1] + dlat / 2.0
        ax.set_xlim(domain.startlon, domain.stoplon)
        ax.set_ylim(domain.startlat, domain.stoplat)

        # Annotations
        ax.set_title(varname, loc='left', pad=10.0)
        ax.set_title(timestr, loc='right', pad=10.0)
        footnotetext = write_footnotetext(field)
        ax.annotate(footnotetext, (0, 0), (0, -4),
                    xycoords='axes fraction',
                    textcoords='offset points',
                    va='top')

        # Add coastlines
        ax.coastlines(resolution='10m', color="black", linewidth=1)
        lines = cfeature.NaturalEarthFeature(
            category='cultural',
            name='admin_0_boundary_lines_land',
            scale='10m',
        )
        ax.add_feature(lines,
                       edgecolor="grey",
                       facecolor='none',
                       linewidth=0.75)

        # Fill the plot
        im = ax.imshow(field,
                       vmin=vmin,
                       vmax=vmax,
                       cmap='viridis',
                       extent=(left, right, bottom, top),
                       interpolation=None,
                       transform=domain.proj)

        # Colorbar
        cax.cla()
        ticks = np.linspace(vmin, vmax, 8, endpoint=True)
        cb = plt.colorbar(im, cax=cax, ticks=ticks)
        cb.ax.set_yticklabels(["{:.1f}".format(i) for i in ticks])
        cb.ax.set_title(out_unit)

        # Save figure
        output_folder = os.path.join(output_path, varname)
        fig.savefig(os.path.join(output_folder,
                                 varname + '_' + timestr + '.png'),
                    bbox_inches='tight')
        plt.close('all')


def create_map_directories(cfg, data, units):
    """Create folders for the 2D maps of variables.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes
    data: pandas.DataFrame
        Dataframe containing diagnostic values for each variable
    units : dict
        Dictionary of units per variable name
    """
    output_path = maps_path(cfg)
    data_path = pkl_path(cfg.output_root)
    df = pd.read_pickle(data_path)

    # Get variable names
    varnames = get_variable_names(df.columns.values)
    # Create directories per variable for map plots
    for varname in varnames:
        output_folder = os.path.join(output_path, varname)
        tools.create_dir(output_folder, varname)


def create_animations(cfg):
    """Creates animated *.gif files from map plots.

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes
    """
    data_path = pkl_path(cfg.output_root)
    df = pd.read_pickle(data_path)

    # Get variable names
    varnames = get_variable_names(df.columns.values)

    map_path = os.path.join(cfg.output_root, 'check_output', 'maps')
    output_path = animations_path(cfg)
    for varname in varnames:
        infile_path = os.path.join(map_path, varname)
        infiles = sorted(glob.glob(os.path.join(infile_path, "*.png")))
        frames = []
        for i in infiles:
            new_frame = Image.open(i)
            frames.append(new_frame)

        # Save into a GIF file
        outfile = os.path.join(output_path, varname + '.gif')
        frames[0].save(outfile,
                       format='GIF',
                       append_images=frames[1:],
                       save_all=True,
                       duration=300)


def main(cfg):
    """Check output variables for physical reasonability and create plots.

    This function checks the output variables to ensure they are in a physically
    reasonable range. It stores the time series of the minimum, maximum, mean, and
    standard deviation of the variables as a pandas object into a pickle file.

    It also creates per-variable plots from the stored time series data.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)
    launch_time = cfg.init_time_logging("check_output")
    date = dt.datetime.today()

    to_print = """check_output

=====================================================
============== POST PROCESSING BEGINS ===============
============== StartTime: %s 
=====================================================""" % date.strftime("%s")

    logging.info(to_print)

    # if cfg.compute_host!="daint":
    #     logging.error("The check_output script is supposed to be run on daint only, "
    #                   "not on %s" % cfg.compute_host)
    #     sys.exit(1)
    """Wait for Cosmo to finish first"""
    tools.check_job_completion(cfg.log_finished_dir, "cosmo",
                               waittime=300)  # check every 30 seconds

    # Get list of files
    logging.info('Getting list of input files')
    infiles = get_infiles(cfg.cosmo_output)

    # Get data from COSMO output files
    logging.info('Getting data from COSMO output files')
    to_write = """#!/usr/bin/env bash
#SBATCH --partition=debug
#SBATCH --account=em05
#SBATCH --job-name="check_output_{cfg.startdate_sim_yyyymmddhh}_{cfg.forecasttime}"
#SBATCH --open-mode=append
#SBATCH --time=00:30:00
#SBATCH --constraint=mc
#SBATCH --ntasks=1
#SBATCH --output={cfg.logfile}


export EASYBUILD_PREFIX=/store/empa/em05/easybuild
export ECCODES_DEFINITION_PATH=/store/empa/em05/easybuild/software/ecCodes/2.12.5-CrayGNU-19.10/share/eccodes/definitions/

module load daint-mc
module load EasyBuild-custom

module load daint-mc
module load EasyBuild-custom

module load cray-python/3.8.5.0
module load PyExtensions/python3-CrayGNU-20.11

module load GEOS
module load PROJ/4.9.3-CrayGNU-20.11
module load ecCodes
#module load GDAL                         # FIXME: currently easybuild install is not working

# source /store/empa/em05/pyvenv-3.8/bin/activate
source ~/python/stem2/bin/activate

srun python jobs/check_output.py {casename} {cosmo_output} {output_root} {chain} {chain_root} {action}
"""
    to_write_fmt = to_write.format(cfg=cfg,
                                   casename=cfg.casename,
                                   cosmo_output=cfg.cosmo_output,
                                   output_root=cfg.output_root,
                                   work_log=cfg.log_working_dir,
                                   logfile=cfg.logfile,
                                   chain=cfg.chain_src_dir,
                                   chain_root=cfg.chain_root,
                                   action='get_data')

    output_file = os.path.join(cfg.cosmo_work, "get_data.job")

    with open(output_file, 'w') as outf:
        outf.write(to_write_fmt)

    result = subprocess.run(["sbatch", "--wait", output_file])
    exitcode = result.returncode

    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

    # Merge data to single pickle file
    logging.info('Merging data to single pickle file')
    data = merge_data(cfg)

    # Get variable names
    logging.info('Getting variable names')
    varnames = get_variable_names(data.columns.values)
    logging.info(varnames)

    # Get units
    logging.info('Reading units')
    units = get_units(infiles, varnames)
    logging.info(units)

    # Plots
    logging.info('Creating timeseries plots in %s' % timeseries_path(cfg))
    plot_timeseries(cfg, units)

    logging.info('Creating map plots in %s' % maps_path(cfg))
    create_map_directories(cfg, data, units)

    to_write_fmt = to_write.format(cfg=cfg,
                                   casename=cfg.casename,
                                   cosmo_output=cfg.cosmo_output,
                                   output_root=cfg.output_root,
                                   logfile=cfg.logfile,
                                   chain=cfg.chain_src_dir,
                                   chain_root=cfg.chain_root,
                                   action='plot_maps')

    output_file = os.path.join(cfg.cosmo_work, "plot_maps.job")

    with open(output_file, 'w') as outf:
        outf.write(to_write_fmt)

    result = subprocess.run(["sbatch", "--wait", output_file])
    exitcode = result.returncode

    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))

    # Move map plots to output directory
    src_folder = os.path.join(cfg.chain_root, 'check_output', 'maps')
    dest_folder = os.path.join(cfg.output_root, 'check_output')
    logging.info('Move map plots to %s' % dest_folder)
    try:
        shutil.move(src_folder, dest_folder)
    except:
        for varname in varnames:
            fromDirectory = os.path.join(src_folder, varname)
            toDirectory = os.path.join(dest_folder, 'maps', varname)
            copy_tree(fromDirectory, toDirectory)

    # Animations
    logging.info('Creating animations in %s' % animations_path(cfg))
    create_animations(cfg)

    date = dt.datetime.today()
    to_print = """=====================================================
============== POST PROCESSING ENDS ==================
============== EndTime: %s
=====================================================""" % date.strftime("%s")
    logging.info(to_print)

    # Check for errors
    with open(cfg.logfile) as f:
        if 'ERROR' in f.read():
            raise RuntimeError('Logfile containing errors! See %s' %
                               cfg.logfile)

    cfg.finish_time_logging("check_output", launch_time)


if __name__ == '__main__':
    args = sys.argv

    casename = args[1]
    cosmo_output = args[2]
    output_root = args[3]
    chain_src_dir = args[4]
    chain_root = args[5]
    action = args[6]

    output_folder = os.path.join(chain_root, 'check_output')
    data_path = os.path.join(output_root, 'check_output', 'data.pkl')
    infiles = get_infiles(cosmo_output)

    nthread = 36

    if action == 'plot_maps':
        with multiprocessing.Pool(nthread) as pool:
            data = pd.read_pickle(data_path)
            # Get variable names
            output_path = os.path.join(output_folder, 'maps')
            varnames = get_variable_names(data.columns.values)
            pool.starmap(plot_single_map,
                         [(data, infile, output_path, varnames)
                          for infile in infiles])

    elif action == 'get_data':
        # Loop over all files and check values
        with multiprocessing.Pool(nthread) as pool:
            pool.starmap(get_data_single_file,
                         [(infile, chain_src_dir, casename, chain_root)
                          for infile in infiles])

#!/usr/bin/env python3

import numpy as np
import glob
import os
from netCDF4 import Dataset, num2date
from os.path import join
import multiprocessing
from .nc_operations import VariableCreator, VariableCopier, DimensionCopier
from datetime import datetime, timedelta, timezone

##########################################################################################
## - Vertical interpolation of chemistry fields (CAM-Chem / CAMS) that are ALREADY
##   remapped horizontally onto the ICON grid (unstructured, dimension 'ncells').
## - Extraction of one NetCDF file per time step.
## - Interpolation of chemistry fields with respect to time variable.
##
## Based on 'mozart2int2m.py', with additions by Louise Aubet and Corina Keller (Empa).
## Adapted for chemistry input on the ICON grid (clat/clon, ncells).
##########################################################################################

# -- Name of the horizontal (unstructured) dimension and coordinates
HDIM = 'ncells'
HCOORDS = ['clat', 'clon']

# -- Physical constants
G = 9.80665  # m s-2
RD = 287.05  # J kg-1 K-1


def _timestamps(time_var):
    """Convert a CF time variable to a list of python datetimes (UTC)."""
    dates = num2date(time_var[:],
                     units=time_var.units,
                     calendar=getattr(time_var, 'calendar', 'standard'),
                     only_use_cftime_datetimes=False,
                     only_use_python_datetimes=True)
    return [
        datetime(d.year,
                 d.month,
                 d.day,
                 d.hour,
                 d.minute,
                 d.second,
                 tzinfo=timezone.utc) for d in np.atleast_1d(dates)
    ]


def time_intpl(file_path, prefix):
    """
    Interpolate chemistry fields with respect to time between consecutive NetCDF files.

    This function generates new NetCDF files at intermediate times between existing
    output files. Takes the arithmetic mean of chemical and surface pressure
    fields and creates interpolated 'time' and 'datesec' variables.

    Parameters
    ----------
    file_path : str
        Path to the directory containing the input NetCDF files.
    prefix : str
        Filename prefix for identifying the input NetCDF files to interpolate.

    Returns
    -------
    None

    """

    # -- Only files named <prefix>YYYYMMDDHH.nc
    files = sorted([
        f for f in os.listdir(file_path) if f.startswith(prefix)
        and f.endswith('.nc') and len(f) == len(prefix) + len('YYYYMMDDHH.nc')
    ])

    for i in range(len(files) - 1):

        in_filename1 = join(file_path, files[i])
        in_filename2 = join(file_path, files[i + 1])

        # -- Date string follows the prefix (prefix itself may contain '_')
        date_str1 = files[i][len(prefix):].split('.')[0]
        date_str2 = files[i + 1][len(prefix):].split('.')[0]

        # Time difference between two files
        date1 = datetime.strptime(date_str1, '%Y%m%d%H')
        date2 = datetime.strptime(date_str2, '%Y%m%d%H')
        time_difference = (date2 - date1).total_seconds() / 3600.0

        # Date of the new file
        delta_t = time_difference / 2.
        interp_date = date1 + timedelta(hours=delta_t)

        outname_template = join(file_path, prefix + '%Y%m%d%H.nc')
        out_name = interp_date.strftime(outname_template)

        with Dataset(in_filename1, 'r') as ds1, Dataset(in_filename2,
                                                        'r') as ds2:
            if os.path.exists(out_name):
                os.remove(out_name)

            with Dataset(out_name, 'w') as out_ds:
                # -- Copy global attributes
                out_ds.setncatts(ds1.__dict__)

                # -- Copy dimensions and variables
                dimpairs = [(dim_name, dim_name)
                            for dim_name, dim in ds1.dimensions.items()]
                dim_copier = [
                    DimensionCopier(src_name, dst_name)
                    for src_name, dst_name in dimpairs
                ]
                var_to_copy = [
                    v for v in [
                        'lev_s', 'lev_m', *HCOORDS, 'hyam', 'hybm', 'hyai',
                        'hybi', 'date'
                    ] if v in ds1.variables
                ]
                dict_var = [{
                    'src_names': var,
                    'dst_name': var
                } for var in var_to_copy]
                var_copier = [VariableCopier(**kwargs) for kwargs in dict_var]
                for op in dim_copier + var_copier:
                    op.apply_to(ds1, out_ds)

                # -- Create time and date variables
                (VariableCreator(
                    var_args={
                        'varname': 'time',
                        'datatype': 'f8',
                        'dimensions': ('time', )
                    },
                    var_attrs={
                        'axis':
                        'T',
                        'calendar':
                        'proleptic_gregorian',
                        'long_name':
                        'simulation_time',
                        'standard_name':
                        'time',
                        'units':
                        interp_date.strftime('hours since %Y-%m-%d %H:%M:%S')
                    },
                    var_vals=0).apply_to(out_ds))

                datesec = ds1['datesec'][:] + delta_t * 3600
                (VariableCreator(var_args={
                    'varname': 'datesec',
                    'datatype': 'f8',
                    'dimensions': ('time', )
                },
                                 var_attrs={
                                     'long_name':
                                     'current seconds of current date',
                                     'units': 's'
                                 },
                                 var_vals=datesec).apply_to(out_ds))

                # -- Interpolate surface pressure and chemical fields
                # -- (dimensions are taken from the input, e.g. (time, ncells)
                # -- for PS and (time, lev_m, ncells) for the tracers)
                var_to_copy.extend(('time', 'datesec'))
                for var_name, var in ds1.variables.items():
                    if var_name not in var_to_copy:
                        mean_field = np.mean(
                            [ds1[var_name][:], ds2[var_name][:]], axis=0)
                        (VariableCreator(var_args={
                            'varname': var_name,
                            'datatype': 'f8',
                            'dimensions': var.dimensions
                        },
                                         var_attrs=var.__dict__,
                                         var_vals=mean_field).apply_to(out_ds))


##################################


def date_from_days_since_ref(days_since_ref, ref_date):
    """Convert a float counting days since a reference date to a datetime object.

    Parameters:
    - days_since_ref: float, the number of days since the reference date.
    - ref_date: str, the reference date in the format 'yyyy-mm-dd hh:mm:ss'.

    Returns:
    - datetime object corresponding to the given number of days since the reference date.
    """
    timestamp_date = datetime.strptime(ref_date, '%Y-%m-%d %H:%M:%S').date()
    timestamp_time = timedelta(days=days_since_ref)

    return datetime.combine(timestamp_date,
                            datetime.min.time()) + timestamp_time


def date_from_hours_since_ref(hours_since_ref, ref_date):
    """Convert a float counting hours since a reference date to a datetime object.

    Parameters:
    - hours_since_ref: float, the number of hours since the reference date.
    - ref_date: str, the reference date in the format 'yyyy-mm-dd hh:mm:ss'.

    Returns:
    - datetime object corresponding to the given number of hours since the reference date.
    """
    timestamp_date = datetime.strptime(ref_date, '%Y-%m-%d %H:%M:%S').date()
    timestamp_time = timedelta(hours=hours_since_ref)

    return datetime.combine(timestamp_date,
                            datetime.min.time()) + timestamp_time


##################################


def extract_timeslice(in_file, out_file_template, spec_intpl, ref_date=None):
    """
    Extract a single time slice from a NetCDF file (output of vert_intpl).

    Copies selected species and metadata from an input NetCDF file into a new
    NetCDF file for each time step. Fields are on the ICON grid (ncells), so no
    longitude transformation is applied.

    Parameters
    ----------
    in_file : str
        Path to the input NetCDF file.
    out_file_template : str
        Template string for the output filename.
    spec_intpl : list of str
        List of chemical species to extract from the input file.
    ref_date : str, optional
        Not used anymore; the time is read from the CF 'units' attribute.

    Returns
    -------
    None
    """

    # -- Create list of DimensionCopier objects for copying dimensions.
    dimpairs = [(HDIM, HDIM), ('lev_m', 'lev_m'), ('lev_s', 'lev_s'),
                ('nhym', 'nhym'), ('nhyi', 'nhyi')]

    dim_copier = [
        DimensionCopier(src_name, dst_name) for src_name, dst_name in dimpairs
    ]

    # -- Create list of VariableCopier objects for copying variables for each time step.
    varpairs_to_copy = [(spc, spc) for spc in spec_intpl]

    with Dataset(in_file) as ds:
        timestamps = _timestamps(ds['time'])

    for time_index, timestamp in enumerate(timestamps):

        # -- Specify the slices of the variable values to be copied.
        sliced_variable_options = {
            'var_args': {
                'dimensions': ('time', 'lev_m', HDIM)
            },
            'var_val_indices': np.s_[time_index, :]
        }

        var_opts = [{
            'src_names': src,
            'dst_name': dst,
            **sliced_variable_options
        } for src, dst in varpairs_to_copy]

        var_opts += [{'src_names': c, 'dst_name': c} for c in HCOORDS]

        var_opts += [{
            'src_names': 'PS',
            'dst_name': 'PS',
            'var_args': {
                'dimensions': ('time', HDIM)
            },
            'var_val_indices': np.s_[time_index, :]
        }, {
            'src_names': 'hyam',
            'dst_name': 'hyam',
            'var_args': {
                'dimensions': ('nhym', )
            }
        }, {
            'src_names': 'hybm',
            'dst_name': 'hybm',
            'var_args': {
                'dimensions': ('nhym', )
            }
        }, {
            'src_names': 'lev_m',
            'dst_name': 'lev_m'
        }, {
            'src_names': 'lev_s',
            'dst_name': 'lev_s'
        }, {
            'src_names': 'hyai',
            'dst_name': 'hyai',
            'var_args': {
                'dimensions': ('nhyi', )
            }
        }, {
            'src_names': 'hybi',
            'dst_name': 'hybi',
            'var_args': {
                'dimensions': ('nhyi', )
            }
        }]

        var_copier = [VariableCopier(**kwargs) for kwargs in var_opts]

        # -- Extract data for given time step.
        with Dataset(in_file) as ds:
            timestamp = timestamp.replace(tzinfo=None)
            out_path = timestamp.strftime(out_file_template)

            with Dataset(out_path, 'w') as out_ds:
                # -- Copy global attributes
                out_ds.setncatts(ds.__dict__)

                # -- Create new time dimension.
                out_ds.createDimension(dimname='time', size=1)

                # -- Create time variable.
                (VariableCreator(
                    var_args={
                        'varname': 'time',
                        'datatype': 'f8',
                        'dimensions': ('time', )
                    },
                    var_attrs={
                        'axis':
                        'T',
                        'calendar':
                        'proleptic_gregorian',
                        'long_name':
                        'simulation_time',
                        'standard_name':
                        'time',
                        'units':
                        timestamp.strftime('hours since %Y-%m-%d %H:%M:%S')
                    },
                    var_vals=0).apply_to(out_ds))

                # -- Create date variable.
                (VariableCreator(
                    var_args={
                        'varname': 'date',
                        'datatype': 'i4',
                        'dimensions': ('time', )
                    },
                    var_attrs={
                        'long_name': ('current date as 8 digit'
                                      ' integer (YYYYMMDD)')
                    },
                    var_vals=int(
                        timestamp.strftime('%Y%m%d'))).apply_to(out_ds))

                # -- Create datesec variable.
                sec_from_midnight = ((timestamp - timestamp.replace(
                    hour=0, minute=0, second=0,
                    microsecond=0)).total_seconds())
                (VariableCreator(var_args={
                    'varname': 'datesec',
                    'datatype': 'i4',
                    'dimensions': ('time', )
                },
                                 var_attrs={
                                     'long_name':
                                     'seconds to complete current date',
                                     'units': 's'
                                 },
                                 var_vals=sec_from_midnight).apply_to(out_ds))

                # -- Write specified dimensions and variables to output.
                for op in dim_copier + var_copier:
                    op.apply_to(ds, out_ds)


##################################


def log_intpl_timeslice(pres_m, pres, var_data):
    """
    Perform vertical interpolation of chemical data using log-pressure weighting.

    For each chemistry level, the two nearest meteorological levels are identified
    and a logarithmic interpolation is applied. Works for any horizontal layout,
    i.e. arrays of shape (lev, lat, lon) or (lev, ncells); level index 0 is the
    model top.

    Parameters
    ----------
    pres_m : ndarray
        Pressure on meteorological levels (Pa).
    pres : ndarray
        Pressure on chemical levels (Pa).
    var_data : ndarray
        Chemical field data to interpolate.

    Returns
    -------
    ndarray
        Interpolated chemical field on meteorological levels.
    """
    pres_m_flipped = np.flip(pres_m, axis=0)
    pres_flipped = np.flip(pres, axis=0)
    var_data_flipped = np.flip(var_data, axis=0)

    # -- Find meteo levels lying above given chem level
    # -- (that is, meteo levels with lower pressure than given chem level)
    above_target = pres_flipped[:, np.newaxis] <= pres_m_flipped

    # -- Calculate number of levels below given chem level
    # -- (that is, number of meteo levels with higher pressire than given chem level)
    lev_count = (pres_flipped[:, np.newaxis] >= pres_m_flipped).sum(axis=0)

    # -- Find indices of first meteo levels lying above given chem level
    # -- The index is set to -1 if no meteo levels are found
    first_above_target = np.where(lev_count == len(pres_flipped), -1,
                                  np.argmax(above_target, axis=0))

    # -- Select indices of 2 closest neighbouring levels for given chem level
    vertical_indices = np.stack([first_above_target, first_above_target - 1],
                                axis=0)

    # --  Set both indices to highest/lower chem level if given chem level is above/below all meteo levels
    vertical_indices[:, first_above_target == 0] = 0
    vertical_indices[:, first_above_target == -1] = len(pres_flipped) - 1

    # -- Select pressure values of 2 closest neighbouring levels
    pk_below = np.take_along_axis(
        (pres_flipped), vertical_indices[1],
        axis=0)  # this is the pressure of the lower level (higher pressure)
    pk_up = np.take_along_axis(
        (pres_flipped), vertical_indices[0],
        axis=0)  # this is the pressure of the upper level (lower pressure)

    # -- Compute interpolation weights
    with np.errstate(divide='ignore', invalid='ignore'):
        alpha = (np.log(pres_m_flipped / pk_up)) / (np.log(pk_below / pk_up))
    weights = np.stack([alpha, 1 - alpha], axis=0)

    # -- Weights for chem levels lying above/below all meteo levels
    weights[:, first_above_target == 0] = 0.5
    weights[:, first_above_target == -1] = 0.5

    # Interpolate
    var_data_intpl_flipped = np.take_along_axis(var_data_flipped, vertical_indices[1], axis=0) * weights[0] + \
                np.take_along_axis(var_data_flipped, vertical_indices[0], axis=0) * weights[1]

    var_data_intpl = np.flip(var_data_intpl_flipped, axis=0)
    return var_data_intpl


def hybrid_pressure_interpolation(in_ds, out_ds, var_name, time_indices):
    """
    Interpolate chemical variable vertically using log-pressure interpolation.

    Parameters
    ----------
    in_ds : netCDF4.Dataset
        Input dataset containing the chemical variable.
    out_ds : netCDF4.Dataset
        Output dataset with meteorological levels.
    var_name : str
        Name of the chemical variable to interpolate.
    time_indices : list of int
        Time indices to process in the datasets.

    Returns
    -------
    ndarray
        Array of vertically interpolated variable on meteorological levels,
        shape (time, lev_m, ncells).
    """
    # Pressure on vertical levels of meteo data
    pres_m = out_ds['pres_m'][:]
    # Pressure on vertical levels of chemistry data
    pres = out_ds['pres'][:]
    var_data = in_ds[var_name][np.s_[time_indices, :]]

    var_arr = np.zeros(pres_m.shape)

    # -- log_intpl_timeslice expects level index 0 = model top (lowest
    # -- pressure). CAMS/TM5 levels are ordered surface -> top, so flip the
    # -- chemistry data if needed. Meteo levels are flipped for the
    # -- interpolation only; the output keeps the order of the meteo file.
    chem_surface_first = np.mean(pres[:, 0]) > np.mean(pres[:, -1])
    meteo_surface_first = np.mean(pres_m[:, 0]) > np.mean(pres_m[:, -1])

    for dt in range(pres_m.shape[0]):
        p_c, v_c, p_m = pres[dt], var_data[dt], pres_m[dt]
        if chem_surface_first:
            p_c, v_c = np.flip(p_c, axis=0), np.flip(v_c, axis=0)
        if meteo_surface_first:
            p_m = np.flip(p_m, axis=0)
        intpl = log_intpl_timeslice(p_m, p_c, v_c)
        var_arr[dt] = np.flip(intpl, axis=0) if meteo_surface_first else intpl

    return var_arr


def chem_pressure_from_altitude(c_ds, time_indices):
    """
    Reconstruct the pressure on the chemistry full levels hydrostatically.

    Fallback only: used if the remapped CAMS/TM5 file does not contain the
    hybrid coefficients (hyam, hybm). Such a file still provides
    surface pressure 'ps', temperature 'T', specific humidity 'Q'
    (on full levels) and the geometric 'altitude' of the half levels. Starting
    from ps at the lowest half level, the hypsometric equation is integrated
    upwards:

        p_half(k) = p_half(k+1) * exp(-g * (z(k) - z(k+1)) / (Rd * Tv(k)))

    Full-level pressure is the mean of the bounding half levels (TM5 convention,
    equivalent to ap_full = mean(ap_half), b_full = mean(b_half)).

    Returns
    -------
    ndarray
        Pressure on chemistry full levels, shape (time, level, ncells), with the
        same vertical ordering as the chemistry fields in the file.
    """
    ps = np.ma.filled(c_ds['ps'][time_indices, :].astype('f8'), np.nan)
    temp = np.ma.filled(c_ds['T'][time_indices, :].astype('f8'), np.nan)
    qv = np.ma.filled(c_ds['Q'][time_indices, :].astype('f8'), 0.)
    z = np.ma.filled(c_ds['altitude'][time_indices, :].astype('f8'), np.inf)

    # -- Ensure ordering top -> surface (index 0 = model top)
    top_first = np.nanmedian(z[:, 0, :]) > np.nanmedian(z[:, -1, :])
    if not top_first:
        temp, qv, z = (np.flip(a, axis=1) for a in (temp, qv, z))

    tv = temp * (1. + 0.608 * qv)
    nlev = temp.shape[1]

    p_half = np.empty_like(z)
    p_half[:, -1, :] = ps
    with np.errstate(over='ignore', invalid='ignore'):
        for k in range(nlev - 1, -1, -1):
            dz = z[:, k, :] - z[:, k + 1, :]
            p_half[:, k, :] = p_half[:, k + 1, :] * np.exp(-G * dz /
                                                           (RD * tv[:, k, :]))
    # Model top (z = inf or missing) -> p = 0
    p_half = np.where(np.isfinite(p_half), p_half, 0.)

    p_full = 0.5 * (p_half[:, :-1, :] + p_half[:, 1:, :])

    if not top_first:
        p_full = np.flip(p_full, axis=1)

    return p_full


def vert_intpl(chem_filename,
               meteo_filename,
               out_filename,
               spec,
               start_chunk,
               end_chunk,
               ref_date=None):
    """
    Perform vertical interpolation of chemical fields using meteorological data.

    This function interpolates chemical species, which are already remapped onto
    the ICON grid (dimension 'ncells'), from their native vertical levels onto
    the hybrid sigma-pressure levels of the meteorological dataset.

    Parameters
    ----------
    chem_filename : str
        Path to the NetCDF file containing chemical data (on ICON grid).
    meteo_filename : str
        Path to the NetCDF file containing meteorological data.
    out_filename : str
        Path for the output NetCDF file with interpolated results.
    spec : list of str
        List of chemical species to interpolate.
    start_chunk : datetime.datetime
        Start of the simulation time window to process (tz-aware, UTC).
    end_chunk : datetime.datetime
        End of the simulation time window to process (tz-aware, UTC).
    ref_date : str, optional
        Not used anymore; the time is read from the CF 'units' attribute.

    Returns
    -------
    None

    """

    with Dataset(chem_filename, 'r') as c_ds, Dataset(meteo_filename,
                                                      'r') as m_ds:

        # -- Horizontal dimension of the chemistry file (e.g. 'ncells' or 'cell')
        ps_dims = c_ds['ps'].dimensions
        if len(ps_dims) != 2:
            raise ValueError(
                f"{chem_filename}: 'ps' has dimensions {ps_dims}, expected "
                "(time, <cells>). Is this file already remapped onto the "
                "ICON grid?")
        hdim_src = ps_dims[-1]

        # -- Extract indices for times between simulation start and end
        # -- (plus the neighbouring step needed for time interpolation)
        timestamps = _timestamps(c_ds['time'])
        time_index_lst = []

        for time_index, timestamp_now in enumerate(timestamps):

            if start_chunk <= timestamp_now <= end_chunk:
                if time_index not in time_index_lst:
                    time_index_lst.append(time_index)

            if time_index != len(timestamps) - 1:
                timestamp_next = timestamps[time_index + 1]

                # Date between two time steps
                timestamp_interp = timestamp_now + (timestamp_next -
                                                    timestamp_now) / 2

                if start_chunk <= timestamp_interp <= end_chunk:
                    if time_index not in time_index_lst:
                        time_index_lst.append(time_index)
                    if time_index + 1 not in time_index_lst:
                        time_index_lst.append(time_index + 1)

        time_index_lst.sort()
        sel_timestamps = [timestamps[i] for i in time_index_lst]
        ntime = len(time_index_lst)

        with Dataset(out_filename, 'w') as out_ds:
            # -- Set global attributes
            out_ds.setncatts(c_ds.__dict__)

            # -- Create new dimensions
            out_ds.createDimension("lev_m", len(m_ds['lev'][:]))
            out_ds.createDimension("lev_s", 1)
            out_ds.createDimension("lev", c_ds.dimensions['level'].size)
            out_ds.createDimension('time', size=ntime)

            # -- Copy dimensions (horizontal dim of the input is renamed to HDIM)
            dimpairs_c = [(hdim_src, HDIM)]
            dim_copier_c = [
                DimensionCopier(src_name, dst_name)
                for src_name, dst_name in dimpairs_c
            ]
            dimpairs_m = [('nhym', 'nhym'), ('nhyi', 'nhyi')]
            dim_copier_m = [
                DimensionCopier(src_name, dst_name)
                for src_name, dst_name in dimpairs_m
            ]

            # -- Copy variables (ICON grid coordinates, surface pressure, time)
            dict_c = [{
                'src_names': var,
                'dst_name': var,
                'var_args': {
                    'dimensions': (HDIM, )
                }
            } for var in HCOORDS]

            dict_c += [{
                'src_names': 'ps',
                'dst_name': 'PS',
                'var_args': {
                    'dimensions': ('time', HDIM)
                },
                'var_val_indices': np.s_[time_index_lst, :]
            }, {
                'src_names': 'time',
                'dst_name': 'time',
                'var_args': {
                    'dimensions': ('time', )
                },
                'var_val_indices': np.s_[time_index_lst]
            }]

            var_copier_c = [VariableCopier(**kwargs) for kwargs in dict_c]
            for op in dim_copier_c + var_copier_c:
                op.apply_to(c_ds, out_ds)

            # -- date / datesec are not in the CAMS file -> create them
            (VariableCreator(
                var_args={
                    'varname': 'date',
                    'datatype': 'i4',
                    'dimensions': ('time', )
                },
                var_attrs={
                    'long_name': 'current date as 8 digit integer (YYYYMMDD)'
                },
                var_vals=[int(t.strftime('%Y%m%d'))
                          for t in sel_timestamps]).apply_to(out_ds))
            (VariableCreator(var_args={
                'varname': 'datesec',
                'datatype': 'i4',
                'dimensions': ('time', )
            },
                             var_attrs={
                                 'long_name':
                                 'seconds to complete current date',
                                 'units': 's'
                             },
                             var_vals=[
                                 t.hour * 3600 + t.minute * 60 + t.second
                                 for t in sel_timestamps
                             ]).apply_to(out_ds))

            copy_from_meteo = ['hyam', 'hybm', 'hyai', 'hybi']
            dict_m = [{
                'src_names': var,
                'dst_name': var
            } for var in copy_from_meteo]

            var_copier_m = [VariableCopier(**kwargs) for kwargs in dict_m]

            for op in dim_copier_m + var_copier_m:
                op.apply_to(m_ds, out_ds)

            # -- Create vertical levels and pressure arrays for vertial interpolation

            # Vertical levels from meteo data
            hybrid_levels = m_ds['hyam'][:] * 1e-2 + m_ds['hybm'][:] * 1e3
            (VariableCreator(var_args={
                'varname': 'lev_m',
                'datatype': 'f8',
                'dimensions': ('lev_m', )
            },
                             var_attrs={
                                 'long_name': 'hybrid sigma levels',
                                 'units': '1'
                             },
                             var_vals=hybrid_levels).apply_to(out_ds))

            # Surface level
            (VariableCreator(var_args={
                'varname': 'lev_s',
                'datatype': 'f8',
                'dimensions': ('lev_s', )
            },
                             var_attrs={
                                 'long_name': 'surface level',
                                 'units': '1'
                             },
                             var_vals=1).apply_to(out_ds))

            # Pressure on vertical levels of chem data (time, lev, ncells):
            # p = hyam + hybm * ps (hyam in Pa, no P0). Fallback: hydrostatic
            # reconstruction if the coefficients got lost during remapping.
            if 'hyam' in c_ds.variables and 'hybm' in c_ds.variables:
                ps_c = c_ds['ps'][time_index_lst, :].astype('f8')
                pres_c_arr = (
                    c_ds['hyam'][:].astype('f8')[np.newaxis, :, np.newaxis] +
                    c_ds['hybm'][:].astype('f8')[np.newaxis, :, np.newaxis] *
                    ps_c[:, np.newaxis, :])
            else:
                pres_c_arr = chem_pressure_from_altitude(c_ds, time_index_lst)
            (VariableCreator(var_args={
                'varname': 'pres',
                'datatype': 'f8',
                'dimensions': ('time', 'lev', HDIM)
            },
                             var_attrs={
                                 'long_name':
                                 'pressure on vertical levels of chem data',
                                 'units': 'Pa'
                             },
                             var_vals=pres_c_arr).apply_to(out_ds))

            # Pressure on vertical levels of meteo data (time, lev_m, ncells),
            # computed with the surface pressure of the chemistry data
            ps = c_ds['ps'][time_index_lst, :].astype('f8')
            hyam = m_ds['hyam'][:].astype('f8')
            hybm = m_ds['hybm'][:].astype('f8')
            pres_m_arr = (
                hyam[np.newaxis, :, np.newaxis] +
                hybm[np.newaxis, :, np.newaxis] * ps[:, np.newaxis, :])
            (VariableCreator(var_args={
                'varname': 'pres_m',
                'datatype': 'f8',
                'dimensions': ('time', 'lev_m', HDIM)
            },
                             var_attrs={
                                 'long_name':
                                 'pressure on vertical levels of meteo data',
                                 'units': 'Pa'
                             },
                             var_vals=pres_m_arr).apply_to(out_ds))

            # -- Interpolate fields
            for var_name in spec:
                arr = hybrid_pressure_interpolation(c_ds, out_ds, var_name,
                                                    time_index_lst)
                (VariableCreator(var_args={
                    'varname': var_name,
                    'datatype': 'f8',
                    'dimensions': ('time', 'lev_m', HDIM)
                },
                                 var_attrs=c_ds[var_name].__dict__,
                                 var_vals=arr).apply_to(out_ds))

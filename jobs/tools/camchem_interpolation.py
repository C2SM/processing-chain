#!/usr/bin/env python3

import numpy as np
import glob
import os
from netCDF4 import Dataset
from os.path import join
import multiprocessing
from .nc_operations import VariableCreator, VariableCopier, DimensionCopier
from datetime import datetime, timedelta, timezone

##########################################################################################
## - Vertical interpolation of chemistry fields from CAM-Chem (CESM2.1) output.
## - Extraction of one NetCDF file per time step.
## - Interpolation of chemistry fields with respect to time variable.
##
## Based on 'mozart2int2m.py', with additions by Louise Aubet and Corina Keller (Empa).
##########################################################################################


def time_intpl(file_path, prefix):
    """ Interpolation of chemistry fields with respect to time variable.
    """
    files = sorted([f for f in os.listdir(file_path) if f.startswith(prefix)])

    for i in range(len(files) - 1):

        in_filename1 = join(file_path, files[i])
        in_filename2 = join(file_path, files[i + 1])

        date_str1 = files[i].split('_')[1].split('.')[0]
        date_str2 = files[i + 1].split('_')[1].split('.')[0]

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
                    'lev_s', 'lev_m', 'lon', 'lat', 'hyam', 'hybm', 'hyai',
                    'hybi', 'date'
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

                # -- Interpolate surface pressure
                mean_pres = np.mean([ds1['PS'][:], ds2['PS'][:]], axis=0)
                (VariableCreator(var_args={
                    'varname': 'PS',
                    'datatype': 'f8',
                    'dimensions': ('time', 'lat', 'lon')
                },
                                 var_attrs={
                                     'long_name': 'surface pressure',
                                     'units': 'Pa'
                                 },
                                 var_vals=mean_pres).apply_to(out_ds))

                # -- Interpolate chemical fields
                var_to_copy.extend(('time', 'datesec', 'PS'))
                for var_name, var in ds1.variables.items():
                    if var_name not in var_to_copy:
                        mean_field = np.mean(
                            [ds1[var_name][:], ds2[var_name][:]], axis=0)
                        (VariableCreator(var_args={
                            'varname':
                            var_name,
                            'datatype':
                            'f8',
                            'dimensions': ('time', 'lev_m', 'lat', 'lon')
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


##################################


def extract_timeslice(in_file, out_file_template, spec_intpl, ref_date):
    """
    - Extract a time slice from the input NetCDF file.
    - Adjust longitude from [0, 360) to (-180, 180].

    Parameters:
    - in_file (str): Path to the input NetCDF file containing data for each time step.
    - out_file_template (str): Template for the output NetCDF file name with strftime placeholder.
    - spec_intpl (list): List with species to be copied from in_file.

    Returns:
    None
    """

    # -- Create list of DimensionCopier objects for copying dimensions.
    dimpairs = [('lat', 'lat'), ('lon', 'lon'), ('lev_m', 'lev_m'),
                ('lev_s', 'lev_s'), ('nhym', 'nhym'), ('nhyi', 'nhyi')]

    dim_copier = [
        DimensionCopier(src_name, dst_name) for src_name, dst_name in dimpairs
    ]

    # -- Create list of VariableCopier objects for copying variables for each time step.
    varpairs_to_copy = [(spc, spc) for spc in spec_intpl]

    for time_index in range(Dataset(in_file).dimensions['time'].size):

        # -- Specify the slices of the variable values to be copied.
        sliced_variable_options = {
            'var_args': {
                'dimensions': ('time', 'lev_m', 'lat', 'lon')
            },
            'var_val_indices': np.s_[time_index, :]
        }

        var_opts = [{
            'src_names': src,
            'dst_name': dst,
            **sliced_variable_options
        } for src, dst in varpairs_to_copy]

        var_opts += [{
            'src_names': 'lat',
            'dst_name': 'lat'
        }, {
            'src_names': 'lon',
            'dst_name': 'lon'
        }, {
            'src_names': 'PS',
            'dst_name': 'PS',
            'var_args': {
                'dimensions': ('time', 'lat', 'lon')
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
            days_since_ref = float(ds.variables['time'][time_index])
            timestamp = date_from_days_since_ref(days_since_ref, ref_date)
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
                                     ('seconds to complete', 'current date'),
                                     'units':
                                     's'
                                 },
                                 var_vals=sec_from_midnight).apply_to(out_ds))

                # -- Write specified dimensions and variables to output.
                for op in dim_copier + var_copier:
                    op.apply_to(ds, out_ds)

                # -- Transform longitude: [0,360) -> [-180,180).
                for i in range(len(out_ds['lon'])):
                    if out_ds['lon'][i] > 180:
                        out_ds['lon'][i] -= 360


##################################


def log_intpl_timeslice(pres_m, pres, var_data):
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
    """Perform vertical interpolation of 'var_name'. 
    The interpolation is linear with the log pressure.
    """
    # Pressure on vertical levels of meteo data
    pres_m = out_ds['pres_m'][:]
    # Pressure on vertical levels of chemistry data
    pres = out_ds['pres'][:]
    var_data = in_ds[var_name][np.s_[time_indices, :]]

    var_arr = np.zeros(
        (out_ds.dimensions['time'].size, out_ds.dimensions['lev_m'].size,
         out_ds.dimensions['lat'].size, out_ds.dimensions['lon'].size))

    for dt in range(len(out_ds['time'][:])):
        var_arr[dt, :, :, :] = log_intpl_timeslice(pres_m[dt, :, :, :],
                                                   pres[dt, :, :, :],
                                                   var_data[dt, :, :, :])

    return var_arr


def vert_intpl(chem_filename, meteo_filename, out_filename, spec, start_chunk,
               end_chunk, ref_date):
    """Perform vertical interpolation of atmospheric chemical fields using meteorological data.
    
    Parameters:
    - chem_filename (str): Path to the netCDF file containing atmospheric chemical data.
    - meteo_filename (str): Path to the netCDF file containing meteorological data.
    - out_filename (str): Path to the output netCDF file for interpolated data.
    - species (list): List of chemical species names to be interpolated.
    - start_chunk (datetime object): Start of simulation time.
    - end_chunk (datetime object): End of simulation time.

    Returns:
    None

    """

    with Dataset(chem_filename, 'r') as c_ds, Dataset(meteo_filename,
                                                      'r') as m_ds:

        # -- Extract indices for times between simulation start and end
        time_index_lst = []

        for time_index in range(c_ds.dimensions['time'].size):
            days_since_ref_now = float(c_ds.variables['time'][time_index])
            timestamp_now = date_from_days_since_ref(days_since_ref_now,
                                                     ref_date)
            timestamp_now = timestamp_now.replace(tzinfo=timezone.utc)

            if start_chunk <= timestamp_now <= end_chunk:
                if time_index not in time_index_lst:
                    time_index_lst.append(time_index)

            if time_index != c_ds.dimensions['time'].size - 1:
                days_since_ref_next = float(c_ds.variables['time'][time_index +
                                                                   1])
                timestamp_next = date_from_days_since_ref(
                    days_since_ref_next, ref_date)
                timestamp_next = timestamp_next.replace(tzinfo=timezone.utc)

                # Time difference of two time steps
                time_difference = (timestamp_next -
                                   timestamp_now).total_seconds() / 3600.0

                # Date between two time steps
                delta_t = time_difference / 2.
                timestamp_interp = timestamp_now + timedelta(hours=delta_t)

                if start_chunk <= timestamp_interp <= end_chunk:
                    if time_index not in time_index_lst:
                        time_index_lst.append(time_index)
                    time_index_lst.append(time_index + 1)

        with Dataset(out_filename, 'w') as out_ds:
            # -- Set global attributes
            out_ds.setncatts(c_ds.__dict__)

            # -- Create new dimensions
            out_ds.createDimension("lev_m", len(m_ds['lev'][:]))
            out_ds.createDimension("lev_s", 1)
            out_ds.createDimension('time', size=len(time_index_lst))

            # -- Copy dimensions
            dimpairs_c = [('lev', 'lev'), ('lat', 'lat'), ('lon', 'lon'),
                          ('ilev', 'ilev'), ('nbnd', 'nbnd')]
            dim_copier_c = [
                DimensionCopier(src_name, dst_name)
                for src_name, dst_name in dimpairs_c
            ]
            dimpairs_m = [('nhym', 'nhym'), ('nhyi', 'nhyi')]
            dim_copier_m = [
                DimensionCopier(src_name, dst_name)
                for src_name, dst_name in dimpairs_m
            ]

            # -- Copy variables
            copy_from_chem = ['lat', 'lon']
            dict_c = [{
                'src_names': var,
                'dst_name': var
            } for var in copy_from_chem]

            dict_c += [{
                'src_names': 'PS',
                'dst_name': 'PS',
                'var_args': {
                    'dimensions': ('time', 'lat', 'lon')
                },
                'var_val_indices': np.s_[time_index_lst, :]
            }]

            sliced_variable_options = {
                'var_args': {
                    'dimensions': ('time')
                },
                'var_val_indices': np.s_[time_index_lst]
            }
            dict_c += [{
                'src_names': src,
                'dst_name': dst,
                **sliced_variable_options
            } for src, dst in [('time',
                                'time'), ('date',
                                          'date'), ('datesec', 'datesec')]]

            var_copier_c = [VariableCopier(**kwargs) for kwargs in dict_c]
            for op in dim_copier_c + var_copier_c:
                op.apply_to(c_ds, out_ds)

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

            # Pressure array for chem data
            pres_c_arr = np.zeros(
                (len(time_index_lst), c_ds.dimensions['lev'].size,
                 c_ds.dimensions['lat'].size, c_ds.dimensions['lon'].size))

            for ilev in range(len(c_ds['ilev'][:]) - 1):
                pres_c_col = c_ds['hyam'][ilev] * c_ds['P0'] + c_ds['hybm'][
                    ilev] * c_ds['PS'][np.s_[time_index_lst, :]]
                pres_c_arr[:, ilev, :, :] = pres_c_col

            # Pressure on vertical levels of chem data
            (VariableCreator(var_args={
                'varname': 'pres',
                'datatype': 'f8',
                'dimensions': ('time', 'lev', 'lat', 'lon')
            },
                             var_attrs={
                                 'long_name':
                                 'pressure on vertical levels of chem data',
                                 'units': 'Pa'
                             },
                             var_vals=pres_c_arr).apply_to(out_ds))

            # Pressure array for meteo data
            pres_m_arr = np.zeros(
                (len(time_index_lst), m_ds.dimensions['lev'].size,
                 c_ds.dimensions['lat'].size, c_ds.dimensions['lon'].size))

            for ilev in range(len(m_ds['lev'][:])):
                pres_m_col = m_ds['hyam'][ilev] + m_ds['hybm'][ilev] * c_ds[
                    'PS'][np.s_[time_index_lst, :]]
                pres_m_arr[:, ilev, :, :] = pres_m_col

            # Pressure on vertical levels of meteo data
            (VariableCreator(var_args={
                'varname': 'pres_m',
                'datatype': 'f8',
                'dimensions': ('time', 'lev_m', 'lat', 'lon')
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
                    'dimensions': ('time', 'lev_m', 'lat', 'lon')
                },
                                 var_attrs=c_ds[var_name].__dict__,
                                 var_vals=arr).apply_to(out_ds))

#!/usr/bin/env python3

import os
from os.path import join
from netCDF4 import Dataset
from .nc_operations import VariableCreator, VariableCopier, DimensionCopier
from .. import tools


def process_ic(file_path_in, file_path_out, ic_datetime, aero_dict, prefix):
    """
    Generate initial condition (IC) NetCDF files from CAM-Chem output.

    This function copies chemical and aerosol fields for a specified datetime 
    from an input CAM-Chem NetCDF file. Aerosol fields can be split into 
    different modes. 
    No unit transformation is performed.

    Parameters
    ----------
    file_path_in : str
        Path to the input directory containing CAM-Chem NetCDF files.
    file_path_out : str
        Path to the output directory where IC files will be saved.
    ic_datetime : datetime.datetime
        Datetime for the IC to process.
    aero_dict : dict
        Dictionary specifying how aerosol variables should be split into modes 
        and their corresponding fractions. 
        Format: `{aerosol_var_name: {mode_name: split_fraction, ...}, ...}`.
    prefix : str
        Filename prefix for input and output NetCDF files.

    Returns
    -------
    None

    Notes
    -----
    - Input files are expected to follow the naming convention: `{prefix}%Y%m%d%H.nc`.
    - Output files are saved as `{prefix}%Y%m%d%H_ic.nc`.
    - Variables not present in `aero_dict` are copied directly.
    - Aerosol variables are split according to the fractions defined in `aero_dict`.
    """

    in_template = prefix + '%Y%m%d%H.nc'
    outname_template = prefix + '%Y%m%d%H_ic.nc'
    in_filename = ic_datetime.strftime(in_template)
    out_filename = ic_datetime.strftime(outname_template)
    in_path = join(file_path_in, in_filename)

    with Dataset(in_path, 'r') as ds:
        # -- Create output filename
        out_path = join(file_path_out, out_filename)
        # -- Write output netCDF file containing IC
        with Dataset(out_path, 'w') as out_ds:
            # -- Copy global attributes
            out_ds.setncatts(ds.__dict__)

            # -- Copy dimensions
            dimpairs = [(dim_name, dim_name)
                        for dim_name, dim in ds.dimensions.items()]
            dim_copier = [
                DimensionCopier(src_name, dst_name)
                for src_name, dst_name in dimpairs
            ]

            # -- Copy variables
            var_to_copy = [
                var_name for var_name, v in ds.variables.items()
                if var_name not in aero_dict
            ]
            dict_var = [{
                'src_names': var,
                'dst_name': var
            } for var in var_to_copy]
            var_copier = [VariableCopier(**kwargs) for kwargs in dict_var]

            for op in dim_copier + var_copier:
                op.apply_to(ds, out_ds)

            # -- Split aerosols into modes
            for var_name, modes_dict in aero_dict.items():
                for mode_name, split in modes_dict.items():
                    var_mmr = ds[var_name][:] * split
                    (VariableCreator(var_args={
                        'varname':
                        mode_name,
                        'datatype':
                        'f8',
                        'dimensions': ('time', 'lev_m', 'lat', 'lon')
                    },
                                     var_attrs={
                                         'long_name':
                                         mode_name + ' mass mixing ratio',
                                         'units': 'kg/kg'
                                     },
                                     var_vals=var_mmr).apply_to(out_ds))


########################################


def process_lbc(file_path_in, file_path_out, start_time, end_time, inc, prefix,
                suffix, nameformat, var_mw_dict, aero_dict):
    """
    Generate lateral boundary condition (LBC) NetCDF files from CAM-Chem output.

    This function processes CAM-Chem NetCDF files and produces LBC files with 
    appropriate unit transformations. 
    Chemical fields are converted from [mol/mol] to [kg/kg], while aerosol fields 
    are split into modes and converted from [kg/kg] to [μg/kg]. 

    Parameters
    ----------
    file_path_in : str
        Path to the input directory containing CAM-Chem NetCDF files.
    file_path_out : str
        Path to the output directory where LBC files will be saved.
    start_time : datetime.datetime
        Start datetime for the LBC processing range.
    end_time : datetime.datetime
        End datetime for the LBC processing range.
    inc : int
        Time increment in hours between consecutive files to process.
    prefix : str
        Filename prefix used for both input and output files.
    suffix : str
        Filename suffix used for both input and output files (e.g., '.nc').
    nameformat : str
        Datetime format string used in the filenames (e.g., '%Y%m%d%H').
    var_mw_dict : dict
        Dictionary mapping chemical variable names to their molecular weights 
        for unit conversion. Format: `{var_name: molecular_weight, ...}`.
    aero_dict : dict
        Dictionary specifying how aerosol variables should be split into modes 
        and their corresponding fractions. 
        Format: `{aerosol_var_name: {mode_name: split_fraction, ...}, ...}`.

    Returns
    -------
    None

    Notes
    -----
    - Input files are iterated over using specified increments between `start_time` 
      and `end_time`.
    - Chemical fields are converted from [mol/mol] to [kg/kg] using the molecular 
      weight of air (28.96 g/mol) and the variable-specific molecular weight.
    - Aerosol fields are split into modes and converted from [kg/kg] to [μg/kg].
    - Essential variables like coordinates and pressure are copied directly
    - Output files are saved with `_lbc` appended to the original filename.
    """

    for time in tools.iter_hours(start_time, end_time, inc):

        # -- Extract datetime information from the filename
        in_file = os.path.join(file_path_in,
                               time.strftime(prefix + nameformat + suffix))
        with Dataset(in_file, 'r') as ds:
            # -- Create output filename
            out_filename = time.strftime(prefix + nameformat + '_lbc' + suffix)
            out_file = join(file_path_out, out_filename)

            # -- Write output netCDF file containing LBC
            with Dataset(out_file, 'w') as out_ds:
                ## Global attributes
                out_ds.setncatts(ds.__dict__)

                # -- Copy dimensions
                dimpairs = [(dim_name, dim_name)
                            for dim_name, dim in ds.dimensions.items()]
                dim_copier = [
                    DimensionCopier(src_name, dst_name)
                    for src_name, dst_name in dimpairs
                ]

                # -- Copy variables
                var_to_copy = [
                    'lev_s', 'lev_m', 'lon', 'lat', 'hyam', 'hybm', 'hyai',
                    'hybi', 'time', 'date', 'datesec', 'PS'
                ]
                dict_var = [{
                    'src_names': var,
                    'dst_name': var
                } for var in var_to_copy]
                var_copier = [VariableCopier(**kwargs) for kwargs in dict_var]

                for op in dim_copier + var_copier:
                    op.apply_to(ds, out_ds)

                # -- Transform units [mol/mol] --> [kg/kg] of chemical fields
                # Molar weight air
                mw_air = 28.96
                for var_name, mw in var_mw_dict.items():
                    # [mol/mol] --> [kg/kg]
                    var_mmr = ds[var_name][:] * mw / mw_air
                    (VariableCreator(var_args={
                        'varname':
                        var_name,
                        'datatype':
                        'f8',
                        'dimensions': ('time', 'lev_m', 'lat', 'lon')
                    },
                                     var_attrs={
                                         'long_name':
                                         var_name + ' mass mixing ratio',
                                         'units': 'kg/kg'
                                     },
                                     var_vals=var_mmr).apply_to(out_ds))

                # -- Split aerosols into modes and transform units [kg/kg] --> [ug/kg]
                kg_to_mug = 1.e9
                for var_name, modes_dict in aero_dict.items():
                    for mode_name, split in modes_dict.items():
                        # [kg/kg] --> [ug/kg]
                        var_mmr = ds[var_name][:] * split * kg_to_mug
                        (VariableCreator(var_args={
                            'varname':
                            mode_name,
                            'datatype':
                            'f8',
                            'dimensions': ('time', 'lev_m', 'lat', 'lon')
                        },
                                         var_attrs={
                                             'long_name':
                                             mode_name + ' mass mixing ratio',
                                             'units': 'mug/kg'
                                         },
                                         var_vals=var_mmr).apply_to(out_ds))


########################################

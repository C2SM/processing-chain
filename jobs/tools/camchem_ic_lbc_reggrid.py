#!/usr/bin/env python3

import os
from os.path import join
from netCDF4 import Dataset
from .nc_operations import VariableCreator, VariableCopier, DimensionCopier
from datetime import datetime
from .. import tools

#######################################################################
## Script for roducing IC and LBC on regular grid from CAM-Chem output. 
##
## Author: Corina Keller (Empa).
#######################################################################

def process_ic(file_path_in, file_path_out, ic_datetime, aero_dict, prefix):
    """
    Copy chem and aerosol fields for specified dates (no unit transformation).
    Splitting of aerosols into modes.
    """
    #ic_datetime = datetime.strptime(ic_date, '%Y-%m-%d %H:%M:%S')

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
            dimpairs = [(dim_name, dim_name) for dim_name, dim in ds.dimensions.items()]
            dim_copier = [
                DimensionCopier(src_name, dst_name) for src_name, dst_name in dimpairs
            ]

            # -- Copy variables 
            var_to_copy = [
                var_name for var_name, v in ds.variables.items() if var_name not in aero_dict
                ]
            dict_var = [{'src_names': var,
                        'dst_name': var}
                        for var in var_to_copy]
            var_copier = [VariableCopier(**kwargs) for kwargs in dict_var]
                    
            for op in dim_copier + var_copier:
                op.apply_to(ds,out_ds)
                    
            # -- Split aerosols into modes
            for var_name, modes_dict in aero_dict.items():
                for mode_name, split in modes_dict.items():
                    var_mmr = ds[var_name][:] * split
                    (VariableCreator(
                    var_args={
                        'varname': mode_name,
                        'datatype': 'f8',
                        'dimensions': ('time', 'lev_m', 'lat', 'lon')
                    },
                    var_attrs={
                        'long_name' : mode_name + ' mass mixing ratio',
                        'units': 'kg/kg'
                    },
                    var_vals=var_mmr).apply_to(out_ds))
                
########################################

def process_lbc(file_path_in, file_path_out, 
                start_time, end_time, inc, 
                prefix, suffix, nameformat, 
                var_mw_dict, aero_dict):
    """ 
    Unit transformation [mol/mol] --> [kg/kg] for chem fields.
    Splitting, renaming and unit transformation [kg/kg] --> [ug/kg] for aerosols.
    """
    # Files in the input folder
    #files = [f for f in os.listdir(file_path) if f.startswith('camchem_') and f.endswith('.nc')]
    #for file in files:
    for time in tools.iter_hours(start_time, end_time,
                                     inc):
    
        # -- Extract datetime information from the filename
        #datetime_str = file.split('_')[1].split('.')[0]
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
                dimpairs = [(dim_name, dim_name) for dim_name, dim in ds.dimensions.items()]
                dim_copier = [
                    DimensionCopier(src_name, dst_name) for src_name, dst_name in dimpairs
                ]

                # -- Copy variables
                var_to_copy = ['lev_s', 'lev_m', 'lon', 'lat', 'hyam', 'hybm', 'hyai', 'hybi', 'time', 'date', 'datesec', 'PS']
                dict_var = [{'src_names': var,
                            'dst_name': var}
                            for var in var_to_copy]
                var_copier = [VariableCopier(**kwargs) for kwargs in dict_var]

                for op in dim_copier + var_copier: 
                    op.apply_to(ds,out_ds)

                # -- Transform units [mol/mol] --> [kg/kg] of chemical fields
                # Molar weight air 
                mw_air = 28.96
                for var_name, mw in var_mw_dict.items():
                    # [mol/mol] --> [kg/kg]
                    var_mmr = ds[var_name][:] * mw/mw_air
                    (VariableCreator(
                        var_args={
                            'varname': var_name,
                            'datatype': 'f8',
                            'dimensions': ('time', 'lev_m', 'lat', 'lon')
                        },
                        var_attrs={
                            'long_name' : var_name + ' mass mixing ratio',
                            'units': 'kg/kg'
                        },
                        var_vals=var_mmr).apply_to(out_ds))
                
                # -- Split aerosols into modes and transform units [kg/kg] --> [ug/kg]
                kg_to_mug = 1.e9
                for var_name, modes_dict in aero_dict.items():
                    for mode_name, split in modes_dict.items():
                        # [kg/kg] --> [ug/kg]
                        var_mmr = ds[var_name][:] * split * kg_to_mug
                        (VariableCreator(
                        var_args={
                            'varname': mode_name,
                            'datatype': 'f8',
                            'dimensions': ('time', 'lev_m', 'lat', 'lon')
                        },
                        var_attrs={
                            'long_name' : mode_name + ' mass mixing ratio',
                            'units': 'mug/kg'
                        },
                        var_vals=var_mmr).apply_to(out_ds))

########################################

import numpy as np

from datetime import time, date, datetime
from netCDF4 import Dataset
from os.path import join

from .nc_operations import VariableCreator, VariableCopier, DimensionCopier


def date_from_days_since_0(days_since_00000101):
    """Convert a float counting days since days since 0000-01-01 00:00:00
    to a datetime object.
    """

    def add_years(d, years):
        """Return a date that's `years` years after the date (or datetime)
        object `d`. Return the same calendar date (month and day) in the
        destination year, if it exists, otherwise use the following day
        (thus changing February 29 to March 1).
        """
        # from https://stackoverflow.com/a/15743908
        try:
            return d.replace(year=d.year + years)
        except ValueError:
            return d + (date(d.year + years, 1, 1) - date(d.year, 1, 1))

    full_days = int(days_since_00000101)
    # fromordinal starts from day 0
    timestamp_date = date.fromordinal(full_days + 1)
    # fromordinal starts at year 1
    timestamp_date = add_years(timestamp_date, -1)

    hours = int((days_since_00000101 - full_days) * 24)
    assert hours == (days_since_00000101 - full_days) * 24, (
        "Can't convert to time at not full hour.")  # too lazy
    timestamp_time = time(hour=hours)

    return datetime.combine(timestamp_date, timestamp_time)


def extract_data(in_path, time_index, dim_ops, var_ops, out_path_template):
    """Extract and transform indicated data from time_index to
    out_path_template.

    Create a new netcdf-file at out_path_template after replacing the
    placeholders with the date from the time_index in in_path.

    Create a 'time' dimension and variable of size 1.
    Add the ncattr calendar: 'proleptic_gregorian' to the time-variable.

    Copy and transform the dimensions specified in the dim_ops.

    Copy and transform the variables specified in the var_ops.

    Adjust longitude from [0, 360) to (-180, 180].
    """
    with Dataset(in_path) as inf:
        timestamp = date_from_days_since_0(inf.variables['time'][time_index])

        out_path = timestamp.strftime(out_path_template)

        with Dataset(out_path, 'w') as of:
            of.createDimension(dimname='time', size=1)

            # Create time variable
            (VariableCreator(
                var_args={
                    'varname': 'time',
                    'datatype': 'f8'
                },
                var_attrs={
                    'axis': 'T',
                    'calendar': 'proleptic_gregorian',
                    'long_name': 'simulation_time',
                    'standard_name': 'time',
                    'units':
                    timestamp.strftime('seconds since %Y-%m-%d %H:%M:%S')
                },
                var_vals=0).apply_to(of))

            # Create date variable
            (VariableCreator(var_args={
                'varname': 'date',
                'datatype': 'i4',
                'dimensions': ('time', )
            },
                             var_attrs={
                                 'long_name': ('current date as 8 digit'
                                               ' integer (YYYYMMDD)')
                             },
                             var_vals=int(
                                 timestamp.strftime('%Y%m%d'))).apply_to(of))

            # Create datesec variable
            sec_from_midnight = ((timestamp - timestamp.replace(
                hour=0, minute=0, second=0, microsecond=0)).total_seconds())
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
                             var_vals=sec_from_midnight).apply_to(of))

            # Transfer specified dimensions and variables
            for op in dim_ops + var_ops:
                op.apply_to(inf, of)

            # Transform longitude: [0,360) -> [-180,180)
            for i in range(len(of['lon'])):
                if of['lon'][i] > 180:
                    of['lon'][i] -= 360


def main(_, infile, outdir, params):
    """Extract each timeslice from infile into seperate file in outdir.

    Copies dimensions, renaming 'lev' -> 'level'.

    Copies, renames and combines variables (see source code for specifics).

    Transforms 'lon' to [-180, 180) range.

    Parameters
    ----------
    _
        Ignored
    infile : str
        Path to file to extract from
    outdir : str
        Path to the directory where the output files are written to
    params : dict
        Only entry: 'suffix'
        Processed files are stored as ``outdir/{suffix}_YYYYMMDDHH.nc``
    """
    outname_template = join(outdir, params['suffix'] + '_%Y%m%d%H.nc')

    dimpairs = [
        (
            'lev',  # name in src
            'level'),  # name in dst
        ('lat', 'lat'),
        ('lon', 'lon'),
        ('ilev', 'ilev')
    ]

    dim_copiers = [
        DimensionCopier(src_name, dst_name) for src_name, dst_name in dimpairs
    ]

    varpairs_to_copy = [
        (['CH3CHO_VMR_inst', 'GLYALD_VMR_inst'], 'ALD'),
        (
            'CO_VMR_inst',  # name in src, lists added toghether
            'CO'),  # name in dst
        ('CRESOL_VMR_inst', 'CSL'),
        ('C2H6_VMR_inst', 'ETH'),
        ('GLYOXAL_VMR_inst', 'GLY'),
        ('H2O2_VMR_inst', 'H2O2'),
        ('C3H8_VMR_inst', 'HC3'),
        ('HNO3_VMR_inst', 'HNO3'),
        ('BIGALK_VMR_inst', 'HC5'),
        ('CH2O_VMR_inst', 'HCHO'),
        ('HO2NO2_VMR_inst', 'HNO4'),
        ('HO2_VMR_inst', 'HO2'),
        ('ISOP_VMR_inst', 'ISO'),
        (['CH3COCH3_VMR_inst', 'HYAC_VMR_inst', 'MEK_VMR_inst'], 'KET'),
        (['MVK_VMR_inst', 'MACR_VMR_inst'], 'MACR'),
        ('CH3COCHO_VMR_inst', 'MGLY'),
        ('MPAN_VMR_inst', 'MPAN'),
        ('N2O5_VMR_inst', 'N2O5'),
        ('NH3_VMR_inst', 'NH3'),
        ('NO_VMR_inst', 'NO'),
        ('NO2_VMR_inst', 'NO2'),
        ('NO3_VMR_inst', 'NO3'),
        ('OH_VMR_inst', 'OH'),
        ('C2H4_VMR_inst', 'OL2'),
        ('ONIT_VMR_inst', 'ONIT'),
        ('CH3OOH_VMR_inst', 'OP1'),
        ('C2H5OOH_VMR_inst', 'OP2'),
        ('CH3COOH_VMR_inst', 'ORA2'),
        ('O3_VMR_inst', 'OZONE'),
        ('CH3COOOH_VMR_inst', 'PAA'),
        ('PAN_VMR_inst', 'PAN'),
        ('SO2_VMR_inst', 'SO2'),
        ('T', 'T'),
        ('TOLUENE_VMR_inst', 'TOL'),
        ('DUST1', 'VSOILA'),
        ('DUST2', 'VSOILB'),
        ('DUST3', 'VSOILC')
    ]

    varpairs_to_copy_dimchange = [('NH4_VMR_inst', 'VNH4Jm'),
                                  (['OC1_VMR_inst',
                                    'OC2_VMR_inst'], 'VORG1Jm'),
                                  ('SO4_VMR_inst', 'VSO4Jm'),
                                  (['CB1_VMR_inst', 'CB2_VMR_inst'], 'VSOOTJ')]

    for time_index in range(Dataset(infile).dimensions['time'].size):
        # Have to give dimensions explicitly because 'lev' changes to 'level'
        # Have to give var_val_indices explicitly because we only copy one
        # time index
        spacial_variable_options = {
            'var_args': {
                'dimensions': ('time', 'level', 'lat', 'lon')
            },
            'var_val_indices': np.s_[time_index, :]
        }

        # 3D variables that simply get copied
        var_opts = [{
            'src_names': src,
            'dst_name': dst,
            **spacial_variable_options
        } for src, dst in varpairs_to_copy]

        # 3D variables with dimchange to mol/mol
        var_opts += [{
            'src_names': src,
            'dst_name': dst,
            'var_attrs': {
                'units': 'mol/mol'
            },
            **spacial_variable_options
        } for src, dst in varpairs_to_copy_dimchange]

        # Others
        var_opts += [{
            'src_names': 'lat',
            'dst_name': 'lat'
        }, {
            'src_names': 'lev',
            'dst_name': 'level',
            'var_args': {
                'dimensions': ('level', )
            }
        }, {
            'src_names': 'lon',
            'dst_name': 'lon'
        }, {
            'src_names': 'P0',
            'dst_name': 'P0'
        }, {
            'src_names': 'PS',
            'dst_name': 'PSURF',
            'var_args': {
                'dimensions': ('time', 'lat', 'lon')
            },
            'var_val_indices': np.s_[time_index, :]
        }, {
            'src_names': 'hyam',
            'dst_name': 'hyam',
            'var_args': {
                'dimensions': ('level', )
            }
        }, {
            'src_names': 'hybm',
            'dst_name': 'hybm',
            'var_args': {
                'dimensions': ('level', )
            }
        }, {
            'src_names': 'ilev',
            'dst_name': 'ilev'
        }]

        var_copiers = [VariableCopier(**kwargs) for kwargs in var_opts]

        extract_data(infile, time_index, dim_copiers, var_copiers,
                     outname_template)

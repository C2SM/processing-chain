#!/usr/bin/env python
# -*- coding: utf-8 -*-

import csv
import sys
import os
from .. import tools

STR2INT = {
    'ytype_adv':      {'off': 0, 'on': 1},
    'ytype_diff':     {'off': 0, 'on': 1},
    'ytype_turbmix':  {'off': 0, '1D': 1, '3D': 2},
    'ytype_passconv': {'off': 0, 'on': 1},
    'ytype_ini':      {'zero': 0, 'file': 1, 'user': 2},
    'ytype_lbc':      {'zero': 0, 'file': 1, 'constant': 2, 'zero_gradient': 3, 'user': 4},
    'ytype_bbc':      {'zero_flux': 0, 'zero_value': 1, 'surface_value': 2},
    'ytype_relax':    {'off': 0, 'full': 1, 'inflow': 2},
    'ytype_damp':     {'off': 0, 'on': 1},
    'ytype_clip':     {'off': 0, 'on': 1}
}

# Read initial conditions from file (= 1) in case of spinup simluation
STR2INT_recycling = STR2INT.copy()
STR2INT_recycling["ytype_ini"] = {'zero': 1, 'file': 1, 'user': 2}

def group2text(group, recycling=False):

    lines = ['&TRACER']
    for key, value in group.items():

        if key == '' or value == '':
            continue

        if key in STR2INT:
            if recycling:
                value = STR2INT_recycling[key][value]
            else:
                value = STR2INT[key][value]
            key = 'i%s' % key[1:]

        if key[0] == 'y':
            value = "'%s'" % value

        if key == 'ycatl' or key == 'ytpl' or key == 'yvpl':
            value = value.replace('\'\'', '\'')

        lines.append('  %s = %s,' % (key, value))
    lines.append('/\n')

    return '\n'.join(lines)


def main(csv_filename, namelist_filename, cfg=None):
    """Convert a table (``.csv`` file) to namelist file (``INPUT_GHG``)
    read by **COSMO**
    
    Parameters
    ----------
    csv_filename : str
        Path to the source csv-file
    namelist_filename : str
        Path to the namelist file that will be created
    """

    # Check if online emissions ('oae') are used
    if hasattr(cfg, 'oae_dir'):
        oae = True
    else:
        oae = False

    # Check if online VPRM ('vprm') is used
    if hasattr(cfg, 'online_vprm_dir'):
        online_vprm = True
    else:
        online_vprm = False

    # Check if online CT ensembles is used
    if hasattr(cfg, 'octe_lambdas'):
        octe = True
    else:
        octe = False

    # Check if cut_area is used
    if hasattr(cfg, 'lon_cut') and hasattr(cfg, 'lat_cut'):
        cut = True
    else:
        cut = False

    with open(csv_filename, 'r') as csv_file:

        reader = csv.DictReader(csv_file, delimiter=',')
        reader = [r for r in reader if r[''] != '#']
        n_tracers = len(reader)

        with open(namelist_filename, 'w') as nml_file:
            ghgctl_vals = ['&GHGCTL',
                           '  in_tracers = %d,' % n_tracers
                           ]
            # Add input files for online emissions
            if oae:
                ghgctl_vals.extend(['  vertical_profile_nc = \'' \
                               + '../input/oae/vertical_profiles.nc' + '\',',
                               '  hour_of_day_nc = \'' \
                               + '../input/oae/hourofday.nc' + '\',',
                               '  day_of_week_nc = \'' \
                               + '../input/oae/dayofweek.nc' + '\',',
                               '  month_of_year_nc = \'' \
                               + '../input/oae/monthofyear.nc' + '\',',
                               '  gridded_emissions_nc = \'' \
                               + '../input/oae/emissions.nc' + '\','
                               ])
            # Add input files for online VPRM
            if online_vprm:
                ghgctl_vals.extend(['  modis_reflectances_nc = \'' \
                                    + '../input/vprm/modis.nc' + '\',',
                                      '  veg_class_frac_nc = \'' \
                                    + '../input/vprm/vegetation.nc' + '\','
                                   ])
            # Add input files for OCTE
            if octe:
                ghgctl_vals.extend([("  octe_maps_nc = "
                                     "'../input/octe/maps.nc',"),
                                    ("  octe_lambdas_nc = "
                                     "'../input/octe/lambdas.nc',")])
            if cfg.target.subtarget is tools.Subtarget.SPINUP:
                if cfg.first_one:
                    ghgctl_vals.insert(len(ghgctl_vals),
                                       '  tracer_start = 0.,')
                else:
                    ghgctl_vals.insert(len(ghgctl_vals),
                                       '  tracer_start = %d.,' % cfg.spinup)

            nml_file.write('\n'.join(ghgctl_vals))
            nml_file.write('\n/\n')

            vprmctl_vals = ['&VPRMCTL',
                           '  yvprm_table = \'europe_2018\','
                           ]
            if cut:
                vprmctl_vals.extend(['  lcut_area = .TRUE.,'])
                vprmctl_vals.extend([
                                     '  lon_cut_start = %f,' % cfg.lon_cut[0],
                                     '  lon_cut_end = %f,' % cfg.lon_cut[1],
                                     '  lat_cut_start = %f,' % cfg.lat_cut[0],
                                     '  lat_cut_end = %f,' % cfg.lat_cut[1]
                                    ])
            else:
                vprmctl_vals.extend(['  lcut_area = .FALSE.,'])
            nml_file.write('\n'.join(vprmctl_vals))
            nml_file.write('\n/\n')

            for group in reader:
                if cfg.target.subtarget is tools.Subtarget.SPINUP \
                and not cfg.first_one:
                    nml_file.write(group2text(group, recycling=True))
                else:
                    nml_file.write(group2text(group))



if __name__ == '__main__':
    input_filename = sys.argv[1]   # csv file with tracers
    output_filename = sys.argv[2]  # filename (INPUT_TRCR) read by COSMO
    main(input_filename, output_filename)

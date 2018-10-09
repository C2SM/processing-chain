#!/usr/bin/env python
# -*- coding: utf-8 -*-

import csv
import sys

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


def group2text(group):

    lines = ['&TRACER']
    for key, value in group.items():

        if key == '':
            continue

        if key in STR2INT:
            value = STR2INT[key][value]
            key = 'i%s' % key[1:]

        if key[0] == 'y':
            value = "'%s'" % value

        lines.append('  %s = %s,' % (key, value))
    lines.append('/\n')

    return '\n'.join(lines)




def main(csv_filename, namelist_filename):
    """Convert a table (``.csv`` file) to namelist file (``INPUT_BGC``)
    read by **COSMO**
    
    Parameters
    ----------
    csv_filename : str
        Path to the source csv-file
    namelist_filename : str
        Path to the namelist file that will be created
    """
    with open(csv_filename, 'r') as csv_file:

        reader = csv.DictReader(csv_file, delimiter=',')
        reader = [r for r in reader if r[''] != '#']
        n_tracers = len(reader)

        with open(namelist_filename, 'w') as nml_file:

            nml_file.write(
                '\n'.join(['&BGCCTL', '  lc_cycle = .TRUE.,', '  in_tracers = %d,' % n_tracers, '/\n'])
            )
            for group in reader:
                nml_file.write(group2text(group))



if __name__ == '__main__':
    input_filename = sys.argv[1]   # csv file with tracers
    output_filename = sys.argv[2]  # filename (INPUT_TRCR) read by COSMO
    main(input_filename, output_filename)

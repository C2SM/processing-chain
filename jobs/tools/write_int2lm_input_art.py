#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import argparse
import csv
import sys


def art_control(nart_ds, nart_trcr, hstart=0):
    lines = [
        "&ARTCONTROL",
        "  nart_ds = %d," % nart_ds,       # number of dataset groups
        "  nart_trcr = %d," % nart_trcr,   # number of tracers groups
        "  hstart = %d," % hstart,
        "/"
    ]
    return '\n'.join(lines)


def make_group(keys, values, group):
    lines = ['&%s' % group]
    for key, value in zip(keys, values):
        if value:
            if key == 'yvarlist':
                value = ','.join('"%s"' % v.strip() for v in value.split(","))
                lines.append('  %s = %s,' % (key, value))
            elif key[0] == 'y':
                lines.append('  %s = "%s",' % (key, value))
            else:
                lines.append('  %s = %s,' % (key, value))
    lines.append('/\n')
    return '\n'.join(lines)


def read_file(filename):
    with open(filename, 'r') as csv_file:
        lines = [line[1:] for line in csv.reader(csv_file) if not line[0].startswith('#')]

    return lines[0], lines[1:]


def main(trcr_filename, set_filename, nml_filename, hstart=0):
    """Write the INPUT_ART namelist file for int2lm from ``.csv`` files
    
    Parameters
    ----------
    trcr_filename : str
        csv file with tracer definitions
    set_filename : str
        csv file with tracer datasets
    nml_filename : str
        output filename (INPUT_ART)
    hstart : int
        meteorology spin up in hours
    """
    # art tracers
    if trcr_filename is None:
        tracer_values = []
        n_tracers = 0
    else:
        tracer_keys, tracer_values = read_file(trcr_filename)
        n_tracers = len(tracer_values)

    # art datasets
    if set_filename is None:
        set_values = []
        n_sets = 0
    else:
        set_keys, set_values = read_file(set_filename)
        n_sets = len(set_values)

    # write namelist file
    with open(nml_filename, 'w') as nml:

        nml.write(art_control(n_sets, n_tracers, hstart))
        nml.write('\n')

        for values in tracer_values:
            text = make_group(tracer_keys, values, 'ARTTRACER')
            nml.write(text)
            nml.write('\n')

        for values in set_values:
            text = make_group(set_keys, values, 'ARTDATASET')
            nml.write(text)
            nml.write('\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Write INPUT_ART for int2lm.')

    parser.add_argument('-t', dest='trcr_filename', default=None,
                        type=str, help='filename of csv file with tracers')
    parser.add_argument('-d', dest='datasets_filename', default=None,
                        type=str, help='filename of csv file with datasets')
    parser.add_argument('-o', dest='nml_filename', default='INPUT_ART',
                        type=str, help='output filename (INPUT_ART)')
    parser.add_argument('-m', dest='meteo_spinup', default=0,
                        type=float, help='meteorology spin up')

    args = parser.parse_args()
    main(args.trcr_filename, args.datasets_filename,
         args.nml_filename, args.meteo_spinup)

